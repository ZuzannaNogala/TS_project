library(scales)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(TSstudio)
library(forecast)
library(prophet)
library(lmtest)
library(broom)

## Wczytanie danych:

dane_raw <-read.table("avia_paoc_monthly_tabular.tsv",
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
names(dane_raw)[1] <- "ident"

clean_data <- dane_raw %>%
  separate(ident, into = c("freq", "unit", "tra_meas", "tra_cov", "schedule", "geo"), sep = ",") %>%
  filter(geo == "PL", freq == "M") %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "data_tekst", 
               values_to = "wartosc_tekst") %>%
  mutate(
    values = as.numeric(str_extract(wartosc_tekst, "[0-9.]+")),
    time = paste0(str_replace_all(data_tekst, "[^0-9]", ""), "01"),
    time = as.Date(time, format = "%Y%m%d")
  ) %>%
  filter( # Polska
    tra_meas == "PAS_CRD", # Pasażerowie obsłużeni    
    tra_cov == "TOTAL", #  Całkowity przewóz pasażerów
    schedule == "TOT") %>%
  select(time, values) %>%
  na.omit()

# podział na train i test
covid_start <- "2020-03-01"
covid_end <- "2022-05-01"

model_data <- clean_data %>%
  mutate(
    okres = case_when(
      time < covid_start ~ "przed epidemią",
      time > covid_end ~ "po epidemii",
      .default = "w trakcie epidemii"
    ),
    log_values = log(values)
  )

# imputacja
train_data <- model_data %>%
  filter(okres != "po epidemii")

train_with_covid <- train_data$log_values

train_no_covid_data <- train_data %>%
  mutate(log_values = case_when(
    okres == "w trakcie epidemii" ~ NA,
    .default = log_values
  ))

train_no_covid <- na.omit(train_no_covid_data$log_values)

covid_obs_num <- length(train_with_covid) - length(train_no_covid)

train_no_covid_ts <- ts(train_no_covid, start = c(2004, 1), , deltat = 1/12)
no_covid_stl <- stl(train_no_covid_ts, s.window = "per")

covid_imp_obs <- forecast(no_covid_stl, h = covid_obs_num)$mean

train_imp_covid <- c(train_no_covid, covid_imp_obs)

# Funkcje

rmse <- function(real, pred) sqrt(mean((as.numeric(real) - as.numeric(pred)) ^ 2))

perform_analysis <- function(ts_data, nonses_order, ses_order){
  nonses_name <- paste0("(", paste(nonses_order, collapse = ","), ")")
  ses_name <- paste0("(", paste(nonses_order, collapse = ","), ")")
  mod_name <-paste0("SARIMA", nonses_name, ses_name, "_12")
  
  mod <- ts_data %>% Arima(order= nonses_order, seasonal= ses_order)
  
  mod %>% residuals() %>% 
    ggtsdisplay(main = paste("Analiza residuów modelu", mod_name)) # ocena residuów
    
    cat("\n--- Kryteria informacyjne ---\n")
  print(mod %>%{ data.frame(AIC = .$aic, BIC = .$bic) }) # ocena kryterii 
  
  cat("\n--- Testy istotności współczynników ---\n")
  print(mod %>% coeftest()) # istotnność
}

