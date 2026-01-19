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

# Funkcje

rmse <- function(real, pred) sqrt(mean((as.numeric(real) - as.numeric(pred)) ^ 2))

perform_analysis <- function(ts_data, nonses_order, ses_order){
  nonses_name <- paste0("(", paste(nonses_order, collapse = ","), ")")
  ses_name <- paste0("(", paste(nonses_order, collapse = ","), ")")
  mod_name <-paste0("SARIMA", nonses_name, ses_name, "_12")
  
  mod <- ts_data %>% Arima(order= nonses_order, seasonal= ses_order)
  
  mod %>% residuals() %>% 
    ggtsdisplay(main = paste("Analiza residuów modelu", mod_name)) +  # ocena residuów
    
    cat("\n--- Kryteria informacyjne ---\n")
  print(mod %>% glance() %>% select(AIC, BIC)) # ocena kryterii 
  
  cat("\n--- Testy istotności współczynników ---\n")
  print(mod %>% coeftest()) # istotnność
}

