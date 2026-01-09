
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(TSstudio)
library(forecast)
library(prophet)

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

ggplot(clean_data) +
  geom_line(aes(x = time, y = values)) +
  ggtitle("Przewozy pasażerskie w transporcie lotniczym według kraju sprawozdającego - Polska")

start_date <- c(2004, 1)
log_data_ts <- ts(log(clean_data$values), start = start_date, deltat = 1/12) #box-cox

stl_decomp <- stl(log_data_ts, s.window = "per")
autoplot(stl_decomp)

# sprowadzenie do stacjonarnego - różnicowanie
ndiffs(log_data_ts)
nsdiffs(log_data_ts) # tutaj 0, ale sezonowość jest widoczna

stat_ts <- diff(diff(log_data_ts, lag = 12))

plot(stat_ts, type = "l")

acf(stat_ts, lag.max = 50)
pacf(stat_ts, lag.max = 50)

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

bound_obs <- which(model_data$time %in% c(covid_start, covid_end))
bound_obs <- c(bound_obs, bound_obs[1] - 1, bound_obs[2] + 1)

ggplot(model_data, aes(x = time, y = log_values)) +
  geom_line(data = cbind(model_data[sort(bound_obs),],
                         group = rep(1:2, each = 2)),
            aes(group = group), linetype = "dashed") +
  geom_line(aes(color = okres)) +
  ggtitle("Przewozy pasażerskie w transporcie lotniczym według kraju sprawozdającego - Polska")

train_data <- model_data %>%
  filter(okres != "po epidemii")

train_with_covid <- train_data$log_values

train_no_covid_data <- train_data %>%
  mutate(log_values = case_when(
    okres == "w trakcie epidemii" ~ NA,
    .default = log_values
  ))

train_no_covid <- na.omit(train_no_covid_data$log_values)

# imputacja - stl ???
covid_obs_num <- length(train_with_covid) - length(train_no_covid)

train_no_covid_ts <- ts(train_no_covid, start = start_date, , deltat = 1/12)
no_covid_stl <- stl(train_no_covid_ts, s.window = "per")

covid_imp_obs <- forecast(no_covid_stl, h = covid_obs_num)$mean

train_imp_covid <- c(train_no_covid, covid_imp_obs)

plot(exp(train_imp_covid), type = "l")

# modelowanie
train_sets <- list(train_with_covid, train_imp_covid, train_no_covid)
train_sets_ts <- setNames(lapply(train_sets, function(train){
  ts(train, start = start_date, deltat = 1/12) 
}), c("z epidemią", "imputacja", "bez epidemii"))

models <- mapply(function(train_ts, train){
    list(
      arima = auto.arima(train_ts),
      max_arima = auto.arima(train_ts, max.p = 2, max.q = 2),
      prophet = prophet(data.frame(
        ds = train_data$time[1:length(train)],
        y = train
      ))
    )
}, train_sets_ts, train_sets, SIMPLIFY = FALSE)

models_train_fit <- lapply(models, function(models_lst){
  c(lapply(models_lst[1:2], function(model) model$fitted),
    list(predict(models_lst[[3]])$yhat))
})

models_train_fit_df <- data.frame(
  fitted = exp(unlist(models_train_fit)),
  model = c(rep(rep(names(models[[1]]), each = length(train_with_covid)), 2),
            rep(names(models[[1]]), each = length(train_no_covid))),
  rodzaj_zbioru = c(
    rep(names(models)[1:2],
        each = length(train_with_covid) * length(models[[1]])),
    rep(names(models)[3], length(train_no_covid)  * length(models[[1]]))
  ),
  passengers = exp(unlist(rep(train_sets, each = length(models[[1]])))),
  time = c(rep(train_data$time, 2 * length(models[[1]])),
           rep(na.omit(train_no_covid_data)$time, length(models[[1]])))
)

ggplot(models_train_fit_df, aes(x = time, y = fitted, color = model)) +
  geom_line(aes(y = passengers), color = "black") +
  geom_line() +
  facet_wrap(~ rodzaj_zbioru, nrow = 3)

# test
test_data <- model_data %>%
  filter(okres == "po epidemii")

models_test_fit <- lapply(models, function(models_lst){
  setNames(c(
    lapply(models_lst[1:2],
           function(model) predict(model, test_data$time)$pred[1:nrow(test_data)]),
    list(predict(models_lst[[3]], data.frame(ds = test_data$time))$yhat)
  ), names(models_lst))
})

models_test_fit_df <- data.frame(
  fitted = exp(unlist(models_test_fit)),
  model = rep(names(models[[1]]), each = nrow(test_data)),
  rodzaj_zbioru = rep(names(models),
                      each = nrow(test_data) * length(models[[1]])),
  passengers = test_data$values,
  time = test_data$time
)

ggplot(models_test_fit_df, aes(x = time, y = fitted, color = model)) +
  geom_line(aes(y = passengers), color = "black") +
  geom_line() +
  facet_wrap(~ rodzaj_zbioru, nrow = 3)

