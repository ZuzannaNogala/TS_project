source("set_up.R")


# Definicja tabeli z informacjami o covid

covid_period <- model_data %>%
  filter(okres == "w trakcie epidemii") %>%
  summarise(ds = min(time), 
            lower_window = 0, 
            upper_window = as.numeric(max(time) - min(time))) %>%
  mutate(holiday = "lockdown")


# Model Prophet
prophet_model <- prophet(df = model_data %>% 
                           filter(okres != "po epidemii") %>% select(ds = time, y=values),
                         changepoints = model_data %>% 
                           filter(okres == "w trakcie epidemii") %>% pull(time),
                         yearly.seasonality = TRUE,
                         seasonality.mode = "multiplicative",
                         holidays = covid_period)


# Prognozy
future <- make_future_dataframe(prophet_model, 
                                periods = model_data %>% filter(okres == "po epidemii") %>% nrow(), 
                                freq = "month")

forecast <- predict(prophet_model, future)

fitted <- forecast %>%
  filter(as.Date(ds) <= covid_end) %>%
  select(yhat) %>%
  ts(start = c(2004, 1), frequency = 12)

covid_preds_prophet <- forecast %>%
  filter(as.Date(ds) > covid_end) %>%
  select(yhat) %>%
  ts(start = c(2022, 6), frequency = 12)

# RMSE
rmse(fitted, model_data %>% filter(okres != "po epidemii") %>%pull(values))
rmse(covid_preds_prophet, model_data %>% filter(okres == "po epidemii") %>%pull(values))


# Wykres podstawowy
plot(prophet_model, forecast)+ 
  add_changepoints_to_plot(prophet_model) +
  geom_line(data = model_data, aes(x = as.POSIXct(time), y = values))


# Wykres zaawansowany
model_data_2 <- model_data %>%
  mutate("zbiór" = ifelse(okres == "po epidemii", "zbiór testowy", "zbiór treningowy"))

split_date <- as.POSIXct("2022-05-01")

bg_data <- data.frame(
  okres = c("Zbiór treningowy", "Zbiór testowy"),
  xmin = c(min(forecast$ds), split_date),
  xmax = c(split_date, max(forecast$ds))
)

ggplot() +
  geom_rect(data = bg_data, 
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = okres), 
            alpha = 0.1) +
  
  geom_ribbon(data = subset(forecast, ds >= split_date),
              aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), 
              fill = "#0072B2", alpha = 0.2) +
  
  geom_line(data = model_data_2, 
            aes(x = as.POSIXct(time), y = values, color = "Prawdziwe dane"), 
            linewidth = 0.7) +
  
  geom_line(data = forecast, 
            aes(x = ds, y = yhat, color = "Predykcja"), 
            linewidth = 1.1) +
  
  scale_color_manual(name = "Dane", 
                     values = c("Prawdziwe dane" = "grey40", "Predykcja" = "#0072B2")) +
  
  scale_fill_manual(name = "Okresy", 
                    values = c("Zbiór treningowy" = "red", "Zbiór testowy" = "steelblue")) +
  
  geom_vline(xintercept = split_date, linetype = "dashed", color = "grey30") +
  theme_minimal() +
  labs(title = "Porównanie wartości rzeczywistych z prognozą modelu Prophet uwzględniającą wpływ COVID-19",
       x = "Data", y = "Liczba pasażerów") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
