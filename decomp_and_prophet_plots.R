library(fpp3)

source("set_up.R")

# Dekompozycja 
ts_data <- ts(model_data$values, frequency = 12, start = c(2004, 1))
decomposed <- decompose(ts_data, type = "multiplicative")

autoplot(decomposed, range.bars = FALSE) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(
    title = "Dekompozycja liczby pasażerów transportu lotniczego w Polsce",
    subtitle = "Model multiplikatywny",
    x = "Rok",
    y = "Liczba pasażerów"
  )

# Prophet z epidemią -  Asia
model_data_2 <- model_data %>%
  mutate("zbiór" = ifelse(okres == "po epidemii", "zbiór testowy", "zbiór treningowy"))

split_date <- as.POSIXct("2022-05-01")

bg_data <- data.frame(
  okres = c("Zbiór treningowy", "Zbiór testowy"),
  xmin = c(min(forecast$ds), split_date),
  xmax = c(split_date, max(forecast$ds))
)

forecast <- predict(no_covid_prophet, data.frame(ds = model_data$time))

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


# Prophet z imputacją - Asia 
model_data_2 <- model_data_2 %>%
  mutate("imp_values" = ifelse(okres != "po epidemii", exp(train_imp_covid), values))

forecast <- predict(prophet_models$`imputacja`, data.frame(ds = model_data$time))

ggplot() +
  geom_rect(data = bg_data, 
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = okres), 
            alpha = 0.1) +
  
  geom_ribbon(data = subset(forecast, ds >= split_date),
              aes(x = ds, ymin = exp(yhat_lower), ymax = exp(yhat_upper)), 
              fill = "#0072B2", alpha = 0.2) +
  
  geom_line(data = model_data_2, 
            aes(x = as.POSIXct(time), y = imp_values, color = "Prawdziwe dane"), 
            linewidth = 0.7) +
  
  geom_line(data = forecast, 
            aes(x = ds, y = exp(yhat), color = "Predykcja"), 
            linewidth = 1.1) +
  
  scale_color_manual(name = "Dane", 
                     values = c("Prawdziwe dane" = "grey40", "Predykcja" = "#0072B2")) +
  
  scale_fill_manual(name = "Okresy", 
                    values = c("Zbiór treningowy" = "red", "Zbiór testowy" = "steelblue")) +
  
  geom_vline(xintercept = split_date, linetype = "dashed", color = "grey30") +
  theme_minimal() +
  labs(title = "Porównanie wartości rzeczywistych z prognozą modelu Prophet z imputacją COVID-19",
       x = "Data", y = "Liczba pasażerów") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
