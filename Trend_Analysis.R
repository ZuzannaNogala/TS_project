## Model auto.arima z komponeta sezonowa

AUTO_SARIMA <- auto.arima(bef_cov_set, seasonal = TRUE) # (4,1,2)(2,1,1) - bardzo skomplikowany model
AUTO_SARIMA %>% summary()
# AIC=-523.42   AICc=-522.12   BIC=-491.43 - gorsze niz wybrany 
AUTO_SARIMA %>% coeftest() # prawie nic nie istotne

covid_preds_auto <- predict(AUTO_SARIMA, n.ahead = model_data %>%
                              filter(time >= covid_start & time <= covid_end) %>%
                              nrow())$pred


# mse / rmse na zbiorze treningowym
mean((fitted(AUTO_SARIMA) - bef_cov_set)^2)
sqrt(mean((exp(fitted(AUTO_SARIMA)) - exp(bef_cov_set))^2))

#mse/rmse na zbiorze covidowym
mean((as.numeric(covid_preds_auto) - as.numeric(model_data %>%
                                                  filter(time >= covid_start & time <= covid_end) %>%
                                                  select(log_values) %>%
                                                  unlist() %>%
                                                  ts(start = c(2020, 3), frequency = 12)))^2)

sqrt(mean((exp(as.numeric(covid_preds_auto)) - exp(as.numeric(model_data %>%
                                                                filter(time >= covid_start & time <= covid_end) %>%
                                                                select(log_values) %>%
                                                                unlist() %>%
                                                                ts(start = c(2020, 3), frequency = 12))))^2))
# po imputacji auto 
cov_imputed_auto_set <- model_data %>%
  filter(time <= covid_end) %>%
  mutate(sarima_auto_log_imp = c(bef_cov_set, covid_preds_auto)) %>%
  select(sarima_auto_log_imp) %>%
  unlist() %>%
  ts(start = c(2004,1), frequency = 12)


AUTO_SARIMA_full <- auto.arima(cov_imputed_auto_set, seasonal = TRUE)  # (4,1,2)(2,1,1) - bardzo skomplikowany model
AUTO_SARIMA_full %>% summary() 
# AIC=-635.18   AICc=-634.06   BIC=-601.8
AUTO_SARIMA_full %>% coeftest() # prawie nic nie istotne

preds_after_covid_auto <- predict(AUTO_SARIMA_full, n.ahead = model_data %>%
                                    filter(time > covid_end) %>%
                                    nrow())$pred

model_data_with_preds <- model_data_with_preds %>%
  mutate(sarima_auto_log_preds = c(fitted(AUTO_SARIMA_full), preds_after_covid_auto),
         sarima_auto_preds = round(c(exp(fitted(AUTO_SARIMA_full)), exp(preds_after_covid_auto)), 0))

# Imputacja danych covidowych (log values)
ggplot(model_data_with_preds %>% filter(okres != "po epidemii")) +
  geom_line(aes(x = time, y = log_values)) +
  geom_line(aes(x = time, y = sarima_auto_log_preds), col = "blue")

# Imputacja danych covidowych (normalne values)
ggplot(model_data_with_preds %>% filter(okres != "po epidemii")) +
  geom_line(aes(x = time, y = values)) +
  geom_line(aes(x = time, y = sarima_auto_preds), col = "blue")

# Co po okresie covidowym, gdy zastosowaliśmy imputację danych ? (log)

ggplot(model_data_with_preds) +
  geom_line(aes(x = time, y = log_values)) +
  geom_line(aes(x = time, y = sarima_auto_log_preds), col = "blue")

# Co po okresie covidowym, gdy zastosowaliśmy imputację danych ? (normalne)
ggplot(model_data_with_preds) +
  geom_line(aes(x = time, y = values)) +
  geom_line(aes(x = time, y = sarima_auto_preds), col = "blue")












ggplot(model_data_with_preds) +
  geom_line(aes(x = time, y = values)) +
  geom_line(aes(x = time, y = sarima_auto_preds), col = "blue") +
  geom_line(aes(x = time, y = sarima_preds), col = "red")


ggplot(model_data_with_preds) +
  geom_line(aes(x = time, y = log_values)) +
  geom_line(aes(x = time, y = sarima_auto_log_preds), col = "blue") +
  geom_line(aes(x = time, y = sarima_log_preds), col = "red")


ggplot(model_data_with_preds %>% filter(okres == "w trakcie epidemii")) +
  geom_line(aes(x = time, y = log_values)) +
  geom_line(aes(x = time, y = sarima_auto_log_preds), col = "blue") +
  geom_line(aes(x = time, y = sarima_log_preds), col = "red")

ggplot(model_data_with_preds %>% filter(okres == "w trakcie epidemii")) +
  geom_line(aes(x = time, y = values)) +
  geom_line(aes(x = time, y = sarima_auto_preds), col = "blue") +
  geom_line(aes(x = time, y = sarima_preds), col = "red")



mod <- rbind.data.frame(model_data_with_preds %>% 
                          filter(okres == "przed epidemią") %>% 
                          select(time, log_values),
                        model_data_with_preds %>% 
                          filter(okres == "w trakcie epidemii") %>% 
                          select(time, log_values = sarima_log_preds))


mod %>%
  filter(time <= covid_end) %>%
  select(log_values) %>%
  unlist() %>% 
  ts(frequency = 12) %>%
  Arima(order = c(0,1,1), seasonal = c(0,1,1)) %>%
  residuals() %>%
  ggtsdisplay()



