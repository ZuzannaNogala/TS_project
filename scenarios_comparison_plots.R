# Scenariusz 1: porównanie modeli bez epidemii

model_data_2 <- model_data_2 %>%
  mutate(okres = factor(okres, 
                        levels = c("przed epidemią", "w trakcie epidemii","po epidemii")))

melted_data_with_preds <- melted_data_with_preds %>%
  mutate(okres = factor(okres, 
                        levels = c("przed epidemią", "w trakcie epidemii","po epidemii")))

forecast_without_cov <- predict(prophet_models$`bez epidemii`, data.frame(ds = model_data$time))

auto_sarima_and_prophet_df <- model_data_2 %>%
  mutate("auto_sarima" = sarima_models_fit$`bez epidemii`[, 1],
         "auto_max_sarima" = sarima_models_fit$`bez epidemii`[, 2],
         "prophet" = exp(forecast_without_cov$yhat),
         okres = factor(okres, levels = c("przed epidemią", "w trakcie epidemii","po epidemii")))

melted_auto_sarima_and_prophet_df  <- auto_sarima_and_prophet_df %>%
  pivot_longer(
    cols = c(ends_with("sarima"), "prophet"), 
    names_to = "model",          
    values_to = "preds"         
  ) %>%
  select(time, values, okres, model, preds)

ggplot() +
  geom_line(data = model_data %>% filter(okres == "po epidemii"), 
            aes(x = as.Date(time), y = values)) +
  geom_line(data = melted_data_with_preds %>% 
              filter(okres == "po epidemii" & model == paste(expression(SARIMA(0,1,1)(0,1,1)[12]), " (ekspercka)")),
            aes(x = as.Date(time), y = preds, color = model)) +
  geom_line(data = melted_auto_sarima_and_prophet_df %>% filter(okres == "po epidemii"),
            aes(x = as.Date(time), y = preds, col = model)) +
  scale_color_discrete(labels = label_parse()) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  ggtitle("Porównanie modeli uczonych na okresie przed pandemią")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))



ggplot() +
  geom_line(data = model_data_2, 
            aes(x = as.Date(time), y = values)) +
  geom_line(data = melted_data_with_preds%>% 
              filter(model == paste(expression(SARIMA(0,1,1)(0,1,1)[12]), " (ekspercka)")),
            aes(x = as.Date(time), y = preds, color = model)) +
  geom_line(data = melted_auto_sarima_and_prophet_df,
            aes(x = as.Date(time), y = preds, col = model)) +
  scale_color_discrete(labels = label_parse()) +
  facet_wrap(.~okres, scale = "free", nrow = 3) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  ggtitle("Porównanie modeli uczonych na okresie przed pandemią") +
  theme(plot.title = element_text(hjust = 0.5, size = 12))


# Scenariusz 2: porównanie modeli epidemią

forecast_with_cov_auto <- predict(no_covid_prophet, data.frame(ds = model_data$time))
forecast_with_cov <- predict(prophet_model, data.frame(ds = model_data$time))

auto_sarima_and_prophet_df <- model_data_2 %>%
  mutate("auto_sarima" = sarima_models_fit$`z epidemią`[, 1],
         "auto_max_sarima" = sarima_models_fit$`z epidemią`[, 2],
         "prophet_auto" = exp(forecast_with_cov_auto$yhat), 
         "prophet" = forecast_with_cov$yhat,
         okres_2 = ifelse(okres %in% c("przed epidemią", "w trakcie epidemii"), 
                          "Przed i w trakcie epidemii", 
                          as.character(okres)))

auto_sarima_and_prophet_df <- auto_sarima_and_prophet_df %>% 
  mutate(okres_2 = factor(okres_2, 
                          levels = c("Przed i w trakcie epidemii", "po epidemii"),
                          labels = c("przed i w trakcie epidemii (zbiór treningowy)", "po epidemii (zbiór testowy)")))

model_data_2 <- model_data_2%>% mutate(okres_2 = ifelse(okres %in% c("przed epidemią", "w trakcie epidemii"), 
                                                         "Przed i w trakcie epidemii", 
                                                         as.character(okres))) %>% 
  mutate(okres_2 = factor(okres_2, 
                          levels = c("Przed i w trakcie epidemii", "po epidemii"),
                          labels = c("przed i w trakcie epidemii (zbiór treningowy)", "po epidemii (zbiór testowy)")))

melted_auto_sarima_and_prophet_df  <-auto_sarima_and_prophet_df %>%
  pivot_longer(
    cols = c(ends_with("sarima"), "prophet", "prophet_auto"), 
    names_to = "model",          
    values_to = "preds"         
  ) %>%
  select(time, values, okres, okres_2, model, preds)

ggplot() +
  geom_line(data = model_data %>% filter(okres == "po epidemii"), 
            aes(x = as.Date(time), y = values)) +
  geom_line(data = melted_data_with_preds_aft %>% 
              filter(okres == "po epidemii" & model == paste(expression(SARIMA(2,1,1)(0,1,1)[12]))),
            aes(x = as.Date(time), y = preds, color = model)) +
  geom_line(data = melted_auto_sarima_and_prophet_df %>% filter(okres == "po epidemii"),
            aes(x = as.Date(time), y = preds, col = model)) +
  scale_color_discrete(labels = label_parse()) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  ggtitle("Porównanie modeli uczonych z okresem pandemicznym")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))


ggplot() +
  geom_line(data = model_data_2, 
            aes(x = as.Date(time), y = values), linewidth = 1) +
  geom_line(data = melted_data_with_preds_aft %>% 
              filter(model == paste(expression(SARIMA(2,1,1)(0,1,1)[12]))),
            aes(x = as.Date(time), y = preds, color = model)) +
  geom_line(data = melted_auto_sarima_and_prophet_df,
            aes(x = as.Date(time), y = preds, col = model)) +
  scale_color_discrete(labels = label_parse()) +
  facet_wrap(.~okres_2, scale = "free", nrow = 3) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  ggtitle("Porównanie modeli uczonych z okresem pandemicznym") +
  theme(plot.title = element_text(hjust = 0.5, size = 12))


# Scenariusz 3: sarima z imputacją

forecast_imp <- predict(prophet_models$imputacja, data.frame(ds = model_data$time))

auto_sarima_and_prophet_df <- model_data %>%
  mutate("auto_sarima" = sarima_models_fit$imputacja[, 1],
         "auto_max_sarima" = sarima_models_fit$imputacja[, 2],
         "prophet" = exp(forecast_imp$yhat), 
         okres_2 = ifelse(okres != "przed epidemią", 
                          ifelse(okres == "po epidemii", "po epidemii (zbiór testowy)", NA),
                          "przed epidemią (zbiór treningowy)"))

model_data_2 <- model_data_2 %>% mutate(okres_2 = ifelse(okres != "przed epidemią", 
                                                         ifelse(okres == "po epidemii", "po epidemii (zbiór testowy)", NA),
                                                         "przed epidemią (zbiór treningowy)"))  %>%
  mutate(okres_2 = factor(okres_2, 
                          levels = c("przed epidemią (zbiór treningowy)", "po epidemii (zbiór testowy)")))

melted_auto_sarima_and_prophet_df  <- auto_sarima_and_prophet_df %>%
  pivot_longer(
    cols = c(ends_with("sarima"), "prophet"), 
    names_to = "model",          
    values_to = "preds"         
  ) %>%
  select(time, values, okres, okres_2, model, preds)

melted_auto_sarima_and_prophet_df  <- melted_auto_sarima_and_prophet_df  %>%
  mutate(okres_2 = factor(okres_2, 
                          levels = c("przed epidemią (zbiór treningowy)", "po epidemii (zbiór testowy)")))


ggplot() +
  geom_line(data = model_data_2 %>% filter(okres == "po epidemii"), 
            aes(x = as.Date(time), y = values)) +
  geom_line(data = melted_data_imp_preds %>% 
              filter(okres == "po epidemii" & model == "SARIMA(1,1,1)(0,1,1)[12]"),
            aes(x = as.Date(time), y = preds, color = model)) +
  geom_line(data = melted_auto_sarima_and_prophet_df %>% filter(okres == "po epidemii"),
            aes(x = as.Date(time), y = preds, col = model)) +
  scale_color_discrete(labels = label_parse()) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  ggtitle("Porównanie modeli uczonych z imputowanym okresem pandemicznym")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))



ggplot() +
  geom_line(data = model_data_2 %>% filter(!is.na(okres_2)), 
            aes(x = as.Date(time), y = values)) +
  geom_line(data = melted_data_imp_preds %>% 
              filter(model == "SARIMA(1,1,1)(0,1,1)[12]" & !is.na(okres_2)),
            aes(x = as.Date(time), y = preds, color = model)) +
  geom_line(data = melted_auto_sarima_and_prophet_df%>% filter(!is.na(okres_2)),
            aes(x = as.Date(time), y = preds, col = model)) +
  scale_color_discrete(labels = label_parse()) +
  facet_wrap(.~okres_2, scale = "free", nrow = 3) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  ggtitle("Porównanie modeli SARIMA uczonych z imputacją okresu pandemii") +
  theme(plot.title = element_text(hjust = 0.5, size = 12))

  
