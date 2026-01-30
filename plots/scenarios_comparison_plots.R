# Scenariusz 1: porównanie modeli bez epidemii

model_data_2 <- model_data_2 %>%
  mutate(okres = factor(okres, 
                        levels = c("przed epidemią", "w trakcie epidemii","po epidemii")))

melted_data_with_preds <- melted_data_with_preds %>%
  mutate(okres = factor(okres, 
                        levels = c("przed epidemią", "w trakcie epidemii","po epidemii")))

forecast_without_cov <- predict(no_covid_prophet, data.frame(ds = model_data$time))

auto_sarima_and_prophet_df <- model_data_2 %>%
  mutate("auto_sarima" = sarima_models_fit$`bez epidemii`[, 1],
         "auto_max_sarima" = sarima_models_fit$`bez epidemii`[, 2],
         "prophet" = forecast_without_cov$yhat,
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
            aes(x = as.Date(time), y = values), linewidth = 0.7) +
  geom_line(data = melted_data_with_preds %>% 
              filter(okres == "po epidemii" & model == paste(expression(SARIMA(0,1,1)(0,1,1)[12]), " (ekspercka)")),
            aes(x = as.Date(time), y = preds, color = model)) +
  geom_line(data = melted_auto_sarima_and_prophet_df %>% filter(okres == "po epidemii"),
            aes(x = as.Date(time), y = preds, col = model)) +
  scale_color_discrete(labels = label_parse()) +
  xlab("Data") +
  theme_light() +
  ylab("Liczba pasażerów") +
  ggtitle("Porównanie prognoz postpandemicznych modeli uczonych na okresie przed pandemią")+
  theme(plot.title = element_text(hjust = 0.5, size = 15), 
        legend.position = "bottom", legend.text = element_text(size = 12))



ggplot() +
  geom_line(data = model_data_2, 
            aes(x = as.Date(time), y = values), linewidth = 0.7) +
  geom_line(data = melted_data_with_preds%>% 
              filter(model == paste(expression(SARIMA(0,1,1)(0,1,1)[12]), " (ekspercka)")),
            aes(x = as.Date(time), y = preds, color = model)) +
  geom_line(data = melted_auto_sarima_and_prophet_df,
            aes(x = as.Date(time), y = preds, col = model)) +
  scale_color_discrete(labels = label_parse()) +
  #facet_wrap(.~okres, scale = "free", nrow = 3) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  theme_light() +
  ggtitle("Porównanie modeli uczonych na okresie przed pandemią") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.position = "bottom", legend.text = element_text(size = 12))


# Scenariusz 2: porównanie modeli epidemią

forecast_with_cov_auto <- predict(prophet_models$`z epidemią`, data.frame(ds = model_data$time))
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
  theme_light() +
  ggtitle("Porównanie modeli uczonych z okresem pandemicznym")+
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.position = "bottom", legend.text = element_text(size = 12))


ggplot() +
  geom_line(data = model_data_2, 
            aes(x = as.Date(time), y = values), linewidth = 1) +
  geom_line(data = melted_data_with_preds_aft %>% 
              filter(model == paste(expression(SARIMA(2,1,1)(0,1,1)[12]))),
            aes(x = as.Date(time), y = preds, color = model)) +
  geom_line(data = melted_auto_sarima_and_prophet_df,
            aes(x = as.Date(time), y = preds, col = model)) +
  scale_color_discrete(labels = label_parse()) +
  #facet_wrap(.~okres_2, scale = "free", nrow = 3) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  theme_light() +
  ggtitle("Porównanie modeli uczonych z okresem pandemicznym") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.position = "bottom", legend.text = element_text(size = 12))


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
  theme_minimal() +
  ggtitle("Porównanie modeli uczonych z imputowanym okresem pandemicznym")+
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.position = "bottom", legend.text = element_text(size = 12))

ggplot() +
  geom_line(data = model_data_2, 
            aes(x = as.Date(time), y = values)) +
  geom_line(data = melted_data_imp_preds %>% 
              filter(model == "SARIMA(1,1,1)(0,1,1)[12]"),
            aes(x = as.Date(time), y = preds, color = model)) +
  geom_line(data = melted_auto_sarima_and_prophet_df,
            aes(x = as.Date(time), y = preds, col = model)) +
  scale_color_discrete(labels = label_parse()) +
  #facet_wrap(.~okres_2, scale = "free", nrow = 3) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  theme_light() +
  ggtitle("Porównanie modeli uczonych z imputacją okresu pandemii") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.position = "bottom", legend.text = element_text(size = 12))


# Porównanie scenariuszy


future <- make_future_dataframe(prophet_model, 
                                periods = model_data %>% filter(okres == "po epidemii") %>% nrow() + 12 * 5, 
                                freq = "month")

forecast_best_model_2 <- predict(prophet_model, future)

split_date <- as.POSIXct("2022-05-01")
future_split_date <- as.POSIXct("2025-08-01")

bg_data <- data.frame(
  okres = c("Zbiór treningowy", "Zbiór testowy", "Przyszłość"),
  xmin = c(min(forecast_best_model_2$ds), split_date, future_split_date),
  xmax = c(split_date, max(forecast_best_model_2 $ds), as.POSIXct("2030-08-01"))
)

#. Prophet skalibrowany 

ggplot() +
  geom_rect(data = bg_data, 
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = okres), 
            alpha = 0.1) +
  # Przedział ufności
  geom_ribbon(data = subset(forecast_best_model_2, ds <= future_split_date & ds >= split_date),
              aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), 
              fill = "#0072B2", alpha = 0.2) +
  geom_line(data = model_data, 
            aes(x = as.POSIXct(time), y = values, color = "Prawdziwe dane"), 
            linewidth = 0.7) +
  geom_line(data = forecast_best_model_2, 
            aes(x = ds, y = yhat, color = "Predykcja"), 
            linewidth = 1.1) +
  scale_color_manual(name = "Dane", 
                     values = c("Prawdziwe dane" = "grey40", "Predykcja" = "#0072B2")) +
  scale_fill_manual(name = "Okresy", 
                    values = c("Zbiór treningowy" = "red", 
                               "Zbiór testowy" = "steelblue", 
                               "Przyszłość" = "darkgreen")) +
  geom_vline(xintercept = c(split_date, future_split_date), 
             linetype = "dashed", color = "grey30", alpha = 0.7) +
  
  theme_minimal() +
  labs(title = "Porównanie wartości rzeczywistych z prognozą modelu Prophet",
       subtitle = "Z podziałem na trening, test i prognozę przyszłą",
       x = "Data", y = "Liczba pasażerów") +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
  
# Prophet (automatyczny)
future <- make_future_dataframe(prophet_models$imputacja, 
                                periods = model_data %>% filter(okres == "po epidemii") %>% nrow() + 12 * 5, 
                                freq = "month")

forecast_best_model <- predict(prophet_models$imputacja, future)

ggplot() +
  geom_rect(data = bg_data, 
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = okres), 
            alpha = 0.1) +
  # Przedział ufności
  geom_ribbon(data = subset(forecast_best_model, ds <= future_split_date & ds >= split_date),
              aes(x = ds, ymin = exp(yhat_lower), ymax = exp(yhat_upper)),
              fill = "#0072B2", alpha = 0.2) +
  geom_line(data = model_data, 
            aes(x = as.POSIXct(time), y = values, color = "Prawdziwe dane"), 
            linewidth = 0.7) +
  geom_line(data = forecast_best_model, 
            aes(x = ds, y = exp(yhat), color = "Predykcja"), 
            linewidth = 1.1) +
  scale_color_manual(name = "Dane", 
                     values = c("Prawdziwe dane" = "grey40", "Predykcja" = "#0072B2")) +
  scale_fill_manual(name = "Okresy", 
                    values = c("Zbiór treningowy" = "red", 
                               "Zbiór testowy" = "steelblue", 
                               "Przyszłość" = "darkgreen")) +
  geom_vline(xintercept = c(split_date, future_split_date), 
             linetype = "dashed", color = "grey30", alpha = 0.7) +
  
  theme_minimal() +
  labs(title = "Porównanie wartości rzeczywistych z prognozą modelu Prophet",
       subtitle = "Z podziałem na trening, test i prognozę przyszłą",
       x = "Data", y = "Liczba pasażerów") +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 


# Porównanie Prophetów:

ggplot() +
  geom_rect(data = bg_data, 
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = okres), 
            alpha = 0.1) +
  geom_line(data = model_data, 
            aes(x = as.POSIXct(time), y = values, color = "Prawdziwe dane"), 
            linewidth = 0.7) +
  geom_line(data = forecast_best_model, 
            aes(x = ds, y = exp(yhat), color = "Predykcja (imputacja)"), 
            linewidth = 1.1) +
  
  geom_line(data = forecast_best_model_2, 
            aes(x = ds, y = yhat, col = "Predykcja (skalibrowany)"), 
            linewidth = 1.1) +
  scale_color_manual(name = "Dane", 
                     values = c("Prawdziwe dane" = "grey40", 
                                "Predykcja (imputacja)" = "#0072B2",
                                "Predykcja (skalibrowany)" = "darkorange")) +
  scale_fill_manual(name = "Okresy", 
                    values = c("Zbiór treningowy" = "red", 
                               "Zbiór testowy" = "steelblue", 
                               "Przyszłość" = "darkgreen")) +
  geom_vline(xintercept = c(split_date, future_split_date), 
             linetype = "dashed", color = "grey30", alpha = 0.7) +
  
  theme_minimal() +
  labs(title = "Porównanie wartości rzeczywistych z prognozą modeli Prophet",
       subtitle = "Model Prophet (imputacja) vs Model Prophet (z pandemią, skalibrowany)",
       x = "Data", y = "Liczba pasażerów") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) 
