library(feasts)

model_data_2 <- model_data %>%
  mutate("imp_values" = ifelse(okres != "po epidemii", exp(train_imp_covid), values))

# Wykresy sarima bez covidu

ts_data <- model_data %>%
  filter(time < covid_start) %>%
  select(log_values) %>%
  unlist() %>%
  ts(frequency = 12)

ts_data %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>%  residuals() %>%
  ggtsdisplay(main = "Analiza residuów dla modelu SARIMA(0,1,1)(0,1,1)")

# parametry p = 0, q = 1, P = 0, Q = 1
# Kryteria: AIC=-532.58   AICc=-532.44   BIC=-522.98
# istotne - tak


ts_data %>%
  Arima(order=c(1,1,1), seasonal=c(1,1,1)) %>%
  residuals() %>% ggtsdisplay(main = "Analiza residuów dla modelu SARIMA(1,1,1)(1,1,1)")

# parametry p = 1, q = 1, P = 1, Q = 1
# Kryteria: AIC=-531.96   AICc=-531.62   BIC=-515.97
# istotne - nie (MA(1) i SAR(1))

ts_data%>%
  Arima(order=c(0,1,2), seasonal=c(0,1,1)) %>%
  residuals() %>% ggtsdisplay(main = "Analiza residuów dla modelu SARIMA(0,1,2)(0,1,1)")

# parametry p = 0, q = 1, P = 0, Q = 1
# Kryteria: AIC=-532.34   AICc=-532.11   BIC=-519.54
# istotne - nie (MA(2) nieistotne)


# ACF i PACF na jednym wykresie

get_resid_plots <- function(ts_obj, order, seasonal, model_name) {
  res <- Arima(ts_obj, order = order, seasonal = seasonal) %>% residuals()
  
  p_acf <- ggAcf(res) + 
    ggtitle(paste(model_name, "- ACF")) +
    theme_light() +
    theme(plot.title = element_text(size = 13, hjust = 0.5))
  
  p_pacf <- ggPacf(res) + 
    ggtitle(paste(model_name, "- PACF")) +
    theme_light() +
    theme(plot.title = element_text(size = 13, hjust = 0.5))
  
  return(p_acf + p_pacf)
}

m1_plots <- get_resid_plots(ts_data, c(0,1,1), c(0,1,1), "SARIMA(0,1,1)(0,1,1)")
m2_plots <- get_resid_plots(ts_data, c(1,1,1), c(1,1,1), "SARIMA(1,1,1)(1,1,1)")
m3_plots <- get_resid_plots(ts_data, c(0,1,2), c(1,1,1), "SARIMA(0,1,2)(1,1,1)")


(m1_plots) / (m2_plots) / (m3_plots) + 
  plot_annotation(title = "Porównanie residuów dla prostych modeli SARIMA",
                  theme = theme(
                    plot.title = element_text(
                      size = 16,
                      hjust = 0.5)))


# Scenariusz 1: porównanie modeli sarima bez epidemii

model_data_2 <- model_data_2 %>%
  mutate(okres = factor(okres, 
                        levels = c("przed epidemią", "w trakcie epidemii","po epidemii")))

melted_data_with_preds <- melted_data_with_preds %>%
  mutate(okres = factor(okres, 
                        levels = c("przed epidemią", "w trakcie epidemii","po epidemii")))

auto_sarima_df <- model_data_2 %>%
  mutate("auto_sarima" = sarima_models_fit$`bez epidemii`[, 1],
         "auto_max_sarima" = sarima_models_fit$`bez epidemii`[, 2],
         okres = factor(okres, levels = c("przed epidemią", "w trakcie epidemii","po epidemii")))

melted_auto_sarima_df  <- auto_sarima_df %>%
  pivot_longer(
    cols = ends_with("sarima"), 
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
  geom_line(data = melted_auto_sarima_df %>% filter(okres == "po epidemii"),
            aes(x = as.Date(time), y = preds, col = model)) +
  scale_color_discrete(labels = label_parse()) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  ggtitle("Porównanie modeli SARIMA uczonych na okresie przed pandemią")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))



ggplot() +
  geom_line(data = model_data_2, 
            aes(x = as.Date(time), y = values)) +
  geom_line(data = melted_data_with_preds%>% 
              filter(model == paste(expression(SARIMA(0,1,1)(0,1,1)[12]), " (ekspercka)")),
            aes(x = as.Date(time), y = preds, color = model)) +
  geom_line(data = melted_auto_sarima_df,
            aes(x = as.Date(time), y = preds, col = model)) +
  scale_color_discrete(labels = label_parse()) +
  facet_wrap(.~okres, scale = "free", nrow = 3) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  ggtitle("Porównanie modeli SARIMA uczonych na okresie przed pandemią") +
  theme(plot.title = element_text(hjust = 0.5, size = 12))



# Scenariusz 2: porównanie modeli sarima z epidemią

auto_sarima_df <- model_data_2 %>%
  mutate("auto_sarima" = sarima_models_fit$`z epidemią`[, 1],
         "auto_max_sarima" = sarima_models_fit$`z epidemią`[, 2],
         okres_2 = ifelse(okres %in% c("przed epidemią", "w trakcie epidemii"), 
                                 "Przed i w trakcie epidemii", 
                                 as.character(okres)))

auto_sarima_df <- auto_sarima_df %>% 
  mutate(okres_2 = factor(okres_2, 
                          levels = c("Przed i w trakcie epidemii", "po epidemii"),
                          labels = c("przed i w trakcie epidemii (zbiór treningowy)", "po epidemii (zbiór testowy)")))

model_data_2 <- model_data_2 %>% mutate(okres_2 = ifelse(okres %in% c("przed epidemią", "w trakcie epidemii"), 
                                "Przed i w trakcie epidemii", 
                                as.character(okres))) %>% 
  mutate(okres_2 = factor(okres_2, 
                          levels = c("Przed i w trakcie epidemii", "po epidemii"),
                          labels = c("przed i w trakcie epidemii (zbiór treningowy)", "po epidemii (zbiór testowy)")))

melted_auto_sarima_df  <- auto_sarima_df %>%
  pivot_longer(
    cols = ends_with("sarima"), 
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
  geom_line(data = melted_auto_sarima_df %>% filter(okres == "po epidemii"),
            aes(x = as.Date(time), y = preds, col = model)) +
  scale_color_discrete(labels = label_parse()) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  ggtitle("Porównanie modeli SARIMA uczonych z okresem pandemicznym")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))


ggplot() +
  geom_line(data = model_data_2, 
            aes(x = as.Date(time), y = values)) +
  geom_line(data = melted_data_with_preds_aft %>% 
              filter(model == paste(expression(SARIMA(2,1,1)(0,1,1)[12]))),
            aes(x = as.Date(time), y = preds, color = model)) +
  geom_line(data = melted_auto_sarima_df,
            aes(x = as.Date(time), y = preds, col = model)) +
  scale_color_discrete(labels = label_parse()) +
  facet_wrap(.~okres_2, scale = "free", nrow = 3) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  ggtitle("Porównanie modeli SARIMA uczonych z okresem pandemicznym") +
  theme(plot.title = element_text(hjust = 0.5, size = 12))


# Scenariusz 3: sarima z imputacją

auto_sarima_df <- model_data %>%
  mutate("auto_sarima" = sarima_models_fit$imputacja[, 1],
         "auto_max_sarima" = sarima_models_fit$imputacja[, 2],
         okres_2 = ifelse(okres != "przed epidemią", 
                          ifelse(okres == "po epidemii", "po epidemii (zbiór testowy)", NA),
                          "przed epidemią (zbiór treningowy)"))

model_data_2 <- model_data_2 %>% mutate(okres_2 = ifelse(okres != "przed epidemią", 
                                                       ifelse(okres == "po epidemii", "po epidemii (zbiór testowy)", NA),
                                                       "przed epidemią (zbiór treningowy)"))  %>%
  mutate(okres_2 = factor(okres_2, 
                          levels = c("przed epidemią (zbiór treningowy)", "po epidemii (zbiór testowy)")))

melted_auto_sarima_df  <- auto_sarima_df %>%
  pivot_longer(
    cols = ends_with("sarima"), 
    names_to = "model",          
    values_to = "preds"         
  ) %>%
  select(time, values, okres, okres_2, model, preds)

melted_auto_sarima_df <- melted_auto_sarima_df %>%
  mutate(okres_2 = factor(okres_2, 
                          levels = c("przed epidemią (zbiór treningowy)", "po epidemii (zbiór testowy)")))

melted_data_imp_preds <- melted_data_imp_preds %>% 
  mutate(okres_2 = ifelse(okres != "przed epidemią", 
                         ifelse(okres == "po epidemii", "po epidemii (zbiór testowy)", NA),
                         "przed epidemią (zbiór treningowy)")) %>% 
  mutate(okres_2 = factor(okres_2, 
                          levels = c("przed epidemią (zbiór treningowy)", "po epidemii (zbiór testowy)")))


ggplot() +
  geom_line(data = model_data_2 %>% filter(okres == "po epidemii"), 
            aes(x = as.Date(time), y = values)) +
  geom_line(data = melted_data_imp_preds %>% 
              filter(okres == "po epidemii" & model == "SARIMA(1,1,1)(0,1,1)[12]"),
            aes(x = as.Date(time), y = preds, color = model)) +
  geom_line(data = melted_auto_sarima_df %>% filter(okres == "po epidemii"),
            aes(x = as.Date(time), y = preds, col = model)) +
  scale_color_discrete(labels = label_parse()) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  ggtitle("Porównanie modeli SARIMA uczonych z okresem pandemicznym")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))


ggplot() +
  geom_line(data = model_data_2 %>% filter(!is.na(okres_2)), 
            aes(x = as.Date(time), y = values)) +
  geom_line(data = melted_data_imp_preds %>% 
              filter(model == "SARIMA(1,1,1)(0,1,1)[12]" & !is.na(okres_2)),
            aes(x = as.Date(time), y = preds, color = model)) +
  geom_line(data = melted_auto_sarima_df%>% filter(!is.na(okres_2)),
            aes(x = as.Date(time), y = preds, col = model)) +
  scale_color_discrete(labels = label_parse()) +
  facet_wrap(.~okres_2, scale = "free", nrow = 3) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  ggtitle("Porównanie modeli SARIMA uczonych z imputacją okresu pandemii") +
  theme(plot.title = element_text(hjust = 0.5, size = 12))
  
  
  
