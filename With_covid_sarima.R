# Analiza szeregu bez wykluczeniem okresu pandemicznego 
# "Przewozy pasażerskie w transporcie lotniczym według kraju sprawozdającego - Polska"

with_covid_set <- model_data %>%
  filter(time <= covid_end) %>%
  select(log_values) %>%
  unlist() %>% 
  ts(frequency = 12)

after_covid_set <- model_data %>%
  filter(time > covid_end) %>%
  select(log_values) %>%
  unlist() %>% 
  ts(start = c(2022, 6), frequency = 12)


# szereg bez sezonowości i dodatkowo zróżnicowany z covid
with_covid_set %>%
  diff(lag = 12) %>%
  diff() %>%
  ggtsdisplay(main="Double differenced log Poland AirPlane Passengers")

# Mamy pik w 12 w ACF -> sugestia by MA(1) w części sezonowej
# Mamy istotne piki w ACF -> sugestia że conajmniej MA(1) w częsci niesezonowej 

with_covid_set %>%
  Arima(order = c(0,1,1), seasonal = c(0,1,1)) %>%
  residuals() %>% ggtsdisplay() 

# nie ma pików w wielokrotności 12 - komponenta sezonowa ok
# Ale jest silny pik w 3 w ACF i PACF - próba dodania AR(1)

with_covid_set %>%
  Arima(order = c(1,1,1), seasonal = c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()

# Ale jest silny pik w 3 w ACF i PACF - próba dodania kolejne komponenty MA

with_covid_set %>%
  Arima(order = c(1,1,2), seasonal = c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()

# Ale jest silny pik w 3 w ACF i PACF - próba dodania kolejne komponenty MA

with_covid_set %>%
  Arima(order = c(1,1,3), seasonal = c(0,1,1)) %>%
  residuals() %>% ggtsdisplay() # fajnie

with_covid_set %>%
  Arima(order = c(2,1,1), seasonal = c(0,1,1)) %>%
  residuals() %>% ggtsdisplay() 


# testujemy takie parametry dobrać na bazie BIC, AIC, istotności parametrów
ars <- 1:3
mas <- 1:3

grid <- expand.grid(p = ars, q = mas)

ts_data <- model_data %>%
  filter(time <= covid_end) %>%
  select(log_values) %>%
  ts(start = c(2004, 1), frequency = 12)

results <- apply(grid, 1, function(params) {
  p <- params["p"]
  q <- params["q"]
  
  perform_analysis(ts_data = ts_data, 
                   nonses_order = c(p, 1, q),
                   ses_order = c(0, 1, 1))
  
  model <- Arima(ts_data, 
                 order = c(p, 1, q), 
                 seasonal = c(0, 1, 1),
                 method = "ML")
  
  coef_signif <- coeftest(model)[, 4]
  all_signif <- sum(coef_signif <= 0.05) == length(coef_signif)
  
  return(data.frame(p, q, AIC = model$aic, BIC = model$bic, all_signif))
})

final_results <- bind_rows(results)
final_results

final_results %>% 
  sort_by(~AIC) 

final_results %>% 
  sort_by(~BIC)

# 1 1 - ar1 nieistotne
# 2 1 - istotne
# 3 1 - ar3 nieistotne 
# 1 2 - istotne
# 2 2 - ma2 nieistotne
# 3 2 - ar3 nieistotne, ma2 nieistotne
# 1 3 - ma1 nieistotne
# 2 3 - masakra
# 3 3 - masakra

# Na podstawie analizy wybrane modele do sprawdzenia prognoz

covid_model_113 <- Arima(with_covid_set, 
                         order = c(1,1,3), 
                         seasonal = c(0,1,1))

covid_model_211 <- Arima(with_covid_set, 
                         order = c(2,1,1), 
                         seasonal = c(0,1,1))

covid_model_311 <- Arima(with_covid_set, 
                         order = c(3,1,1), 
                         seasonal = c(0,1,1))

# Bliższe przyjrzenie się kandydatom 
perform_analysis(ts_data = ts_data,
                 nonses_order = c(1,1,3),
                 ses_order = c(0,1,1))

# prawie wszystko w PU
# AIC   BIC -  143.  163.
# nieistotne ma1

perform_analysis(ts_data = ts_data,
                 nonses_order = c(2,1,1),
                 ses_order = c(0,1,1))

# wszystko ładnie w PU
# AIC   BIC -  148.  165.
# istotne

perform_analysis(ts_data = ts_data,
                 nonses_order = c(3,1,1),
                 ses_order = c(0,1,1))

# wszystko ładnie w PU
# AIC   BIC - 147.  167.
# nieistotne AR 3

candidats_grid <- grid[c(7, 2, 3), ]

train_ts <- ts(with_covid_set, start = c(2004, 1), frequency = 12)
h_ahead <- nrow(model_data %>% filter(okres == "po epidemii"))
real_test <- exp(model_data %>% filter(okres == "po epidemii") %>% pull(log_values))


mod_results <-  apply(candidats_grid, 1, function(params){
  p <- as.numeric(params["p"])
  q <- as.numeric(params["q"])
  
  nonses_name <- paste0("(", paste(c(p, 1, q), collapse = ","), ")")
  ses_name <- "(0,1,1)"
  mod_name <-paste0("SARIMA", nonses_name, ses_name, "_12")
  
  cat("\n--- MODEL ---\n")    
  cat(mod_name)
  
  mod <-  Arima(train_ts, 
                order = c(p, 1, q), 
                seasonal = c(0, 1, 1),
                method = "ML")
  
  # perform_analysis(train_ts, 
  #                  nonses_order = c(p, 1, q),
  #                  ses_order = c(0, 1, 1))
  
  fit <- exp(as.numeric(fitted(mod)))
  rmse_fit <- rmse(exp(train_df$log_imp_values), fit)
  
  preds_obj <- predict(mod, n.ahead = h_ahead)
  preds <- exp(as.numeric(preds_obj$pred))
  rmse_pred <- rmse(real_test, preds)
  
  list("params" = c(p=p, q=q, r=0, s=1),
       "fit" = fit,
       "preds" = preds,
       "rmse_fit" = rmse_fit,
       "rmse_pred" = rmse_pred)})

# Podusmowanie wyników
names(results) <- c("113", "211", "311")

models_aft_preds <- model_data %>% 
  mutate(covid_sarima_113_preds = c(results[["113"]]$fit, results[["113"]]$preds),
         covid_sarima_211_preds = c(results[["211"]]$fit, results[["211"]]$preds),
         covid_sarima_311_preds = c(results[["311"]]$fit, results[["311"]]$preds))


melted_data_with_preds_aft <- models_aft_preds %>%
  pivot_longer(
    cols = ends_with("_preds"), 
    names_to = "model",          
    values_to = "preds"         
  ) %>%
  select(time, values, okres, model, preds)


melted_data_with_preds_aft <- melted_data_with_preds_aft %>%
  mutate(model = factor(model, 
                        levels = c("covid_sarima_113_preds", "covid_sarima_211_preds",
                                   "covid_sarima_311_preds"),
                        labels = c(expression(SARIMA(1,1,3)(0,1,1)[12]), 
                                   expression(SARIMA(2,1,1)(0,1,1)[12]),
                                   expression(SARIMA(3,1,1)(0,1,1)[12]))))

melted_data_with_preds_aft <- melted_data_with_preds_aft %>%
  mutate(okres_2 = ifelse(okres %in% c("przed epidemią", "w trakcie epidemii"), 
                        "Przed i w trakcie epidemii", 
                        as.character(okres)))

melted_data_with_preds_aft <- melted_data_with_preds_aft %>%
  mutate(okres_2 = factor(okres_2, 
                          levels = c("Przed i w trakcie epidemii", "po epidemii"),
                          labels = c("przed i w trakcie epidemii (zbiór treningowy)", "po epidemii (zbiór testowy)")))

# Podsumowanie RMSE

fit_vals <- sapply(results, function(x) x$rmse_fit)
pred_vals <- sapply(results, function(x) x$rmse_pred)

rmse_table <- data.frame(
  model_id = names(fit_vals),
  rmse_fit = as.numeric(fit_vals),
  rmse_pred = as.numeric(pred_vals)
)

rmse_table
with_imp_summary_df

# 3. Wykresy podsumowujące

ggplot(melted_data_with_preds_aft) +
  geom_line(aes(x = time, y = values)) +
  geom_line(aes(x = time, y = preds, color = model)) +
  scale_color_discrete(labels = label_parse()) +
  facet_wrap(.~okres_2, scale = "free", nrow=3) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  ggtitle("Porównanie modeli SARIMA uczonych z okresem pandemicznym")

ggplot(melted_data_with_preds_aft) +
  geom_line(aes(x = time, y = values)) +
  geom_line(aes(x = time, y = preds, color = model)) +
  scale_color_discrete(labels = label_parse()) +
  theme(legend.position = "none") +
  ggtitle("Porównanie modeli SARIMA (uczonych z okresem pandemicznym) na całym zbiorze danych") +
  facet_wrap(.~model, scale = "free", nrow=3, labeller = label_parsed)

