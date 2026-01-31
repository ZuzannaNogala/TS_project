
source("set_up.R")

sarima_models <- mapply(function(train_ts, train){
    list(
      sarima = auto.arima(train_ts, d = 1, D = 1),
      max_sarima = auto.arima(train_ts, max.p = 8, max.q = 8, max.order = 8,
                              d = 1, D = 1, stepwise = FALSE)
    )
}, train_sets_ts, train_sets, SIMPLIFY = FALSE)

# analiza dopasowania - reszty, istotne współczynniki
sarima_resid_plots <- lapply(sarima_models, function(models_lst){
  plts <- mapply(function(model, model_name){
    list(
      ggAcf(model$residuals) +
        ggtitle(paste(model_name, "- ACF")) +
        theme_light() +
        theme(plot.title = element_text(size = 13, hjust = 0.5)),
      ggPacf(model$residuals) +
        ggtitle(paste(model_name, "- PACF")) +
        theme_light() +
        theme(plot.title = element_text(size = 13, hjust = 0.5))
    )
  }, models_lst, c("auto SARIMA", "max SARIMA"))
  
  wrap_plots(plts)
})

sarima_resid_plots$`z epidemią`
sarima_resid_plots$imputacja
sarima_resid_plots$`bez epidemii`

sarima_coefs <- lapply(sarima_models, function(models_lst){
  lapply(models_lst[1:2], coeftest)
})

sarima_coefs$`z epidemią`
sarima_coefs$`bez epidemii`
sarima_coefs$imputacja

# test
sarima_models_fit <- mapply(function(models_lst, scenario){
  n.ahead <- nrow(test_data)
  
  if(scenario == "bez epidemii")
    n.ahead <- n.ahead + covid_obs_num
  
  sapply(models_lst, function(model){
    c(exp(model$fitted),
      exp(predict(model, n.ahead = n.ahead)$pred))
  })
}, sarima_models, names(sarima_models), SIMPLIFY = FALSE)

sarima_models_plt <- mapply(function(fit_matrix, train){
  fit_df <- fit_matrix %>%
    as.data.frame() %>%
    mutate(time = model_data$time) %>%
    pivot_longer(cols = sarima:max_sarima, names_to = "model",
                 values_to = "values", cols_vary = "slowest")
  
  if(length(train) + nrow(test_data) < nrow(model_data))
    model_data <- filter(model_data, okres != "w trakcie epidemii")
  
  plt1 <- ggplot(fit_df, aes(x = time, y = values)) +
    geom_line(aes(color = model)) +
    geom_line(data = model_data, aes(group = okres)) +
    geom_vline(xintercept = as.Date(covid_end), linetype = "dashed") +
    labs(x = "Data", y = "Liczba pasażerów",
         title = "Predykcje automatycznych modeli SARIMA na zbiorze treningowym
         i testowym") +
    theme_light()
    
  pred_df <- fit_df %>%
    filter(time > as.Date(covid_end))
     
  plt2 <- ggplot(pred_df, aes(x = time, y = values)) +
    geom_line(aes(color = model)) +
    geom_line(data = test_data) +
    labs(x = "Data", y = "Liczba pasażerów",
         title = "Predykcje automatycznych modeli SARIMA na zbiorze testowym") +
  theme_light()
  
  plt1 / plt2 +
    plot_layout(guides = "collect") &
    theme(legend.position = 'bottom')
}, sarima_models_fit, train_sets, SIMPLIFY = FALSE)
