
source("set_up.R")

prophet_models <- lapply(train_sets_ts, function(train_ts){
  prophet(data.frame(ds = as.Date(train_ts),
                     y = exp(train_ts)),
    yearly.seasonality = TRUE, seasonality.mode = "multiplicative"
  )
})

forecasts <- lapply(prophet_models, function(model){
  predict(model, data.frame(ds = model_data$time)) 
})

prophet_plots <- mapply(function(model_forecast, scen_name){
  fit_df <- model_data %>%
    bind_cols(model_forecast)
  
  per_bounds <- if(scen_name == "bez epidemii")
    model_data %>%
      filter(okres != "w trakcie epidemii")
  else
    model_data
  
  per_bounds <- per_bounds %>%
    mutate(set = case_when(okres == "po epidemii" ~ "testowy",
                           .default = "treningowy")) %>%
    group_by(set) %>%
    summarise(min = min(time),
              max = max(time)) %>%
    mutate(max = case_when(set == "treningowy" ~ max + 14,
                           .default = max),
           min = case_when(set == "testowy" ~ min - 15,
                           .default = min))
  
  if(scen_name == "imputacja")
    model_data <- model_data %>%
      mutate(values = replace(values, okres != "po epidemii",
                              exp(train_imp_covid)))
  
  plt <- ggplot(fit_df, aes(x = time)) +
    geom_rect(data = per_bounds, aes(xmin = min, xmax = max,
                                     ymin = -Inf, ymax = Inf, fill = set),
              inherit.aes = FALSE, alpha = 0.1) +
    
    geom_ribbon(data = subset(fit_df, ds >= covid_end),
                aes(x = as.Date(ds), ymin = yhat_lower, ymax = yhat_upper), 
                fill = "#0072B2", alpha = 0.2) +
    
    geom_line(data = model_data,
              aes(y = values, color = "prawdziwe dane"),
              linewidth = 0.7) +
    
    geom_line(aes(y = yhat, color = "predykcja"), linewidth = 1.1) +
    
    geom_vline(xintercept = per_bounds %>%
                 filter(set == "testowy") %>%
                 select(min) %>%
                 unlist(),
               linetype = "dashed") +
    
    labs(x = "Data", y = "Liczba pasażerów") +
    
    scale_fill_manual(values = c("treningowy" = "red", "testowy" = "steelblue"),
                      name = "Zbiór") +
    
    scale_color_manual(values = c("prawdziwe dane" = "grey35",
                                  "predykcja" = "#0072B2"),
                       name = "Dane") +
    
    scale_y_continuous(labels = label_scientific()) +
    
    theme_minimal() +
    
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  
  if(scen_name == "bez epidemii")
    plt +
      geom_vline(xintercept = per_bounds %>%
                 filter(set == "treningowy") %>%
                 select(max) %>%
                 as.numeric(),
                 linetype = "dashed")
  else
    plt
}, forecasts, names(forecasts), SIMPLIFY = FALSE)

prophet_plots$`z epidemią` +
  ggtitle("Porównanie wartości rzeczywistych z prognozą modelu Prophet
          uwzględniającą wpływ COVID-19")

prophet_plots$imputacja +
  ggtitle("Porównanie wartości rzeczywistych z prognozą modelu Prophet
          po imputacji okresu COVID-19")
  
prophet_plots$`bez epidemii` +
  ggtitle("Porównanie wartości rzeczywistych z prognozą modelu Prophet
          nie uwzględniającej wpływu COVID-19")
  
