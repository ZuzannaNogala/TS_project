library(scales)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(TSstudio)
library(forecast)
library(prophet)
library(lmtest)
library(broom)

rmse <- function(real, pred) sqrt(mean((as.numeric(real) - as.numeric(pred)) ^ 2))

perform_analysis <- function(ts_data, nonses_order, ses_order){
  nonses_name <- paste0("(", paste(nonses_order, collapse = ","), ")")
  ses_name <- paste0("(", paste(nonses_order, collapse = ","), ")")
  mod_name <-paste0("SARIMA", nonses_name, ses_name, "_12")
  
  mod <- ts_data %>% Arima(order= nonses_order, seasonal= ses_order)
  
  mod %>% residuals() %>% 
    ggtsdisplay(main = paste("Analiza residuów modelu", mod_name)) +  # ocena residuów
    
    cat("\n--- Kryteria informacyjne ---\n")
  print(mod %>% glance() %>% select(AIC, BIC)) # ocena kryterii 
  
  cat("\n--- Testy istotności współczynników ---\n")
  print(mod %>% coeftest()) # istotnność
}

