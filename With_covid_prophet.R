library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(TSstudio)
library(forecast)
library(prophet)
library(lmtest)


prophet_model <- prophet(df = model_data %>% 
                           filter(okres == "przed epidemią") %>% select(ds = time, y=log_values))
# change points - gdzie cos sie zmienia (spropbac z okresem pandemicznym) 


future <- make_future_dataframe(prophet_model, periods = model_data %>% 
                                  filter(okres == "w trakcie epidemii") %>% nrow(), freq = "month")
forecast <- predict(prophet_model, future)

fitted <- forecast %>%
  filter(ds < covid_start) %>%
  select(yhat) %>%
  ts(start = c(2004, 1), frequency = 12)

covid_preds_prophet <- forecast %>%
  filter(ds >= covid_start & ds < covid_end) %>%
  select(yhat) %>%
  ts(start = c(2020, 3), frequency = 12)

mean((fitted - model_data %>% 
        filter(okres == "przed epidemią") %>% select(log_values) %>% ts())^2)

sqrt(mean((exp(fitted$yhat) - model_data %>% 
        filter(okres == "przed epidemią") %>% select(log_values) %>% exp() %>% unlist())^2))






mean((forecast$yhat - model_data %>% 
        filter(okres == "w trakcie epidemii") %>% select(log_values) %>% unlist())^2)
mean((forecast$yhat - model_data %>% 
        filter(okres == "w trakcie epidemii") %>% select(log_values) %>% unlist())^2)

prophet_plot_components(prophet_model, forecast)

fitted(prophet_model)
