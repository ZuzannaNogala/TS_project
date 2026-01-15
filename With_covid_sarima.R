# szereg bez sezonowości i dodatkowo zróżnicowany z covid
model_data %>%
  filter(time <= covid_end) %>%
  select(log_values) %>%
  unlist() %>% 
  ts(frequency = 12) %>%
  diff(lag = 12) %>%
  diff() %>%
  ggtsdisplay(main="Double differenced log Poland AirPlane Passengers")

# Mamy pik w 12 w ACF -> sugestia by MA(1) w części sezonowej
# Mamy istotne piki w ACF -> sugestia że conajmniej MA(1) w częsci niesezonowej 

model_data %>%
  filter(time <= covid_end) %>%
  select(log_values) %>%
  unlist() %>% 
  ts(frequency = 12) %>%
  Arima(order = c(0,1,1), seasonal = c(0,1,1)) %>%
  residuals() %>% ggtsdisplay() 


# nie ma pików w wielokrotności 12 - komponenta sezonowa ok
# Ale jest silny pik w 3 w ACF i PACF - próba dodania AR(1)

model_data %>%
  filter(time <= covid_end) %>%
  select(log_values) %>%
  unlist() %>% 
  ts(frequency = 12) %>%
  Arima(order = c(1,1,1), seasonal = c(0,1,1)) %>%
  residuals() %>%
  ggtsdisplay()

# Ale jest silny pik w 3 w ACF i PACF - próba dodania koljenje komponenty MA

model_data %>%
  filter(time <= covid_end) %>%
  select(log_values) %>%
  unlist() %>% 
  ts(frequency = 12) %>%
  Arima(order = c(1,1,2), seasonal = c(0,1,1)) %>%
  residuals() %>%
  ggtsdisplay()

# Ale jest silny pik w 3 w ACF i PACF - próba dodania koljenje komponenty MA


model_data %>%
  filter(time <= covid_end) %>%
  select(log_values) %>%
  unlist() %>% 
  ts(frequency = 12) %>%
  Arima(order = c(1,1,3), seasonal = c(0,1,1)) %>%
  residuals() %>%
  ggtsdisplay() # fajnie


model_data %>%
  filter(time <= covid_end) %>%
  select(log_values) %>%
  unlist() %>% 
  ts(frequency = 12) %>%
  Arima(order = c(1,1,3), seasonal = c(0,1,1)) %>%
  residuals() %>%
  ggtsdisplay()


model_data %>%
  filter(time <= covid_end) %>%
  select(log_values) %>%
  unlist() %>% 
  ts(frequency = 12) %>%
  Arima(order = c(1,1,3), seasonal = c(0,1,1)) %>%
  residuals() %>%
  ggtsdisplay() 

model_data %>%
  filter(time <= covid_end) %>%
  select(log_values) %>%
  unlist() %>% 
  ts(frequency = 12) %>%
  Arima(order = c(2,1,1), seasonal = c(0,1,1)) %>%
  residuals() %>%
  ggtsdisplay() 


model_data %>%
  filter(time <= covid_end) %>%
  select(log_values) %>%
  unlist() %>% 
  ts(frequency = 12) %>%
  Arima(order = c(1,1,2), seasonal = c(0,1,1)) %>%
  residuals() %>%
  ggtsdisplay() 


# testujemy takie parametry 
ars <- 1:3
mas <- 1:3


grid <- expand.grid(p = ars, q = mas)

results <- apply(grid, 1, function(params) {
  p <- params["p"]
  q <- params["q"]
  
  model <- Arima(model_data %>%
                   filter(time <= covid_end) %>%
                   select(log_values) %>%
                   unlist() %>% 
                   ts(start = c(2004, 1), frequency = 12), 
                 order = c(p, 1, q), 
                 seasonal = c(0, 1, 1),
                 method = "ML")
  
  coef_signif <- coeftest(model)[, 4]
  all_signif <- sum(coef_signif <= 0.05) == length(coef_signif)
  
  return(data.frame(p, q, AIC = model$aic, BIC = model$bic, all_signif))
})

final_results <- bind_rows(results)
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

model_data %>%
  filter(time <= covid_end) %>%
  select(log_values) %>%
  unlist() %>% 
  ts(frequency = 12) %>%
  Arima(order = c(1,1,3), seasonal = c(0,1,1)) %>%
  residuals() %>%
  ggtsdisplay() 


model_data %>%
  filter(time <= covid_end) %>%
  select(log_values) %>%
  unlist() %>% 
  ts(frequency = 12) %>%
  Arima(order = c(2,1,1), seasonal = c(0,1,1)) %>%
  residuals() %>%
  ggtsdisplay() 


model_data %>%
  filter(time <= covid_end) %>%
  select(log_values) %>%
  unlist() %>% 
  ts(frequency = 12) %>%
  Arima(order = c(3,1,1), seasonal = c(0,1,1)) %>%
  residuals() %>%
  ggtsdisplay() 