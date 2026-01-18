# Analiza szeregu z wykluczeniem okresu pandemicznego  2020-03-01 - 2022-05-01
# "Przewozy pasażerskie w transporcie lotniczym według kraju sprawozdającego - Polska"

library(dplyr)
library(ggplot2)
library(TSstudio)
library(forecast)
library(lmtest)

rmse <- function(real, pred) sqrt(mean((as.numeric(real) - as.numeric(pred)) ^ 2))

# Dane
model_data <- clean_data %>%
  mutate(
    okres = case_when(
      time < covid_start ~ "przed epidemią",
      time > covid_end ~ "po epidemii",
      .default = "w trakcie epidemii"
    ),
    log_values = log(values)
  )

# log - do stabilizacji wariancji
ts(cbind("oryginalne wartości" = model_data$values,
      'zlogarytmowane wartości'= model_data$log_values)) %>% 
  autoplot(facets=TRUE) + 
  ggtitle("Lotnicze przewozy pasażerskie w Polsce") 
  
## 1. Szukanie modelu do imputacji danych

# Zakładamy, że dane z okresu pandemii są nieznane
# i na podstawie danych przed pandemią uzupełniamy je

# 1a. Szukanie modelu SARIMA do imputacji danych przed covid (metoda ekspercka)

# Usunięcie sezonowości
model_data %>%
  filter(time < covid_start) %>%
  select(log_values) %>%
  unlist()  %>% 
  ts(frequency = 12) %>%
  diff(lag = 12) %>%
  ggtsdisplay(main="Lotnicze przewozy pasażerskie w Polsce - Szereg zróżnicowany sezonowo") # "Seasonally differenced log Poland AirPlane Passengers"

# Usunięcie sezonowości i dodatkowo zróżnicowany
model_data %>%
  filter(time < covid_start) %>%
  select(log_values) %>%
  unlist() %>% 
  ts(frequency = 12) %>%
  diff(lag = 12) %>%
  diff() %>%
  ggtsdisplay(main="Lotnicze przewozy pasażerskie w Polsce - Szereg zróżnicowany sezonowo + diff(lag=1)")

# Analiza wykresu

# Najpierw MA - analiza ACF
# spike w 1 - sugeruje MA(1) częsci niesezonowej
# spike w 12 - sugeruje MA(1) częsci sezonowej

# sprawdzenie ile razy trzeba zróżnicować by doprowadzić do stacjonarnarności
# cześć sezonową i niesezonową

ndiffs(model_data %>%
         filter(time < covid_start) %>%
         select(log_values) %>%
         unlist() %>% 
         ts(frequency = 12)) # 1 
nsdiffs(model_data %>%
          filter(time < covid_start) %>%
          select(log_values) %>%
          unlist() %>% 
          ts(frequency = 12)) # 1

# część I = 1 w obu przypadkach

# Wybierzemy model, które residua będą jak najbliżej białego szumu
# dodatkowo przyjrzymy się kryteriom i istotności współczynników modelu

model_data %>%
  filter(time < covid_start) %>%
  select(log_values) %>%
  unlist() %>%
  ts(frequency = 12) %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>%
  residuals() %>% ggtsdisplay() # wszytsko w przedziałach ufności - super

model_data %>%
  filter(time < covid_start) %>%
  select(log_values) %>%
  unlist() %>%
  ts(frequency = 12) %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>% summary() # ocena kryterii

# AIC=-532.58   AICc=-532.44   BIC=-522.98

model_data %>%
  filter(time < covid_start) %>%
  select(log_values) %>%
  unlist() %>%
  ts(frequency = 12) %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>% coeftest() # ocena istotności współczynników - ok 

# Czy dodanie AR coś poprawi - analiza PACF
# spike w 1 - sugeruje AR(1) częsci niesezonowej
# spike w 12 - sugeruje AR(1) częsci sezonowej

model_data %>%
  filter(time < covid_start) %>%
  select(log_values) %>%
  unlist() %>%
  ts(frequency = 12) %>%
  Arima(order=c(1,1,1), seasonal=c(1,1,1)) %>%
  residuals() %>% ggtsdisplay() # wszytsko w przedziałach ufności - super

model_data %>%
  filter(time < covid_start) %>%
  select(log_values) %>%
  unlist() %>%
  ts(frequency = 12) %>%
  Arima(order=c(1,1,1), seasonal=c(1,1,1)) %>% summary() # gorsze warości kryteria niż poprzednio

# AIC=-531.96   AICc=-531.62   BIC=-515.97


model_data %>%
  filter(time < covid_start) %>%
  select(log_values) %>%
  unlist() %>%
  ts(frequency = 12) %>%
  Arima(order=c(1,1,1), seasonal=c(1,1,1)) %>% coeftest() #SAR(1) nieistotna - poprzedni model lepszy


# Ponowna analiza ACF - czy może dodać komponente MA(2)

model_data %>%
  filter(time < covid_start) %>%
  select(log_values) %>%
  unlist() %>%
  ts(frequency = 12) %>%
  Arima(order=c(0,1,2), seasonal=c(0,1,1)) %>%
  residuals() %>% ggtsdisplay() # wszytsko w przedziałach ufności - super

model_data %>%
  filter(time < covid_start) %>%
  select(log_values) %>%
  unlist() %>%
  ts(frequency = 12) %>%
  Arima(order=c(0,1,2), seasonal=c(0,1,1)) %>% summary() # lepsze wyniki od poprzedniego, ale 
                                                         # gorsze warości kryteria niż pierwszego

# AIC=-532.34   AICc=-532.11   BIC=-519.54

model_data %>%
  filter(time < covid_start) %>%
  select(log_values) %>%
  unlist() %>%
  ts(frequency = 12) %>%
  Arima(order=c(0,1,2), seasonal=c(0,1,1)) %>% coeftest() # ale MA(2) nieistotna

# Wybór SARIMA(0,1,1)(0,1,1)_12

# Do imputacji danych zbiorem treningowym będzie cały zbiór danych przed wybuchem 
# pandemii

bef_cov_set <- model_data %>%
  filter(time < covid_start) %>%
  select(log_values) %>%
  unlist() %>%
  ts(frequency = 12)

in_cov_set <- model_data %>%
  filter(okres == "w trakcie epidemii") %>%
  select(log_values) %>%
  unlist() %>%
  ts(start = c(2020, 3), frequency = 12)

after_covid_start_set <- model_data %>%
  filter(time >= covid_start) %>%
  select(log_values) %>%
  unlist() %>% 
  ts(start = c(2020, 3), frequency = 12)

# Utworzenie modelu SARIMA(0,1,1)(0,1,1)_12 i predykcje

SARIMA_own <- Arima(bef_cov_set, order=c(0,1,1), seasonal=c(0,1,1))

SARIMA_own %>% summary() # AIC=-532.58   AICc=-532.44   BIC=-522.98
SARIMA_own %>% coeftest()

after_preds <- exp(predict(SARIMA_own, n.ahead =length(after_covid_start_set))$pred)
covid_preds <- after_preds[1:(model_data %>% filter(okres == "w trakcie epidemii") %>% nrow())]
after_covid_preds <- after_preds[(model_data %>% filter(okres == "w trakcie epidemii") %>% nrow() + 1):length(after_covid_start_set)]

model_data_with_preds <- model_data %>%
  mutate(sarima_preds = round(c(exp(fitted(SARIMA_own)), after_preds), 0))


# Imputacja danych covidowych (oryginalna skala)
ggplot(model_data_with_preds %>% filter(okres != "po epidemii")) +
  geom_line(aes(x = time, y = values)) +
  geom_line(aes(x = time, y = sarima_preds), col = "blue")

# Co po okresie covidowym, gdy zastosowaliśmy imputację danych ? (oryginalna skala)
ggplot(model_data_with_preds) +
  geom_line(aes(x = time, y = values)) +
  geom_line(aes(x = time, y = sarima_preds), col = "blue")


# 1b. Model AUTO.SARIMA z komponentą sezonową
SARIMA_auto <- auto.arima(bef_cov_set, seasonal = TRUE) # (4,1,2)(2,1,1) - bardzo skomplikowany model

SARIMA_auto %>% summary()
# AIC=-523.42   AICc=-522.12   BIC=-491.43 - gorsze niz wybrany ekspercko
SARIMA_auto %>% coeftest() # prawie nic nie istotne

after_preds_auto <- exp(predict(SARIMA_auto, n.ahead = length(after_covid_start_set))$pred)
covid_preds_auto <- after_preds_auto[1:(model_data %>% filter(okres == "w trakcie epidemii") %>% nrow())]
after_covid_preds_auto <- after_preds_auto[(model_data %>% filter(okres == "w trakcie epidemii") %>% nrow() + 1):length(after_covid_start_set)]

model_data_with_preds <- model_data_with_preds %>%
  mutate(sarima_auto_preds = round(c(exp(fitted(SARIMA_auto)), after_preds_auto), 0))


# Imputacja danych covidowych (normalne values)
ggplot(model_data_with_preds %>% filter(okres != "po epidemii")) +
  geom_line(aes(x = time, y = values)) +
  geom_line(aes(x = time, y = sarima_auto_preds), col = "blue")

# Co po okresie covidowym, gdy zastosowaliśmy imputację danych ? (normalne)
ggplot(model_data_with_preds) +
  geom_line(aes(x = time, y = values)) +
  geom_line(aes(x = time, y = sarima_auto_preds), col = "blue")


# 1c. Model AUTO.SARIMA z komponentą sezonową z max
SARIMA_auto_max <- auto.arima(bef_cov_set, 
                              max.p = 1,
                              max.q = 2,
                              max.P = 1,
                              max.Q = 2,
                              seasonal = TRUE) # (1,1,0)(1,1,0) 
SARIMA_auto_max%>% summary()
# AIC=-511.88   AICc=-511.74   BIC=-502.28
SARIMA_auto_max %>% coeftest() # istotne

after_preds_auto_max <- exp(predict(SARIMA_auto_max, n.ahead = length(after_covid_start_set))$pred)
covid_preds_auto_max <- after_preds_auto_max[1:(model_data %>% filter(okres == "w trakcie epidemii") %>% nrow())]
after_covid_preds_auto_max <- after_preds_auto_max[(model_data %>% filter(okres == "w trakcie epidemii") %>% nrow() + 1):length(after_covid_start_set)]

model_data_with_preds <- model_data_with_preds %>%
  mutate(sarima_auto_max_preds = round(c(exp(fitted(SARIMA_auto_max)), after_preds_auto), 0))




# Imputacja danych covidowych (log values)
ggplot(model_data_with_preds %>% filter(okres != "po epidemii")) +
  geom_line(aes(x = time, y = values)) +
  geom_line(aes(x = time, y = sarima_auto_max_preds), col = "blue")

# Co po okresie covidowym, gdy zastosowaliśmy imputację danych ? (normalne)
ggplot(model_data_with_preds) +
  geom_line(aes(x = time, y = values)) +
  geom_line(aes(x = time, y = sarima_auto_max_preds), col = "blue")

# 2. RMSE

with_imp_summary_df <- data.frame("model" = c("sarima_own", "sarima_auto", "sarima_max"),
                                  "rmse_fit" = unlist(lapply(list(SARIMA_own, SARIMA_auto, SARIMA_auto_max), 
                                                             function(mod) rmse(exp(bef_cov_set), round(exp(fitted(mod)), 0)))),
                                  "rmse_cov" = c(rmse(exp(in_cov_set), covid_preds), 
                                                 rmse(exp(in_cov_set), covid_preds_auto), 
                                                 rmse(exp(in_cov_set), covid_preds_auto_max)),
                                  "rmse_after_cov" = c(rmse(exp(after_covid_set), after_covid_preds), 
                                                       rmse(exp(after_covid_set), after_covid_preds_auto), 
                                                       rmse(exp(after_covid_set), after_covid_preds_auto_max)),
                                  "rmse_after_start_cov" = c(rmse(exp(after_covid_start_set), after_preds), 
                                                             rmse(exp(after_covid_start_set), after_preds_auto), 
                                                             rmse(exp(after_covid_start_set), after_preds_auto_max)),
                                  "AIC" = c(SARIMA_own %>% AIC(), SARIMA_auto %>% AIC(), SARIMA_auto_max %>% AIC()),
                                  "BIC" = c(SARIMA_own %>% BIC(), SARIMA_auto %>% BIC(), SARIMA_auto_max %>% BIC()))


# 3. Wykresy podsumowujące

melted_data_with_preds <- model_data_with_preds %>%
  pivot_longer(
    cols = ends_with("_preds"), 
    names_to = "model",          
    values_to = "preds"          
  ) %>%
select(time, values, okres, model, preds)

melted_data_with_preds <- melted_data_with_preds %>%
  mutate(okres_2 = factor(okres, 
                        levels = c("przed epidemią", "w trakcie epidemii", "po epidemii"),
                        labels = c("przed epidemią (zbiór treningowy)", 
                                   "w trakcie epidemii", "po epidemii")))

melted_data_with_preds <- melted_data_with_preds %>%
  mutate(model = factor(model, 
                          levels = c("sarima_preds", "sarima_auto_preds", "sarima_auto_max_preds"),
                          labels = c(paste(expression(SARIMA(0,1,1)(0,1,1)[12]), " (ekspercka)"), 
                                     paste(expression(SARIMA(4,1,2)(2,1,1)[12]), " (auto.arima)"),
                                     paste(expression(SARIMA(1,1,0)(1,1,0)[12]), " (auto.arima_max)"))))


ggplot(melted_data_with_preds) +
  geom_line(aes(x = time, y = values)) +
  geom_line(aes(x = time, y = preds, color = model)) +
  scale_color_discrete(labels = label_parse()) +
  facet_wrap(.~okres_2, scale = "free", nrow=3) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  ggtitle("Porównanie modeli SARIMA uczonych bez okresu pandemicznego w poszczególnych okresach")


ggplot(melted_data_with_preds) +
  geom_line(aes(x = time, y = values)) +
  geom_line(aes(x = time, y = preds, color = model)) +
  scale_color_discrete(labels = label_parse()) +
  facet_wrap(.~model, scale = "free", nrow=3, labeller = label_parsed)+
  xlab("Data") +
  ylab("Liczba pasażerów") +
  ggtitle("Porównanie modeli SARIMA uczonych bez okresu pandemicznego na całym zbiorze danych")



                                                     
