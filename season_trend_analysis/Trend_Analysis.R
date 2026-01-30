# Analiza trendu 

# Oryginalne dane - ignorując anomalie covidową trend jest ściśle rosnący, ale nie jest liniowy, 
# bardziej rośnie eksponencjonalnie 

ggplot(clean_data) +
  geom_line(aes(x = time, y = values)) +
  ggtitle("Przewozy pasażerskie w transporcie lotniczym według kraju sprawozdającego - Polska")

# Dopasowywanie trendu na danych zlogarytmowanych
train_trend_set_bef <- model_data %>%
  filter(okres == "przed epidemią") %>%
  select(time, log_values)

# 1. Trend na danych przed epidemią (co by było gdyby pandemii nie było - wariant pozytywny)
# 2. Trend na danych z okresem pandemii (uwzględnienie pandemii - wariant pesisistyczny)
# 3. Trend na danych po pandemii (trend teraźniejszy, który wskaże nam trend długoterminowy)

trend_bef_covid <- lm(log_values ~ time, data = model_data %>% filter(okres == "przed epidemią"))
trend_with_covid <- lm(log_values ~ time, data = model_data %>% filter(okres != "po epidemii"))
trend_aft_covid <- lm(log_values ~ time, data = model_data  %>% filter(okres == "po epidemii"))  


trend_set <- model_data %>%
  mutate(
    trend_bef_cov  = exp(predict(trend_bef_covid, newdata = .)),
    trend_with_cov = exp(predict(trend_with_covid, newdata = .)),
    trend_aft_cov  = exp(predict(trend_aft_covid, newdata = .))
  )


melted_trend_set <- trend_set %>%
  pivot_longer(
    cols = ends_with("_cov"), 
    names_to = "trend_type",          
    values_to = "trend"          
  ) %>%
  select(time, values, okres, trend, trend_type)

melted_trend_set <- melted_trend_set %>%
  mutate(trend_type = factor(trend_type, 
                             levels = c("trend_bef_cov", "trend_with_cov", "trend_aft_cov"),
                             labels = c("Przed epidemią", "Uwzględniający epidemię", "Po epidemii")))

# Porównanie trendów
ggplot(melted_trend_set) +
  geom_line(aes(x = time, y = values)) +
  geom_line(aes(x = time, y = trend, color = trend_type), linewidth = 1) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  labs(colour = "Trend") +
  facet_wrap(.~trend_type, nrow = 3) +
  ggtitle("Analiza zmian trendu długookresowego w przewozach lotniczych w Polsce")

# Jak będzie zachowywał się trend w przyszłości? 
future_dates <- seq(from = as.Date("2025-09-01"), 
                    to = as.Date("2030-09-01"), 
                    by = "month")

future_trend_df <- data.frame(time = future_dates)

trend_set_future <- future_trend_df %>%
  mutate(
    trend_bef_cov  = exp(predict(trend_bef_covid, newdata = .)),
    trend_with_cov = exp(predict(trend_with_covid, newdata = .)),
    trend_aft_cov  = exp(predict(trend_aft_covid, newdata = .))
  )

melted_trend_set_future <- trend_set_future %>%
  pivot_longer(
    cols = ends_with("_cov"), 
    names_to = "trend_type",          
    values_to = "trend"          
  ) %>%
  select(time, trend, trend_type)

melted_trend_set_future <- melted_trend_set_future %>%
  mutate(trend_type = factor(trend_type, 
                             levels = c("trend_bef_cov", "trend_with_cov", "trend_aft_cov"),
                             labels = c("Przed epidemią", "Uwzględniający epidemię", "Po epidemii")))

# Porównanie trendów z przyszłości 

ggplot() +
  geom_line(data = model_data, aes(x = time, y = values)) +
  geom_line(data = melted_trend_set,  aes(x = time, y = trend, color = trend_type), linewidth = 1) +
  geom_line(data = melted_trend_set_future, aes(x = time, y = trend, color = trend_type), linewidth = 1) +
  geom_vline(xintercept = as.Date("2025-08-01"), linetype = "dashed", color = "darkgrey", linewidth = 0.8) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  labs(colour = "Trend") +
  facet_wrap(.~trend_type, nrow = 3) +
  ggtitle("Analiza zmian trendu długookresowego w przewozach lotniczych w Polsce w przyszłości")

# Porównanie trendów z przyszłości - jeden wykres

ggplot() +
  geom_line(data = model_data, aes(x = time, y = values)) +
  geom_line(data = melted_trend_set,  aes(x = time, y = trend, color = trend_type), linewidth = 1) +
  geom_line(data = melted_trend_set_future, aes(x = time, y = trend, color = trend_type), linewidth = 1) +
  geom_vline(xintercept = as.Date("2025-08-01"), linetype = "dashed", color = "darkgrey", linewidth = 0.8) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  labs(colour = "Trend") +
  ggtitle("Analiza zmian trendu długookresowego w przewozach lotniczych w Polsce")

