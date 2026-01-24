
prophet_models <- mapply(function(train_ts, train){
  prophet(data.frame(
    ds = train_data$time[1:length(train)],
    y = train
  ))
}, train_sets_ts, train_sets, SIMPLIFY = FALSE)

no_covid_prophet <- prophet(
  data.frame(ds = as.Date(train_no_covid_ts),
             y = exp(train_no_covid)),
  yearly.seasonality = TRUE, seasonality.mode = "multiplicative"
)

forecast <- predict(no_covid_prophet, data.frame(ds = model_data$time))

fit_df <- model_data %>%
  bind_cols(forecast)

resid_df <- fit_df %>%
  filter(okres == "przed epidemią") %>%
  mutate(residuals = yhat - values)

ggplot(resid_df, aes(x = time, xend = time, y = 0, yend = residuals)) +
  geom_segment() +
  geom_hline(yintercept = 0) +
  labs(x = "Data", y = "Reszta",
       title = "Reszty dla modelu Prophet na zbiorze bez epidemii") +
  theme_light()

plt_model_data <- filter(model_data, okres != "w trakcie epidemii")

per_bounds <- plt_model_data %>%
  group_by(okres) %>%
  summarise(min = min(time),
            max = max(time)) %>%
  mutate(set = case_when(okres == "przed epidemią" ~ "treningowy",
                           .default = "testowy"))

ggplot(fit_df, aes(x = time)) +
  geom_rect(data = per_bounds, aes(xmin = min, xmax = max,
                ymin = -Inf, ymax = Inf, fill = set),
            inherit.aes = FALSE, alpha = 0.1) +
  geom_line(data = plt_model_data, aes(y = values, group = okres,
                                       color = "prawdziwe dane"),
            linewidth = 0.6) +
  geom_line(aes(y = yhat, color = "predykcja"), linewidth = 0.6) +
  geom_vline(xintercept = as.Date(covid_end)+31, linetype = "dashed") +
  labs(x = "Data", y = "Liczba pasażerów",
       title = "Predykcje modelu Prophet na podstawie danych bez okresu pandemii") +
  scale_fill_manual(values = c("treningowy" = "skyblue3", "testowy" = "red"),
                    name = "Zbiór") +
  scale_color_manual(values = c("prawdziwe dane" = "black",
                                "predykcja" = "mediumblue"),
                     name = "Dane") +
  theme_light() +
  theme(legend.position = "bottom")

prophet_plot_components(no_covid_prophet, forecast)





# future <- make_future_dataframe(no_covid_prophet,
#                                 periods = model_data %>% 
#                                   filter(okres != "przed epidemią") %>% nrow(),
#                                 freq = "month")
# forecast <- predict(no_covid_prophet, future)
# 
# fitted <- forecast %>%
#   filter(ds < covid_start) %>%
#   select(yhat) %>%
#   ts(start = c(2004, 1), frequency = 12)
# 
# covid_preds_prophet <- forecast %>%
#   filter(ds < covid_start) %>%
#   select(yhat) %>%
#   ts(start = c(2020, 3), frequency = 12)
# 
# mean((fitted - model_data %>% 
#         filter(okres == "przed epidemią") %>% select(log_values) %>% ts())^2)
# 
# sqrt(mean((exp(fitted$yhat) - model_data %>% 
#              filter(okres == "przed epidemią") %>% select(log_values) %>% exp() %>% unlist())^2))
# 
# 
# 
# 
# 
# 
# mean((forecast$yhat - model_data %>% 
#         filter(okres == "w trakcie epidemii") %>% select(log_values) %>% unlist())^2)
# mean((forecast$yhat - model_data %>% 
#         filter(okres == "w trakcie epidemii") %>% select(log_values) %>% unlist())^2)
# 
# prophet_plot_components(no_covid_prophet, forecast)
# 
# fitted(no_covid_prophet)