# Wybór SARIMY EKSPERCKO do danych zimputowanych 
source("set_up.R")

# train_imp_covid 
train_df <- model_data %>%
  filter(okres != "po epidemii") %>%
  mutate(log_imp_values = train_imp_covid)


# perform_analysis <- function(ts_data, nonses_order, ses_order){
#   nonses_name <- paste0("(", paste(nonses_order, collapse = ","), ")")
#   ses_name <- paste0("(", paste(nonses_order, collapse = ","), ")")
#   mod_name <-paste0("SARIMA", nonses_name, ses_name, "_12")
#     
#   mod <- ts_data %>% Arima(order= nonses_order, seasonal= ses_order)
#   
#   mod %>% residuals() %>% 
#     ggtsdisplay(main = paste("Analiza residuów modelu", mod_name)) +  # ocena residuów
#   
#   cat("\n--- Kryteria informacyjne ---\n")
#   print(mod %>% glance() %>% select(AIC, BIC)) # ocena kryterii 
#   
#   cat("\n--- Testy istotności współczynników ---\n")
#   print(mod %>% coeftest()) # istotnność
# }

# Komponenta I w części sezonowej i niesezonowej
ndiffs(ts(train_df$log_imp_values, frequency = 12))
nsdiffs(ts(train_df$log_imp_values,  frequency = 12))

# Pozbycie się sezonowości i ponowne zróżnicowanie
train_df %>%
  select(log_imp_values) %>%
  ts(frequency = 12) %>%
  diff(lag = 12) %>%
  diff() %>%
  ggtsdisplay(main="Double differenced log Poland AirPlane Passengers")

# Analiza ACF i PACF 

# ACF - analiza MA: pik w 1 - sugestia MA(1) w komponencie niesezonowej
# ACF - analiza MA: pik w 12 -  sugestia MA(1) w komponencie sezonowej

# PACF - analiza AR: pik w 1 - sugestia AR(1) w komponencie niesezonowej
# PACF - analiza AR: pik w 12, 24, 36 - sugestia max AR(3) w komponencie niesezonowej

# Najpierw prostsze modele: komponenta MA(1) i I=1 w obu kompomponentach:

perform_analysis(ts_data = train_df %>%
                   select(log_imp_values) %>%
                   ts(frequency = 12), 
                 nonses_order = c(0,1,1), 
                 ses_order = c(0,1,1))

 # wszytsko w przedziałach ufności - super
 # ocena kryterii # AIC=-642   BIC=-632
# istotne

# Lepiej tylko komponenta RA(1) i I=1 w obu kompomponentach?

perform_analysis(ts_data = train_df %>%
                   select(log_imp_values) %>%
                   ts(frequency = 12), 
                 nonses_order = c(1,1,0), 
                 ses_order = c(1,1,0))
# piki 24 poza przedziałem
# ocena kryterii AIC=-615  BIC=-605
# istotne, ar(1) bardziej niż ma(1) wczesniej


# Komponenta AR(1) i MA(1)

perform_analysis(ts_data = train_df %>%
                   select(log_imp_values) %>%
                   ts(frequency = 12), 
                 nonses_order = c(1,1,1), 
                 ses_order = c(1,1,1))

# wszystko prawie w PU
# ocena kryterii AIC=-641 BIC=-624.
 # nieistotne sar1


# Komponenta AR(1) i MA(1) w niesezonowej a w sezononowej tylko MA(1)

perform_analysis(ts_data = train_df %>%
                   select(log_imp_values) %>%
                   ts(frequency = 12), 
                 nonses_order = c(1,1,1), 
                 ses_order = c(0,1,1))

# wszytsko w PU - super
# ocena kryterii AIC=-643   BIC=-629
# istotne


# Wybór modelu na podstawie postowania wybranych kombinacji parametrów

mas <-c(0,1)
ars <- c(0,1)
smas <- c(0,1)
sars <- 0:3

grid <- expand.grid(p = ars, q = mas, r = smas, s = sars)[2:32, ]

models_to_choose <- apply(grid, 1, function(params) {
  p <- params["p"]
  q <- params["q"]
  r <- params["r"]
  s <- params["s"]
  
  model <- Arima(train_df %>%
                   select(log_imp_values) %>%
                   ts(start = c(2004, 1), frequency = 12), 
                 order = c(p, 1, q), 
                 seasonal = c(r, 1, s),
                 method = "ML")
  
  coef_signif <- coeftest(model)[, 4]
  all_signif <- sum(coef_signif <= 0.05) == length(coef_signif)
  
  return(data.frame(p, q, r, s, AIC = model$aic, BIC = model$bic, all_signif))
})

models_to_choose_df <- models_to_choose %>% bind_rows()

# 3 pierwsze modele najlepsze według kryterium AIC

models_to_choose_df %>% sort_by(~AIC) 
# modele:
# (1,1,1)(0,1,1) ind - 11
# (1,1,0)(0,1,1) ind - 9
# (0,1,1)(0,1,1) ind - 10

# 4 pierwsze modele najlepsze według kryterium BIC
models_to_choose_df %>% 
  sort_by(~BIC)
# modele:
# (1,1,0)(1,1,0) * - ind 9
# (0,1,0)(0,1,1)   - ind 8
# (0,1,1)(0,1,1) * - ind 10
# (1,1,1)(0,1,1) * - ind 11

# 3 z 4 modeli pokrywają się (oraz mają istotne współczynniki) więc tym czterem
# przyjrzymy się bliżej pod kątem 
# rmse na zbiorze treningowym i testowym 

candidats_grid <- grid[c(8, 9, 10, 11), ]

train_ts <- ts(train_df$log_imp_values, start = c(2004, 1), frequency = 12)
h_ahead <- nrow(model_data %>% filter(okres == "po epidemii"))
real_test <- exp(model_data %>% filter(okres == "po epidemii") %>% pull(log_values))


mod_results <-  apply(candidats_grid, 1, function(params){
  p <- as.numeric(params["p"])
  q <- as.numeric(params["q"])
  r <- as.numeric(params["r"])
  s <- as.numeric(params["s"])
  
  nonses_name <- paste0("(", paste(c(p, 1, q), collapse = ","), ")")
  ses_name <- paste0("(", paste(c(r, 1, s), collapse = ","), ")")
  mod_name <-paste0("SARIMA", nonses_name, ses_name, "_12")
  
  cat("\n--- MODEL ---\n")    
  cat(mod_name)
  
  mod <-  Arima(train_ts, 
                order = c(p, 1, q), 
                seasonal = c(r, 1, s),
                method = "ML")
  
  perform_analysis(train_ts, 
                   nonses_order = c(p, 1, q),
                   ses_order = c(r, 1, s))
  
  fit <- exp(as.numeric(fitted(mod)))
  rmse_fit <- rmse(exp(train_df$log_imp_values), fit)
  
  preds_obj <- predict(mod, n.ahead = h_ahead)
  preds <- exp(as.numeric(preds_obj$pred))
  rmse_pred <- rmse(real_test, preds)
  
  list("params" = c(p=p, q=q, r=r, s=s),
       "fit" = fit,
       "preds" = preds,
       "rmse_fit" = rmse_fit,
       "rmse_pred" = rmse_pred)})


# (0,1,0)(0,1,1) 
# piki 1 w MA i AR poza przedzialem
# AIC i BIC, odpowiednio: -639. -632.
# istotne

# (1,1,0)(1,1,0) 
# wszytsko w przedziale
# AIC i BIC, odpowiednio: -643. -632.
# istotne


# (0,1,1)(0,1,1) 
# wszytsko w przedziale
# AIC i BIC, odpowiednio: -642. -632.
# istotne

# (1,1,1)(0,1,1) 
#  wszytsko w 
# AIC i BIC, odpowiednio: -643. -629.
# istotne

# Zebranie wyników
names(mod_results) <- c("0001", "1001", "0101", "1101")

models_aft_preds_imp <- model_data %>% 
  mutate(imp_sarima_0001_preds = c(mod_results[["0001"]]$fit, mod_results[["0001"]]$preds),
         imp_sarima_1001_preds = c(mod_results[["1001"]]$fit, mod_results[["1001"]]$preds),
         imp_sarima_0101_preds = c(mod_results[["0101"]]$fit, mod_results[["0101"]]$preds),
         imp_sarima_1101_preds = c(mod_results[["1101"]]$fit, mod_results[["1101"]]$preds),
         log_imp_values = c(train_imp_covid, model_data %>% filter(okres == "po epidemii") %>% pull(log_values)))

melted_data_imp_preds <- models_aft_preds_imp %>%
  pivot_longer(
    cols = ends_with("_preds"), 
    names_to = "model",          
    values_to = "preds"         
  ) %>%
  select(time, values, log_imp_values, okres, model, preds)

melted_data_imp_preds <- melted_data_imp_preds %>%
  mutate(model = factor(model, 
                             levels = c("imp_sarima_0001_preds", "imp_sarima_1001_preds",
                                        "imp_sarima_0101_preds", "imp_sarima_1101_preds"),
                             labels = c(expression(SARIMA(0,1,0)(0,1,1)[12]), 
                                        expression(SARIMA(1,1,0)(0,1,1)[12]),
                                        expression(SARIMA(0,1,1)(0,1,1)[12]),
                                        expression(SARIMA(1,1,1)(0,1,1)[12]))))

# Wyniki rmse
fit_vals_imp <- sapply(mod_results, function(x) x$rmse_fit)
pred_vals_imp <- sapply(mod_results, function(x) x$rmse_pred)

rmse_table_imp <- data.frame(
  model_id = names(fit_vals_imp),
  rmse_fit = as.numeric(fit_vals_imp),
  rmse_pred = as.numeric(pred_vals_imp)
)

rmse_table_imp


# Wizualizacja wyników
ggplot(melted_data_imp_preds) +
  geom_line(aes(x = time, y = exp(log_imp_values))) +
  geom_line(aes(x = time, y = preds, color = model)) +
  facet_wrap(.~model, scale = "free", nrow=2, labeller = label_parsed)+
  theme(legend.position = "none") +
  xlab("Data") +
  ylab("Liczba pasażerów")+
  ggtitle("Porównanie modeli SARIMA (z imputacją okresu w trakcie pandemii) na całym zbiore danych")

ggplot(melted_data_imp_preds %>% filter(okres == "po epidemii")) +
  geom_line(aes(x = time, y = exp(log_imp_values))) +
  geom_line(aes(x = time, y = preds, color = model)) +
  facet_wrap(.~model, nrow=3, labeller = label_parsed) +
  theme(legend.position = "none") +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  ggtitle("Porównanie modeli SARIMA (z imputacją okresu w trakcie pandemii) na zbiorze po epidemii")


ggplot(melted_data_imp_preds %>% filter(okres == "po epidemii")) +
  geom_line(aes(x = time, y = exp(log_imp_values))) +
  geom_line(aes(x = time, y = preds, color = model)) +
  xlab("Data") +
  ylab("Liczba pasażerów") +
  scale_color_discrete(labels = label_parse()) +
  ggtitle("Porównanie modeli SARIMA (z imputacją okresu w trakcie pandemii) na zbiorze po epidemii")






