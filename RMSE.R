# RMSE

# Wyniki RMSE zapisane w pliku "rmse_results_df.RDS" !

# Scenariusz 1:

N <- nrow(model_data)
true_vals_train <- model_data %>% filter(okres == "przed epidemią") %>% pull(values)
true_vals_cov <- model_data %>% filter(okres == "w trakcie epidemii") %>% pull(values)

true_vals_train_with_cov <- model_data %>% filter(okres != "po epidemii") %>% pull(values)
true_vals_test <- model_data %>% filter(okres == "po epidemii") %>% pull(values)

forecast_without_cov_auto <- predict(prophet_models$`bez epidemii`, data.frame(ds = model_data$time))

rmse_s1_df <- data.frame(
  model = c("sarima_011_011", "sarima_auto", "sarima_max", "prophet"),
  scenario = rep(1, 4),
  rmse_fit = c(
    with_imp_summary_df %>% filter(model == "sarima_own") %>%pull(rmse_fit),
    apply(sarima_models_fit$`bez epidemii`[1:length(true_vals_train), ], 2, rmse, true_vals_train),
    rmse(exp(forecast_without_cov_auto[1:length(true_vals_train), "yhat"]), true_vals_train)
    ),
  rmse_covid = c(
    with_imp_summary_df %>% filter(model == "sarima_own") %>%pull(rmse_cov),
    apply(sarima_models_fit$`bez epidemii`[(length(true_vals_train)+1):(N - length(true_vals_test)), ], 2, rmse, true_vals_cov),
    rmse(exp(forecast_without_cov_auto[(length(true_vals_train)+1):(N - length(true_vals_test)), "yhat"]), true_vals_cov)
  ),
  rmse_test = c(
    with_imp_summary_df %>% filter(model == "sarima_own") %>%pull(rmse_after_cov),
    apply(sarima_models_fit$`bez epidemii`[(N - length(true_vals_test) + 1):N, ], 2, rmse, true_vals_test),
    rmse(exp(forecast_without_cov_auto[(N- length(true_vals_test) + 1):N, "yhat"]), true_vals_test)))


# Scenariusz 2:

forecast_with_cov_auto <- predict(prophet_models$`z epidemią`, data.frame(ds = model_data$time))
forecast_with_cov <- predict(prophet_model, data.frame(ds = model_data$time))


rmse_s2_df <- data.frame(
  model = c("sarima_211_011", "sarima_auto", "sarima_max", "prophet_auto", "prophet"),
  scenario = rep(2, 5),
  rmse_fit = c(
    rmse_table %>% filter(model_id == "211") %>% pull(rmse_fit),
    apply(sarima_models_fit$`z epidemią`[1:length(true_vals_train_with_cov), ], 2, rmse, true_vals_train_with_cov),
    rmse(forecast_with_cov_auto[1:length(true_vals_train_with_cov), "yhat"], true_vals_train_with_cov),
    rmse(forecast_with_cov[1:length(true_vals_train_with_cov), "yhat"], true_vals_train_with_cov)
    ),
  rmse_covid = matrix(NA, nrow = 5, ncol =1),
  rmse_test = c(
    rmse_table %>% filter(model_id == "211") %>% pull(rmse_pred),
    apply(sarima_models_fit$`z epidemią`[(length(true_vals_train_with_cov) + 1):N, ], 2, rmse, true_vals_test),
    rmse(forecast_with_cov_auto[(length(true_vals_train_with_cov) + 1):N, "yhat"], true_vals_test),
    rmse(forecast_with_cov[(length(true_vals_train_with_cov) + 1):N, "yhat"], true_vals_test)))


# Scenariusz 3:

forecast_imp_cov_auto <- predict(prophet_models$imputacja, data.frame(ds = model_data$time))

rmse_s3_df <- data.frame(
  model = c("sarima_111_011", "sarima_auto", "sarima_max", "prophet_auto"),
  scenario = rep(3, 4),
  rmse_fit = c(
    rmse_table_imp %>% filter(model_id == "1101") %>% pull(rmse_fit),
    apply(sarima_models_fit$imputacja[1:length(true_vals_train_with_cov), ], 2, rmse, true_vals_train_with_cov),
    rmse(forecast_imp_cov_auto[1:length(true_vals_train_with_cov), "yhat"], true_vals_train_with_cov)
    ),
  rmse_covid = matrix(NA, nrow = 4, ncol =1),
  rmse_test = c(
    rmse_table_imp %>% filter(model_id == "1101") %>% pull(rmse_pred),
    apply(sarima_models_fit$imputacja[(length(true_vals_train_with_cov) + 1):N, ], 2, rmse, true_vals_test),
    rmse(forecast_imp_cov_auto[(length(true_vals_train_with_cov) + 1):N, "yhat"], true_vals_test)))


# Combine:

rmse_full_df <- rbind(rmse_s1_df, rmse_s2_df, rmse_s3_df)
rmse_full_df

# saveRDS(rmse_full_df, "rmse_results_df.RDS")
readRDS("rmse_results_df.RDS")

rmse_full_df %>% filter(scenario == 1) %>% sort_by(~rmse_fit)
rmse_full_df %>% filter(scenario == 1) %>% sort_by(~rmse_test)
rmse_full_df %>% filter(scenario == 1) %>% sort_by(~rmse_test)

rmse_full_df %>% filter(scenario != 1) %>% sort_by(~rmse_fit)
rmse_full_df %>% filter(scenario != 1) %>% sort_by(~rmse_test)

rmse_full_df %>% filter(scenario == 2) %>% select(model, rmse_fit, rmse_test)
