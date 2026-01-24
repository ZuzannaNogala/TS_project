
source("set_up.R")

# data plot
ggplot(clean_data) +
  geom_line(aes(x = time, y = values)) +
  labs(x = "Data", y = "Liczba pasażerów") +
  ggtitle("Przewozy pasażerskie w transporcie lotniczym w Polsce") +
  theme_light()

# periods plot
bound_obs <- which(model_data$time %in% c(covid_start, covid_end))
bound_obs <- c(bound_obs, bound_obs[1] - 1, bound_obs[2] + 1)

ggplot(model_data, aes(x = time, y = values)) +
  geom_line(data = cbind(model_data[sort(bound_obs),],
                         group = rep(1:2, each = 2)),
            aes(group = group), linetype = "dashed") +
  geom_line(aes(color = okres)) +
  labs(x = "Data", y = "Liczba pasażerów",
       title = "Podział danych na trzy okresy") +
  theme_light()

# imputation plot
ggplot(data.frame(val = exp(train_imp_covid),
                  date = train_data$time,
                  imputed = rep(c("nie", "tak"),
                                c(length(train_no_covid), covid_obs_num))),
       aes(x = date, y = val, color = imputed, group = 1)) +
  geom_line() +
  scale_color_manual(values = c("nie" = "black", "tak" = "orange"),
                     name = "imputacja") +
  labs(x = "Data", y = "Liczba pasażerów",
       title = "Zbiór treningowy po imputacji danych z okresu epidemii") +
  theme_light()