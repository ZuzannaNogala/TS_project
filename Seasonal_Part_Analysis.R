# Analiza sezonowości

# Oryginalne dane
ggplot(model_data, aes(x = time, y = values)) +
  geom_line(data = cbind(model_data[sort(bound_obs),],
                         group = rep(1:2, each = 2)),
            aes(group = group), linetype = "dashed") +
  geom_line(aes(color = okres)) +
  ggtitle("Przewozy pasażerskie w transporcie lotniczym według kraju sprawozdającego - Polska")


# Porównanie rozkładów liczby pasażerów dla poszczególnych miesięcy przed i po epidemii
# Problem duża wariancja pomiędzy danymi starymi (2004 - 2010) a nowymi (2017+) przed pandemią
model_data %>%
  filter(okres != "w trakcie epidemii") %>%
  mutate(miesiac = format(as.Date(time), "%m"),
         okres = factor(okres, 
                        levels = c("przed epidemią", "po epidemii"),
                        labels = c("Okres przed epidemią (2004-2020)", 
                                   "Okres po epidemii (2022 - 2025)"))) %>%
  ggplot(aes(x = miesiac, y = values, fill = okres)) +
  geom_boxplot() +
  labs(title = "Sezonowość przewozów: Przed vs Po COVID-19",
       x = "Miesiąc",
       y = "Liczba pasażerów",
       fill = "Okres") +
  facet_wrap(~okres, ncol = 2) +
  theme(legend.position = "none") +
  ylim(c(0, 7500000)) +
  labs(title = "Rozkład miesięczny liczby pasażerów")


# Porównanie rozkładów liczby pasażerów dla poszczególnych miesięcy przed i po epidemii na okresach 3 letnich
# Okres 3 letni przed covidem vs okres 3 letni po covidzie

# około 3 lata i 3 miesiące (38 i 39 miesięcy, odpowiednio)
# model_data %>% filter(okres == "przed epidemią" & time >= "2017-01-01") %>% nrow() / 12 
# model_data %>% filter(okres == "po epidemii") %>% nrow() / 12

model_data %>%
  filter(okres != "w trakcie epidemii" & time >= "2017-01-01") %>%
  mutate(miesiac = format(as.Date(time), "%m"),
         okres = factor(okres, 
                        levels = c("przed epidemią", "po epidemii"),
                        labels = c("Okres 3 letni przed epidemią (2017-2020)", 
                                   "Okres 3 letni po epidemii (2022 - 2025)"))) %>%
  ggplot(aes(x = miesiac, y = values, fill = okres)) +
  geom_boxplot() +
  labs(title = "Sezonowość przewozów: Przed vs Po COVID-19",
       x = "Miesiąc",
       y = "Liczba pasażerów",
       fill = "Okres") +
  facet_wrap(~okres, ncol = 2) +
  ylim(c(0, 7500000)) +
  theme(legend.position = "none") +
  labs(title = "Rozkład miesięczny liczby pasażerów na okresie 3 lat przed i po covidzie")


# Porównanie rozkładów iczby pasażerów dla poszczególnych miesięcy przed i po epidemii na okresach 3 letnich
# na jednym wykresie
model_data %>%
  filter(okres != "w trakcie epidemii" & time >= "2017-01-01") %>%
  mutate(miesiac = format(as.Date(time), "%m"),
         okres = factor(okres, 
                        levels = c("przed epidemią", "po epidemii"),
                        labels = c("Okres 3 letni przed epidemią (2017-2020)", 
                                   "Okres 3 letni po epidemii (2022 - 2025)"))) %>%
  ggplot(aes(x = miesiac, y = values, fill = okres)) +
  geom_boxplot() +
  labs(title = "Sezonowość przewozów: Przed vs Po COVID-19",
       x = "Miesiąc",
       y = "Liczba pasażerów",
       fill = "Okres") +
  theme(legend.position = "bottom") +
  ylim(c(0, 7500000)) +
  labs(title = "Rozkład miesięczny liczby pasażerów na okresie 3 lat przed i po covidzie")





