unicef <- read.csv("unicef-u5mr.csv")

unicef_tidy <- tidyr::pivot_longer(
  unicef,
  cols = tidyselect::starts_with("U5MR."),
  names_to = "Year",
  names_prefix = "U5MR\\.",
  names_transform = list(Year = as.integer),
  values_to = "U5MR"
)

print(head(unicef_tidy))
library(ggplot2)

# Garder les observations dont le taux est disponible
unicef_observed <- subset(unicef_tidy, !is.na(U5MR))

# Graphique 1 : une ligne par pays, un panneau par continent
plot_1 <- ggplot(
  unicef_observed,
  aes(x = Year, y = U5MR, group = CountryName)
) +
  geom_line(alpha = 0.4) +
  facet_wrap(~ Continent) +
  labs(
    title = "Under-5 mortality by country",
    x = "Year",
    y = "Deaths per 1,000 live births"
  ) +
  theme_minimal()

ggsave("Doumbia_Plot_1.png", plot = plot_1, width = 10, height = 6)

# Graphique 2 : moyenne des pays par continent et par année
continent_means <- aggregate(
  U5MR ~ Continent + Year,
  data = unicef_observed,
  FUN = mean
)

plot_2 <- ggplot(
  continent_means,
  aes(x = Year, y = U5MR, color = Continent)
) +
  geom_line() +
  labs(
    title = "Mean under-5 mortality by continent",
    x = "Year",
    y = "Mean deaths per 1,000 live births"
  ) +
  theme_minimal()

ggsave("Doumbia_Plot_2.png", plot = plot_2, width = 9, height = 5)

print(plot_2)
# Modèle 1 : une seule tendance pour tous les continents
mod1 <- lm(U5MR ~ Year, data = unicef_observed)

# Modèle 2 : une pente commune et un niveau différent par continent
mod2 <- lm(U5MR ~ Year + Continent, data = unicef_observed)

# Modèle 3 : une pente et un niveau propres à chaque continent
mod3 <- lm(U5MR ~ Year * Continent, data = unicef_observed)

# Comparer les modèles sur les mêmes observations.
# Un AIC plus bas indique un meilleur compromis entre ajustement et complexité.
comparison <- AIC(mod1, mod2, mod3)
print(comparison)

# Prédire pour chaque année et chaque continent
prediction_grid <- expand.grid(
  Year = sort(unique(unicef_observed$Year)),
  Continent = sort(unique(unicef_observed$Continent))
)

predictions <- rbind(
  transform(prediction_grid, Model = "Model 1",
            Predicted = predict(mod1, newdata = prediction_grid)),
  transform(prediction_grid, Model = "Model 2",
            Predicted = predict(mod2, newdata = prediction_grid)),
  transform(prediction_grid, Model = "Model 3",
            Predicted = predict(mod3, newdata = prediction_grid))
)

plot_models <- ggplot(
  predictions,
  aes(x = Year, y = Predicted, color = Model)
) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Continent) +
  labs(
    title = "Predicted under-5 mortality by model",
    x = "Year",
    y = "Predicted deaths per 1,000 live births"
  ) +
  theme_minimal()

print(plot_models)
# I prefer mod3 because it has the lowest AIC of the three models.
# The Year-by-Continent interaction allows the slope to vary by continent.
ecuador_2020 <- data.frame(Year = 2020, Continent = "Americas")
ecuador_pred <- predict(mod3, newdata = ecuador_2020)

print(data.frame(
  predicted = round(ecuador_pred, 2),
  observed = 13,
  absolute_error = round(abs(ecuador_pred - 13), 2)
))
# Bonus: fit a log-linear trend to Ecuador's observed rates.
ecuador_data <- subset(unicef_observed, CountryName == "Ecuador" & U5MR > 0)
mod4 <- lm(log(U5MR) ~ Year, data = ecuador_data)
ecuador_pred4 <- exp(predict(mod4, newdata = ecuador_2020))

print(data.frame(
  model = c("mod3", "mod4"),
  predicted = round(c(ecuador_pred, ecuador_pred4), 2),
  observed = 13,
  absolute_error = round(abs(c(ecuador_pred, ecuador_pred4) - 13), 2)
))