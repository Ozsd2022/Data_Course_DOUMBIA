# Assignment 8: mushroom growth modeling
# Open Assignment_8.Rproj, then Source this script. Only base R is needed.
# Paths are relative to the Assignment_8 project directory.
mushrooms <- read.csv("Data/mushroom_growth.csv")
mushrooms$Species <- factor(mushrooms$Species)
mushrooms$Humidity <- factor(mushrooms$Humidity)
stopifnot(!anyNA(mushrooms))
dir.create("Results", showWarnings = FALSE)

# Explore all five predictors. Curvature is especially visible for nitrogen.
pdf("Results/exploratory_plots.pdf", width = 10, height = 7)
par(mfrow = c(2, 3))
for (v in c("Light", "Nitrogen", "Temperature")) {
  plot(mushrooms[[v]], mushrooms$GrowthRate, xlab = v,
       ylab = "GrowthRate", pch = 19, col = "steelblue")
  means <- aggregate(mushrooms$GrowthRate, list(mushrooms[[v]]), mean)
  lines(means[[1]], means[[2]], col = "red", lwd = 2)
}
boxplot(GrowthRate ~ Species, data = mushrooms, ylab = "GrowthRate")
boxplot(GrowthRate ~ Humidity, data = mushrooms, ylab = "GrowthRate")
dev.off()

# lm() is linear in its coefficients; squared predictors allow curved fits.
models <- list(
  Light_only = lm(GrowthRate ~ Light, data = mushrooms),
  Additive = lm(GrowthRate ~ Light + Nitrogen + Temperature + Species + Humidity,
                data = mushrooms),
  Quadratic = lm(GrowthRate ~ Light + I(Light^2) + Nitrogen + I(Nitrogen^2) +
                   Temperature + Species + Humidity, data = mushrooms),
  Quadratic_interactions = lm(GrowthRate ~ Light + I(Light^2) + Nitrogen +
    I(Nitrogen^2) + Temperature + Species + Humidity +
    Species:Light + Species:Nitrogen, data = mushrooms)
)
# Training MSE is the selection criterion requested in this assignment.
# It does not establish how well a model predicts unseen observations.
comparison <- data.frame(
  Model = names(models),
  MSE = sapply(models, function(m) mean(residuals(m)^2)),
  Adjusted_R2 = sapply(models, function(m) summary(m)$adj.r.squared),
  AIC = sapply(models, AIC)
)
print(comparison)
write.csv(comparison, "Results/model_comparison.csv", row.names = FALSE)
best_name <- comparison$Model[which.min(comparison$MSE)]
best_model <- models[[best_name]]
print(best_name)
print(summary(best_model))
capture.output(summary(best_model), file = "Results/best_model_summary.txt")

# Predictions hold other variables fixed to make a meaningful comparison.
# Nitrogen above 45 is extrapolation, included to show model limitations.
new_data <- data.frame(
  Light = 10, Nitrogen = c(0, 10, 20, 30, 40, 45, 60, 100),
  Temperature = 20,
  Species = factor("P.ostreotus", levels = levels(mushrooms$Species)),
  Humidity = factor("High", levels = levels(mushrooms$Humidity))
)
new_data$PredictedGrowthRate <- predict(best_model, newdata = new_data)
new_data$Extrapolated <- new_data$Nitrogen > max(mushrooms$Nitrogen)
print(new_data)
write.csv(new_data, "Results/hypothetical_predictions.csv", row.names = FALSE)
# Compare with observed data at exactly the same other predictor values.
observed <- subset(mushrooms, Light == 10 & Temperature == 20 &
                     Species == "P.ostreotus" & Humidity == "High")
grid <- new_data[rep(1, 201), ]
grid$Nitrogen <- seq(0, 100, length.out = 201)
grid$PredictedGrowthRate <- predict(best_model, newdata = grid)
pdf("Results/predictions.pdf", width = 9, height = 6)
plot(grid$Nitrogen, grid$PredictedGrowthRate, type = "l", col = "steelblue",
     xlab = "Nitrogen", ylab = "GrowthRate",
     ylim = range(c(grid$PredictedGrowthRate, observed$GrowthRate)),
     main = "P.ostreotus; High humidity; Light 10; Temperature 20")
points(observed$Nitrogen, observed$GrowthRate, pch = 19)
points(new_data$Nitrogen, new_data$PredictedGrowthRate, pch = 17, col = "red")
abline(h = 0, lty = 3)
abline(v = 45, lty = 2)
legend("bottomleft", c("Observed", "Hypothetical prediction", "Model curve"),
       col = c("black", "red", "steelblue"), pch = c(19, 17, NA),
       lty = c(NA, NA, 1), bty = "n")
dev.off()
pdf("Results/model_diagnostics.pdf")
par(mfrow = c(2, 2))
plot(best_model)
dev.off()

# Canvas question 3: transform the predictor, keeping lm linear in coefficients.
nonlinear <- read.csv("Data/non_linear_relationship.csv")
nonlinear_model <- lm(response ~ I(exp(predictor)), data = nonlinear)
print(summary(nonlinear_model))
pdf("Results/nonlinear_model.pdf")
plot(response ~ predictor, data = nonlinear, pch = 19, col = "gray60")
x <- data.frame(predictor = seq(min(nonlinear$predictor),
                              max(nonlinear$predictor), length.out = 200))
lines(x$predictor, predict(nonlinear_model, newdata = x), col = "blue", lwd = 2)
dev.off()
