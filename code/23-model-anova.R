## ---------------------------------------------------------------------------------------
# load("./data/table08-7.rdata")
# data <- `table08-7`
# data[1, 1] <- 3
# data_anova <- data.frame("y"=c(data[, 1], data[, 2], data[, 3]),
#                       "food"=rep(c("cereals", "energy", "veggie"), each = 5))
# data_anova[1, 1] <- 3
data_anova <- read.csv("./data_anova.csv")


## ---------------------------------------------------------------------------------------
#| echo: true
data_anova


## ---------------------------------------------------------------------------------------
#| label: fig-qqplots
#| fig-cap: QQ plots for each type of food
#| fig-asp: 0.35
# par(mgp = c(2, 1, 0))
# par(mar = c(3.5, 3.5, 1.5, 0))
car::qqPlot(y ~ food, data = data_anova, layout = c(1, 3), las = 1)


## ---------------------------------------------------------------------------------------
#| echo: true
anova(lm(formula = y ~ food, data = data_anova))
anova(lm(formula = data_anova$y ~ data_anova$food))


## ---------------------------------------------------------------------------------------
#| echo: true
summary(aov(y ~ food, data = data_anova))

oneway.test(y ~ food, data = data_anova, var.equal = TRUE)


## ----fig.height=6, fig.width=10---------------------------------------------------------
# load("../../intro_stats/OTT_Final/R_WORKSPACE/CH08/table08-16.rdata")
# data_8_16 <- `table08-16`  ## This is a data frame
# # original_y <- df_heter_var$y
# data_oxygen <- data_8_16[, 2:5]
# data_oxygen

data_oxygen <- read.csv("./data_oxygen.csv")

## ---------------------------------------------------------------------------------------
#| echo: true
#| eval: false
## boxplot(data_oxygen)


## ---------------------------------------------------------------------------------------
#| echo: !expr c(2)
# data_oxygen_tidy <- data.frame(oxygen = c(data_8_16[, 2], data_8_16[, 3], 
#                                         data_8_16[, 4], data_8_16[, 5]),
#                              km = c(rep("1", 10), rep("5", 10), 
#                                     rep("10", 10), rep("20", 10)))
data_oxygen_tidy <- read.csv("./data_oxygen_tidy.csv")

## ---------------------------------------------------------------------------------------
#| echo: true
library(car)
leveneTest(oxygen ~ km, data = data_oxygen_tidy)


## ---------------------------------------------------------------------------------------
#| echo: true
colMeans(data_oxygen)


## ---------------------------------------------------------------------------------------
#| echo: true
# data_oxygen_tidy
## fit a linear model to get fitted values and residuals
oxygen_fit <- lm(oxygen ~ km, data = data_oxygen_tidy)
oxygen_fit$fitted.values


## ---------------------------------------------------------------------------------------
#| echo: true
fitted_val <- matrix(oxygen_fit$fitted.values, 10, 4)

## use the same column name as the original data
colnames(fitted_val) <- paste0(c(1, 5, 10, 20), "KM")

## fitted value data
as.data.frame(fitted_val)


## ---------------------------------------------------------------------------------------
#| echo: true
data_oxygen_tidy[, 1] - oxygen_fit$fitted.values


## ---------------------------------------------------------------------------------------
#| echo: true
oxygen_fit$residuals


## ----fig.height=5-----------------------------------------------------------------------
qqPlot(oxygen_fit$residuals, pch = 19, id = FALSE, ylab = "residuals", 
       main = "Normal Probability Plot for Residuals")


## ---------------------------------------------------------------------------------------
shapiro.test(oxygen_fit$residuals)
library(nortest)
nortest::ad.test(oxygen_fit$residuals)


## ----fig.height=4-----------------------------------------------------------------------
plot(oxygen_fit$fitted.values, oxygen_fit$residuals, xlab = "Fitted Value",
     ylab = "Residual", main = "Versus Fits", pch = 19, col = "red",
     xlim = c(0, 35))
abline(h = 0)


## ---------------------------------------------------------------------------------------
#| echo: true
data_oxygen_tidy_trans <- data_oxygen_tidy
data_oxygen_tidy_trans[, 1] <- sqrt(data_oxygen_tidy[, 1] + 0.375)


## ---------------------------------------------------------------------------------------
#| echo: true
oxygen_fit_trans <- lm(oxygen ~ km, data = data_oxygen_tidy_trans)
anova(oxygen_fit_trans)


## ---------------------------------------------------------------------------------------
#| fig-height: 5
#| fig-width: 11.5
par(mfrow = c(1, 2))
qqPlot(oxygen_fit_trans$residuals, pch = 19, id = FALSE, ylab = "residuals", 
       main = "Normal Probability Plot for Residuals (transformed y)")
plot(oxygen_fit_trans$fitted.values, oxygen_fit_trans$residuals, xlab = "Fitted Value",
     ylab = "Residual", main = "Versus Fits (transformed y)", pch = 19, col = "red",
     xlim = c(0, 6))
abline(h = 0)


## ---------------------------------------------------------------------------------------
shapiro.test(oxygen_fit_trans$residuals)
ad.test(oxygen_fit_trans$residuals)


## ---------------------------------------------------------------------------------------
#| echo: true
anova(oxygen_fit)


## ---------------------------------------------------------------------------------------
#| echo: true
str(airquality)
head(airquality)
## remove observations with missing values using complete.cases()
air_data <- airquality[complete.cases(airquality), ] 


## ---------------------------------------------------------------------------------------
#| label: normality-air
#| fig-height: 3
#| fig-width: 12
#| echo: true

car::qqPlot(Ozone ~ Month, data = air_data, layout=c(1, 5))


## ---------------------------------------------------------------------------------------
#| label: normality-test
shapiro.test(air_data[air_data$Month == 5, 1])
shapiro.test(air_data[air_data$Month == 9, 1])


## ---------------------------------------------------------------------------------------
#| echo: true
#| eval: true

ad.test(air_data[air_data$Month == 5, 1])
ad.test(air_data[air_data$Month == 9, 1])


## ---------------------------------------------------------------------------------------
ks.test(air_data[air_data$Month == 5, 1], "pnorm")
ks.test(air_data[air_data$Month == 9, 1], "pnorm")


## ---------------------------------------------------------------------------------------
#| label: kruskal-wallis-air
#| echo: true
kruskal.test(formula = Ozone ~ Month, data = air_data) 

