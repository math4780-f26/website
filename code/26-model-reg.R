## ---------------------------------------------------------------------------------------
#| echo: true
plot(x = mtcars$wt, y = mtcars$mpg, 
     main = "MPG vs. Weight", 
     xlab = "Car Weight", 
     ylab = "Miles Per Gallon", 
     pch = 16, col = 4, las = 1)


## ---------------------------------------------------------------------------------------
#| echo: true
cor(x = mtcars$wt,
    y = mtcars$mpg)


## ---------------------------------------------------------------------------------------
library(ggplot2)
mpg


## ---------------------------------------------------------------------------------------
#| echo: true
reg_fit <- lm(formula = hwy ~ displ, data = mpg)
reg_fit


## ---------------------------------------------------------------------------------------
#| echo: true
typeof(reg_fit)
## all elements in reg_fit
names(reg_fit)
## use $ to extract an element of a list
reg_fit$coefficients


## ---------------------------------------------------------------------------------------
#| echo: true
## the first 5 observed response value y
mpg$hwy[1:5]
## the first 5 fitted value y_hat
head(reg_fit$fitted.values, 5)
## the first 5 predictor value x
mpg$displ[1:5]
length(reg_fit$fitted.values)


## ---------------------------------------------------------------------------------------
#| echo: !expr -c(1)
par(mar = c(3, 3, 2, 0), mgp = c(2, 0.5, 0))
plot(x = mpg$displ, y = mpg$hwy, las = 1, pch = 19, col = "navy", cex = 0.5,
     xlab = "Displacement (litres)", ylab = "Highway MPG",
     main = "Highway MPG vs. Engine Displacement (litres)")
abline(reg_fit, col = "#FFCC00", lwd = 3)


## ---------------------------------------------------------------------------------------
#| echo: true
#| output.lines: !expr c(15:16)
(summ_reg_fit <- summary(reg_fit))


## ---------------------------------------------------------------------------------------
#| echo: true

# lots of fitted information saved in summary(reg_fit)!
names(summ_reg_fit)
# residual standard error (sigma_hat)
summ_reg_fit$sigma


## ---------------------------------------------------------------------------------------
#| echo: true
# from reg_fit
sqrt(sum(reg_fit$residuals^2) / reg_fit$df.residual)


## ---------------------------------------------------------------------------------------
#| echo: true
confint(reg_fit, level = 0.95)


## ---------------------------------------------------------------------------------------
#| echo: true
#| output.lines: !expr c(9, 10, 11, 12)
summ_reg_fit

## ---------------------------------------------------------------------------------------
#| echo: true
summ_reg_fit$coefficients


## ---------------------------------------------------------------------------------------
#| echo: true
anova(reg_fit)

## ---------------------------------------------------------------------------------------
#| echo: true
summ_reg_fit$coefficients
summ_reg_fit$coefficients[2, 3] ^ 2


## ---------------------------------------------------------------------------------------
#| echo: true
#| output.lines: !expr c(16, 17)
summ_reg_fit

## ---------------------------------------------------------------------------------------
#| echo: true
summ_reg_fit$r.squared


## ---------------------------------------------------------------------------------------
#| echo: true
## CI for the mean response
predict(reg_fit, newdata = data.frame(displ = 5.5), interval = "confidence", level = 0.95)
## PI for the new observation
predict(reg_fit, newdata = data.frame(displ = 5.5), interval = "predict", level = 0.95)


## ---------------------------------------------------------------------------------------
par(mar = c(3, 3, 0, 0), mgp = c(2, 0.5, 0), mfrow = c(1, 1))
plot(x = mpg$displ, y = mpg$hwy, las = 1, pch = 19, cex = 0.5, ylim = c(4, 45),
     xlab = "Displacement (litres)", ylab = "Highway MPG")
newx <- seq(min(mpg$displ), max(mpg$displ), by = 0.1)
ci <- predict(reg_fit, newdata = data.frame(displ = newx), interval = "confidence", level = 0.95)
pi <- predict(reg_fit, newdata = data.frame(displ = newx), interval = "prediction", level = 0.95)
lines(newx, ci[, 1], col = "#003366", lwd = 2)
matlines(newx, ci[, 2:3], col = "red", lty = 1, lwd = 2)
matlines(newx, pi[, 2:3], col = "blue", lty = 1, lwd = 2)
abline(v = mean(mpg$displ))
segments(x0 = 5.52, y0 = 15.35839, y1 = 17.20043, col = "red", lwd = 3)
segments(x0 = 5.48, y0 = 8.665682, y1 = 23.89314, col = "blue", lwd = 3)
legend("topright", c("Regression line", "CI", "PI"),
       lty = c(1, 1, 1), lwd = c(3, 3, 3), col = c("#003366", "red", "blue"), bty = "n")


## ---------------------------------------------------------------------------------------
#| echo: true
#| message: false
library(boot)
set.seed(2024)
coef_fn <- function(data, index) {
    coef(lm(hwy ~ displ, data = data, subset = index))
}

boot_lm <- boot::boot(data = mpg, statistic = coef_fn, R = 100)
boot_lm


## ---------------------------------------------------------------------------------------
#| echo: true
boot_lm$t0
head(boot_lm$t)


## ---------------------------------------------------------------------------------------
#| out-width: 100%
par(mfrow = c(1, 2), mar = c(4, 4, 2, 0.5))
hist(boot_lm$t[, 1], main = "Bootstrap Dist: Intercept", las = 1,
     xlab = "Intercept")
hist(boot_lm$t[, 2], main = "Bootstrap Dist: Slope", las = 1,
     xlab = "Slope")


## ---------------------------------------------------------------------------------------
#| echo: true
boot.ci(boot.out = boot_lm, index = 1, 
        type = c("norm","basic", "perc"))
boot.ci(boot.out = boot_lm, index = 2,
        type = c("norm","basic", "perc"))


## ---------------------------------------------------------------------------------------
mpg[, c("hwy", "trans")]


## ---------------------------------------------------------------------------------------
mpg_trans <- mpg
mpg_trans$trans[grepl("auto", mpg_trans$trans)] <- "auto"
mpg_trans$trans[grepl("manual", mpg_trans$trans)] <- "manual"


## ---------------------------------------------------------------------------------------
#| echo: true
mpg_trans[, c("hwy", "trans")]


## ---------------------------------------------------------------------------------------
#| echo: true
typeof(mpg_trans$trans)


## ---------------------------------------------------------------------------------------
#| echo: true
lm(hwy ~ trans, data = mpg_trans)
