## ---------------------------------------------------------------------------------------
#| echo: true

## set values
n <- 16
s2 <- var(heights)
alpha <- 0.05

## two chi-square critical values
chi2_right <- qchisq(p = alpha/2, df = n - 1, lower.tail = FALSE)
chi2_left <- qchisq(p = alpha/2, df = n - 1, lower.tail = TRUE)

## two bounds of CI for sigma2
ci_sig2_lower <- (n - 1) * s2 / chi2_right
ci_sig2_upper <- (n - 1) * s2 / chi2_left

## two bounds of CI for sigma
(ci_sig_lower <- sqrt(ci_sig2_lower))
(ci_sig_upper <- sqrt(ci_sig2_upper))



## ---------------------------------------------------------------------------------------
#| echo: true
#| label: f-test-two-sigma-ci

## set values
n1 <- 10; n2 <- 10
s1 <- 0.5; s2 <- 0.7
alpha <- 0.05

## 95% CI for sigma_1^2 / sigma_2^2
f_small <- qf(p = alpha / 2, df1 = n1 - 1, df2 = n2 - 1,lower.tail = TRUE)
f_big <- qf(p = alpha / 2, df1 = n1 - 1, df2 = n2 - 1, lower.tail = FALSE)

## lower bound
(s1 ^ 2 / s2 ^ 2) / f_big

## upper bound
(s1 ^ 2 / s2 ^ 2) / f_small


## ---------------------------------------------------------------------------------------
#| echo: true
#| label: f-test-two-sigma-test
## Testing sigma_1 = sigma_2
(test_stats <- s1 ^ 2 / s2 ^ 2)


## ---------------------------------------------------------------------------------------
y1 <- c(4.2, 2.9, 0.2, 25.7, 6.3, 7.2, 2.3, 9.9, 5.3, 6.5)
y2 <- c(0.2, 11.3, 0.3, 17.1, 51, 10.1, 0.3, 0.6, 7.9, 7.2)
y3 <- c(7.2, 6.4, 9.9, 3.5, 10.6, 10.8, 10.6, 8.4, 6.0, 11.9)
# data_ex78 <- data.frame("mpg_increase" = c(y1, y2, y3), "additive" = factor(rep(1:3, each = 10)))
data_additive <- data.frame("mpg_increase" = c(y1, y2, y3), "additive" = factor(rep(1:3, each = 10)))


## ---------------------------------------------------------------------------------------
#| label: additive-boxplot
#| echo: false
#| fig-height: 3.6
#| fig-width: 10
boxplot(mpg_increase~additive, data = data_additive, axes = FALSE, ylab = "",
        main = "Boxplots for three additives", horizontal = TRUE)
axis(1)
axis(2, las = 2, at = 1:3, labels = paste("Additive", 1:3))


## ---------------------------------------------------------------------------------------
#| echo: true
#| message: false
library(car)
leveneTest(y = data_additive$mpg_increase, 
           group = data_additive$additive)

