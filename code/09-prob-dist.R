## ----------------------------------------------------------------
#| label: binom-1
## 1. P(X = 6)
dbinom(x = 6, size = 15, prob = 0.2)


## ----------------------------------------------------------------
#| label: binom-2
## 2. P(X >= 6) = 1 - P(X <= 5)
1 - pbinom(q = 5, size = 15, prob = 0.2)


## ----------------------------------------------------------------
#| label: binom-3
## 2. P(X >= 6) = P(X > 5)
pbinom(q = 5, size = 15, prob = 0.2, lower.tail = FALSE)


## ----------------------------------------------------------------
#| label: binom-plot
#| fig-align: center
#| echo: !expr c(-1)
par(mar = c(4, 4, 2, 0), mgp = c(2.7, 1, 0), las = 1)
plot(x = 0:15, y = dbinom(0:15, size = 15, prob = 0.2),
     type = 'h', xlab = "x", ylab = "P(X = x)",
     lwd = 5, main = "Binomial(15, 0.2)")


## ----------------------------------------------------------------
#| label: pois-1
# 1.
(lam <- 4200 / 365)

## 2. P(X = 10)
dpois(x = 10, lambda = lam)


## ----pois-1------------------------------------------------------
#| label: pois-2

## 3.
## P(X > 10) = 1 - P(X <= 10)
1 - ppois(q = 10, lambda = lam)

## P(X > 10)
ppois(q = 10, lambda = lam,
      lower.tail = FALSE)


## ----------------------------------------------------------------
#| label: pois-plot
#| echo: !expr c(-1)
par(mar = c(4, 4, 2, 0))
plot(0:24, dpois(0:24, lambda = lam), type = 'h',
     lwd = 5, ylab = "P(X = x)", xlab = "x",
     main = "Poisson(11.5)")


## ----------------------------------------------------------------
#| echo: true
pnorm(1, mean = 0, sd = 1)
pnorm(1300, mean = 1100, sd = 200)


## ----------------------------------------------------------------
#| echo: true
1 - pnorm(0.45)


## ----------------------------------------------------------------
#| echo: true
1 - pnorm(1190, mean = 1100, sd = 200)


## ----------------------------------------------------------------
#| echo: true
1 - pnorm(0.45)


## ----------------------------------------------------------------
#| echo: true
pnorm(1190, mean = 1100, sd = 200, lower.tail = FALSE)


## ----------------------------------------------------------------
#| echo: true
(z_95 <- qnorm(0.95))


## ----------------------------------------------------------------
#| echo: true
(x_95 <- 1100 + z_95 * 200)


## ----------------------------------------------------------------
#| echo: true
qnorm(p = 0.95, mean = 1100, sd = 200)


## ----------------------------------------------------------------
#| echo: true
#| label: normal_approximation_to_binommial
n <- 100  # number of trials
p <- 0.2  # probability of being licensed for sale

## 1. Exact Binomial Probability P(X >= 15) = 1 - P(X < 14)
1 - pbinom(q = 14, size = n, prob = p)

## 2. Normal approximation with Continuity Correction
## P(X >= 14.5) = 1 - P(X < 14.5)
1 - pnorm(q = 14.5, mean = n * p, sd = sqrt(n * p * (1 - p)))

## 3. Normal approximation with NO Continuity Correction
## P(X >= 15) = 1 - P(X < 15)
1 - pnorm(q = 15, mean = n * p, sd = sqrt(n * p * (1 - p)))

