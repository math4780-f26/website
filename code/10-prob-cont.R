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

