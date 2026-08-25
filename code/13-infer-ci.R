## ----ci_normal, echo=TRUE, tidy=FALSE----------------------------
## save all information we have
alpha <- 0.05
n <- 16
x_bar <- 121.5
sig <- 5

## 95% CI
## z-critical value
(cri_z <- qnorm(p = alpha / 2, lower.tail = FALSE))  

## margin of error
(m_z <- cri_z * (sig / sqrt(n)))  

## 95% CI for mu when sigma is known
x_bar + c(-1, 1) * m_z  


## ----------------------------------------------------------------
#| eval: false
#| label: ci-mean-known-sig
#| echo: !expr c(-1)
par(mar = c(3.5, 3.5, 0, 0), mgp = c(2.5, 1, 0))
mu <- 120; sig <- 5
al <- 0.05; M <- 100; n <- 16

set.seed(2023)
x_rep <- replicate(M, rnorm(n, mu, sig))
xbar_rep <- apply(x_rep, 2, mean)
E <- qnorm(p = 1 - al / 2) * sig / sqrt(n)
ci_lwr <- xbar_rep - E
ci_upr <- xbar_rep + E

plot(NULL, xlim = range(c(ci_lwr, ci_upr)), ylim = c(0, 100),
     xlab = "95% CI", ylab = "Sample", las = 1)
mu_out <- (mu < ci_lwr | mu > ci_upr)
segments(x0 = ci_lwr, y0 = 1:M, x1 = ci_upr, col = "navy", lwd = 2)
segments(x0 = ci_lwr[mu_out], y0 = (1:M)[mu_out], x1 = ci_upr[mu_out],
         col = 2, lwd = 2)
abline(v = mu, col = "#FFCC00", lwd = 2)


## ----------------------------------------------------------------
#| echo: true
alpha <- 0.05
n <- 16
x_bar <- 121.5
s <- 5  ## sigma is unknown and s = 5

## t-critical value
(cri_t <- qt(p = alpha / 2, df = n - 1, lower.tail = FALSE)) 

## margin of error
(m_t <- cri_t * (s / sqrt(n)))  

## 95% CI for mu when sigma is unknown
x_bar + c(-1, 1) * m_t  

