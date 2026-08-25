## ---------------------------------------------------------------------------------------
#| echo: true
## create objects for any information we have
alpha <- 0.05; mu_0 <- 150; 
x_bar <- 147.2; s <- 5.5; n <- 25

## Test statistic
(t_test <- (x_bar - mu_0) / (s / sqrt(n))) 
## Critical value
(t_cri <- qt(alpha, df = n - 1)) 
## p-value
(p_val <- pt(t_test, df = n - 1)) 


## ---------------------------------------------------------------------------------------
#| echo: true
## create objects to be used
alpha <- 0.05; mu_0 <- 2.78; 
x_bar <- 2.8; sigma <- 0.1; n <- 25

## Test statistic
(z_test <- (x_bar - mu_0) / (sigma / sqrt(n))) 
## Critical value
(z_crit <- qnorm(alpha/2, lower.tail = FALSE)) 
## p-value
(p_val <- 2 * pnorm(z_test, lower.tail = FALSE)) 


## ---------------------------------------------------------------------------------------
#| echo: true
pnorm(-0.355)
pnorm(54.94, mean = 56, sd = 18/sqrt(36))

