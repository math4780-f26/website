## ---------------------------------------------------------------------------------------
#| echo: true

# Load the data set
load("./pair_data.RDS")
pair_data

## Create the difference data d
(d <- pair_data$before - pair_data$after)

## sample mean of d
(d_bar <- mean(d))


## ---------------------------------------------------------------------------------------
#| echo: true

## sample standard deviation of d
(s_d <- sd(d))

## t test statistic
(t_test <- d_bar/(s_d/sqrt(length(d))))

## t critical value

qt(p = 0.95, df = length(d) - 1)

## p value
pt(q = t_test, df = length(d) - 1, 
   lower.tail = FALSE)


## ---------------------------------------------------------------------------------------
#| echo: true

## 95% confidence interval for the mean difference of the paired data
d_bar + c(-1, 1) * qt(p = 0.975, df = length(d) - 1) * (s_d / sqrt(length(d)))


## ---------------------------------------------------------------------------------------
#| echo: true

## t.test() function
t.test(x = pair_data$before, y = pair_data$after,
       alternative = "greater", mu = 0, paired = TRUE)


## ---------------------------------------------------------------------------------------
#| echo: true

## Prepare needed variables 
n1 = 33; x1_bar = 25.2; s1 = 8.6
n2 = 12; x2_bar = 33.9; s2 = 17.4
A <- s1^2 / n1; B <- s2^2 / n2
df <- (A + B)^2 / (A^2/(n1-1) + B^2/(n2-1))

## Use floor() function to round down to the nearest integer.
(df <- floor(df))
## t_test
(t_test <- (x1_bar - x2_bar) / sqrt(s1^2/n1 + s2^2/n2))
## t_cv
qt(p = 0.05, df = df)
## p_value
pt(q = t_test, df = df)



## ---------------------------------------------------------------------------------------
#| echo: true

## Prepare values
n1 = 10; x1_bar = 2.1; s1 = 0.5
n2 = 10; x2_bar = 4.2; s2 = 0.7

## pooled sample standard deviation
sp <- sqrt(((n1 - 1) * s1 ^ 2 + (n2 - 1) * s2 ^ 2) / (n1 + n2 - 2))

## degrees of freedom
df <- n1 + n2 - 2

## t_test
(t_test <- (x1_bar - x2_bar) / (sp * sqrt(1 / n1 + 1 / n2)))

## t_cv
qt(p = 0.05, df = df)

## p_value
pt(q = t_test, df = df)

