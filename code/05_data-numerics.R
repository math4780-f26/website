## -----------------------------------------------------------------------------
int_rate <- round(loan50$interest_rate, 1)


## -----------------------------------------------------------------------------
#| echo: true
mean(int_rate)


## -----------------------------------------------------------------------------
#| echo: true
## Compute the median using definition
(sort_rate <- sort(int_rate))  ## sort data
length(int_rate)  ## Check sample size is odd or even
(sort_rate[25] + sort_rate[26]) / 2  ## Verify the answer


## -----------------------------------------------------------------------------
#| echo: true
(int_rate[25] + int_rate[26]) / 2


## -----------------------------------------------------------------------------
#| echo: true

## Compute the median using command median()
median(int_rate)


## -----------------------------------------------------------------------------
#| echo: !expr c(1, 2)
## Create a frequency table
(table_data <- table(int_rate))
# sort_table_data <- sort(table_data, decreasing = TRUE)
# sort_table_data
# print(paste("The mode is",names(sort_table_data)[1]))


## -----------------------------------------------------------------------------
#| echo: true

sort(table_data, decreasing = TRUE)


## -----------------------------------------------------------------------------
#| echo: !expr c(3)
data_extreme <- int_rate; data_extreme[1] <- 90 ## replace the first 3 values with 3 large values
## In the original data, the maximum value is 42.
data_extreme


## -----------------------------------------------------------------------------
#| echo: true
mean(data_extreme)  ## Large mean! Original mean is 11.56
median(data_extreme)  ## Median does not change!
names(sort(table(data_extreme), decreasing = TRUE))[1] ## Mode does not change either!


## -----------------------------------------------------------------------------
#| echo: true

quantile(x = int_rate,
         probs = c(0.25, 0.5, 0.75))

## IQR by definition
quantile(x = int_rate, probs = 0.75) -
  quantile(x = int_rate, probs = 0.25)


## -----------------------------------------------------------------------------
#| echo: true

IQR(int_rate)


## -----------------------------------------------------------------------------
#| echo: true
summary(int_rate)


## -----------------------------------------------------------------------------
#| echo: true
var(int_rate)
sqrt(var(int_rate))
sd(int_rate)


## -----------------------------------------------------------------------------
#| echo: !expr c(3)
par(mar = c(0,4,0,0))
par(mfrow = c(1, 1))
boxplot(int_rate, ylab = "Interest Rate (%)")


## -----------------------------------------------------------------------------
#| echo: true
sort(int_rate, decreasing = TRUE)[1:5]
sort(int_rate)[1:5]
Q3 <- quantile(int_rate, probs = 0.75,
               names = FALSE)
Q1 <- quantile(int_rate, probs = 0.25,
               names = FALSE)
IQR <- Q3 - Q1
Q1 - 1.5 * IQR
Q3 + 1.5 * IQR

