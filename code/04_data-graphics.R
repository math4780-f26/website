## -----------------------------------------------------------------------------
#| echo: true

# install.packages("openintro")
library(openintro)
str(loan50)


## -----------------------------------------------------------------------------
#| echo: true
(x <- loan50$homeownership)


## -----------------------------------------------------------------------------
#| echo: true
## frequency table
table(x)


## -----------------------------------------------------------------------------
#| echo: true
freq <- table(x)
rel_freq <- freq / sum(freq)
cbind(freq, rel_freq)


## -----------------------------------------------------------------------------
#| echo: !expr -c(1)
par(mar = c(4, 4, 2, 1))
barplot(height = table(x), main = "Bar Chart", xlab = "Homeownership")


## ----echo=2-------------------------------------------------------------------
#| echo: !expr -c(1)
par(mar = c(0, 0, 1, 0))
pie(x = table(x), main = "Pie Chart")


## -----------------------------------------------------------------------------
table(c(2.3, 4.5, 4.6, 4.8, 2.8, 5.9))



## -----------------------------------------------------------------------------
#| echo: true
int_rate <- round(loan50$interest_rate, 1)
int_rate


## -----------------------------------------------------------------------------
k <- 9
class_width <- 2.5
lower_limit <- 5

class_boundary <- lower_limit + 0:k * class_width
class_int <- paste(paste0(class_boundary[1:k], "%"),
                   paste0(class_boundary[2:(k+1)], "%"), 
                   sep = "-")

freq_info <- hist(int_rate, 
                  breaks = class_boundary, 
                  plot = FALSE)
freq_dist <- data.frame("Class" = as.character(1:k), 
                        "Class_Intvl" = class_int, 
                        "Freq" = freq_info$counts, 
                        "Rel_Freq" = round(freq_info$counts / length(int_rate), 2))
print(freq_dist, row.names = FALSE)


## -----------------------------------------------------------------------------
#| echo: true
# min and max value
range(int_rate)


## -----------------------------------------------------------------------------
hist(x = int_rate, 
     xlab = "Interest Rate (%)",
     main = "Hist. of Int. Rate (Defualt)")


## -----------------------------------------------------------------------------
#| echo: true
class_boundary
hist(x = int_rate, 
     breaks = class_boundary, #<<
     xlab = "Interest Rate (%)",
     main = "Hist. of Int. Rate (Ours)")


## -----------------------------------------------------------------------------
#| label: fig-histogram
#| fig-cap: Interest Rate Histogram
par(mar = c(4, 4, 2, 1))
par(mfrow = c(1, 1))
hist(x = int_rate, breaks = class_boundary, xlab = "Interest Rate (%)", las = 1,
     main = "Histogram of Interest Rate")


## -----------------------------------------------------------------------------
plot(x = loan50$total_income, y = loan50$loan_amount,
     xlab = "Total Income", ylab = "Loan Amount",
     pch = 16, col = 4)

