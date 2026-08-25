
## ----arithmetic-example-------------------------------------------------------
#| echo: true
2 + 3 * 5 + 4
2 + 3 * (5 + 4)

## ----logical-example----------------------------------------------------------
#| echo: true
5 <= 5
5 <= 4
# Is 5 is NOT equal to 5?
5 != 5


## -----------------------------------------------------------------------------
#| echo: true
## Is TRUE not equal to FALSE?
TRUE != FALSE
## Is not TRUE equal to FALSE?
!TRUE == FALSE
## TRUE if either one is TRUE or both are TRUE
TRUE | FALSE


## ----builtin-fcns-------------------------------------------------------------
#| echo: true
sqrt(144)
exp(1)  ## Euler's number
sin(pi/2)
abs(-7)


## -----------------------------------------------------------------------------
#| echo: true
factorial(5)
## without specifying base value
## it is a natural log with base e
log(100)
## log function and we specify base = 2
log(100, base = 10)


## ----assignment---------------------------------------------------------------
#| echo: true
x <- 5
x


## ----variable-----------------------------------------------------------------
#| echo: true
(x <- x + 6)
x == 5
log(x) 


## ----bad-naming---------------------------------------------------------------
#| echo: true

## THIS IS BAD CODING! DON'T DO THIS!
pi
(pi <- 20)
abs
(abs <- abs(pi))


## ----type---------------------------------------------------------------------
#| echo: true
typeof(5)
typeof(5L)
typeof("I_love_stats!")


## -----------------------------------------------------------------------------
#| echo: true
typeof(1 > 3)
is.double(5L)


## ----vector-------------------------------------------------------------------
#| echo: true
(dbl_vec <- c(1, 2.5, 4.5)) 
(int_vec <- c(1L, 6L, 10L))
(log_vec <- c(TRUE, FALSE, F))  
(chr_vec <- c("pretty", "girl"))


## -----------------------------------------------------------------------------
#| echo: true
length(dbl_vec) 
str(dbl_vec) 


## ----vector-arithmetic--------------------------------------------------------
#| echo: true
# Create two vectors
v1 <- c(3, 8)
v2 <- c(4, 100) 

# Vector addition
v1 + v2
# Vector subtraction
v1 - v2


## -----------------------------------------------------------------------------
#| echo: true
# Vector multiplication
v1 * v2
# Vector division
v1 / v2
sqrt(v2)


## ----recycle------------------------------------------------------------------
#| echo: true
v1 <- c(3, 8, 4, 5)
v1 * 2
v1 * c(2, 2, 2, 2)
v3 <- c(4, 11)
v1 + v3


## ----subsetting---------------------------------------------------------------
#| echo: true
v1
v2
## The first element
v1[1] 
## The second element
v2[2]  


## -----------------------------------------------------------------------------
#| echo: true
v1[c(1, 3)]
v1[-c(2, 3)] 


## ----factor-------------------------------------------------------------------
#| echo: true
fac <- factor(c("med", "high", "low"))
typeof(fac)
levels(fac)
str(fac)
order_fac <- factor(c("med", "high", "low"), levels = c("low", "med", "high"))
str(order_fac)


## ----list---------------------------------------------------------------------
#| echo: true
x_lst <- list(idx = 1:3, 
              "a", 
              c(TRUE, FALSE))
x_lst


## -----------------------------------------------------------------------------
#| echo: true
str(x_lst)
names(x_lst)
length(x_lst)


## -----------------------------------------------------------------------------
#| echo: true
x_lst <- list(idx = 1:3,
             "a",
             c(TRUE, FALSE))


## -----------------------------------------------------------------------------
#| echo: true
## subset by name (a vector)
x_lst$idx  
## subset by indexing (a vector)
x_lst[[1]]  
typeof(x_lst$idx)


## -----------------------------------------------------------------------------
#| echo: true
## subset by name (still a list)
x_lst["idx"]  
## subset by indexing (still a list)
x_lst[1]  
typeof(x_lst["idx"])


## ----matrix-------------------------------------------------------------------
#| echo: true
## Create a 3 by 2 matrix called mat
(mat <- matrix(data = 1:6, nrow = 3, ncol = 2)) 
dim(mat)  # dimension
nrow(mat) # number of rows
ncol(mat) # number of columns


## ----matrix-indexing----------------------------------------------------------
#| echo: true
mat
mat[, 2]


## -----------------------------------------------------------------------------
#| echo: true
## 2nd row and all columns
mat[2, ] 
## The 1st and 3rd rows
mat[c(1, 3), ] 


## ----matrix-bind--------------------------------------------------------------
#| echo: true
mat
mat_c <- matrix(data = c(7, 0, 0, 8, 2, 6), nrow = 3, ncol = 2)
## should have the same number of rows
cbind(mat, mat_c)  


## -----------------------------------------------------------------------------
#| echo: true
mat_r <- matrix(data = 1:4, nrow = 2, ncol = 2)
## should have the same number of columns
rbind(mat, mat_r)  


## ----dataframe----------------------------------------------------------------
#| echo: true
## data frame w/ an dbl column named age and char column named gender
(df <- data.frame(age = c(19, 21, 40), gender = c("m", "f", "m")))

## a data frame has a list structure
str(df)  


## -----------------------------------------------------------------------------
#| echo: true
data.frame(c(19, 21, 40), c("m","f", "m")) 


## ----df-fcns, echo=1:5--------------------------------------------------------
#| echo: true
names(df)  ## as a list

colnames(df)  ## as a matrix

length(df) ## as a list

ncol(df) ## as a matrix

dim(df) ## as a matrix

typeof(df)

class(df)


## -----------------------------------------------------------------------------
#| echo: true
## rbind() and cbind() can be used on df

df_r <- data.frame(age = 10, gender = "f")
rbind(df, df_r)

df_c <- data.frame(col = c("red","blue","gray"))
(df_new <- cbind(df, df_c))


## ----df-subset----------------------------------------------------------------
#| echo: true
df_new

## Subset rows
df_new[c(1, 3), ]

## select the row where age == 21
df_new[df_new$age == 21, ]


## -----------------------------------------------------------------------------
#| echo: true
## Subset columns
## like a list
df_new$age
df_new[c("age", "gender")]

## like a matrix
df_new[, c("age", "gender")]

str(df["age"])  ## a data frame with one column

str(df[, "age"])  ## becomes a vector by default


## -----------------------------------------------------------------------------
#| echo: true
# ==============================================================================
## Vector
# ==============================================================================
poker_vec <- c(170, -20, 50, -140, 210)
roulette_vec <- c(-30, -40, 70, -340, 20)
days_vec <- c("Mon", "Tue", "Wed", "Thu", "Fri")
names(poker_vec) <- days_vec
names(roulette_vec) <- days_vec


## -----------------------------------------------------------------------------
#| echo: true
# ==============================================================================
## Factor
# ==============================================================================
# Create speed_vector
speed_vec <- c("medium", "low", "low", "medium", "high")


## ----eval=FALSE---------------------------------------------------------------
## _________ <- factor(________, ordered = ______,
##                     levels = ______________________)


## -----------------------------------------------------------------------------
#| echo: true
# ==============================================================================
## Data frame
# ==============================================================================
# Definition of vectors
name <- c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", 
          "Uranus", "Neptune")
type <- c("Terrestrial planet", "Terrestrial planet", "Terrestrial planet", 
          "Terrestrial planet", "Gas giant", "Gas giant", 
          "Gas giant", "Gas giant")
diameter <- c(0.375, 0.947, 1, 0.537, 11.219, 9.349, 4.018, 3.843)
rotation <- c(57.63, -242.03, 1, 1.05, 0.42, 0.44, -0.73, 0.65)
rings <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)


## ----eval=FALSE---------------------------------------------------------------
## ________ <- data.frame(______, ______, ______, ______, ______)

