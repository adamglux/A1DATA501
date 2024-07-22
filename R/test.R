
# Generating a random sample for demonstration
set.seed(501)
data_sample <- rnorm(100)  
shapiro.test(data_sample)

list.files("Data/")
milk <- read.csv("Data/Milk.csv")
milk <- milk[,2:3]
shapiro.test(milk)
SW_test(milk)

plot(lm(Cost ~ Volume, data=milk))


usethis::use_testthat()

SW_test <- function(data, plot_qq = FALSE) {
  # Input validation
  if (!is.numeric(data)) {
    stop("Input data must be numeric.")
  }
  
  if (length(data) < 3) {
    stop("Input data must have at least 3 values.")
  }
  
  if (any(is.na(data))) {
    stop("Input data contains NA values.")
  }
  
  if (any(is.infinite(data))) {
    stop("Input data contains infinite values.")
  }
}

data <- cbind(rnorm(100),rnorm(100),rnorm(100))

  #Obtain sorted data, x_{(i)}
  n <- length(data)
  sorted_data <- sort(data)
  
  # compute expected values for a normal dist
  m <- qnorm((1:n - .375) / (n + .25))
  
  V <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      V[i, j] <- min(i, j) / n - (i * j) / n^2
    }
  }
  
  # Compute inverse of V
  V_inv <- MASS::ginv(V)
  
  # Compute C
  C <- sqrt(t(m) %*% V_inv %*% V_inv %*% m)
  
  # Compute weights a
  a <- (t(m) %*% V_inv) / C
  
  # Compute the mean of the data
  x_bar <- mean(data)

  # Compute the Shapiro-Wilk test statistic W
  W <- (sum(a * sorted_data)^2) / sum((sorted_data - mean(sorted_data))^2)
  
  # Plot QQ-plot if required
  if (plot_qq) {
    qqnorm(data)
    qqline(data, col = "blue", lwd = 2)
  }
  
  # Return W statistic
  return(W)
}

# Basic tests
# Test 1: Valid numeric data
data1 <- cbind(rnorm(100),rnorm(100),rnorm(100)) 
cat("Test 1 - W statistic:", SW_test(data1), "\n")
length(data1)

# Test 2: Valid numeric data with QQ-plot
data2 <- runif(100)
cat("Test 2 - W statistic with QQ-plot:", SW_test(data2, plot_qq = TRUE), "\n")

# Test 3: Invalid data (NA values)
data3 <- c(rnorm(50), NA)
cat("Test 3 - W statistic with NA values:", SW_test(data3), "\n") # Uncomment to see error

# Test 4: Invalid data (infinite values)
data4 <- c(rnorm(50), Inf)
# cat("Test 4 - W statistic with infinite values:", shapiro_wilk_manual(data4), "\n") # Uncomment to see error

# Test 5: Invalid data (non-numeric)
data5 <- c("a", "b", "c")
# cat("Test 5 - W statistic with non-numeric data:", shapiro_wilk_manual(data5), "\n") # Uncomment to see error

# Test 6: Invalid data (too few values)
data6 <- c(1, 2)
# cat("Test 6 - W statistic with too few values:", shapiro_wilk_manual(data6), "\n") # Uncomment to see error
