
# Generating a random sample for demonstration
set.seed(501)
data_sample <- rnorm(100)  
shapiro.test(data_sample)

list.files("Data/")
milk <- read.csv("Data/Milk.csv")
milk <- milk[,2:3]
shapiro.test(milk)

plot(lm(Cost ~ Volume, data=milk))


