#setwd("C:/Users/HP/Desktop/AIMS Documents/Review Phase/Statistical Regression/Assignment")
# Installing the packages needed for this project
install.packages("ggplot")

# Loading packages
library("ggplot2")

# Task A - Performing a descriptive analysis

# Loading the data
data <- read.csv("data_purchase_behaviour.csv")

# Converting User_ID and Marital status to categorical data
data$User_ID <- as.factor(data$User_ID)
data$Marital_Status <- as.factor(data$Marital_Status)
str(data)
summary(data)
nrow(data)

#Getting the numeric variables in the dataset
numeric_columns <- sapply(data, is.numeric)
numeric_data <- data[, numeric_columns]

# Computing the mean of the numerical variables
sapply(numeric_data, mean)

# Computing the mode of the numerical variables.
# Mode function
mode <- function(x) {
  num <- unique(x)
  num[which.max(tabulate(match(x, num)))]
}
sapply(numeric_data, mode)

# Computing the standard deviation of the numerical variables
sapply(numeric_data, sd)

# Computing the median of the numerical variables
sapply(numeric_data, median)

# Getting frequency table for Gender
table(data$Gender)

# More male are sampled in this dataset. There are 119626 males and 39374 females.

# Getting frequency table for City Category.
table(data$City_Category)
# We noticed City B has the highest proportion of the customers.

# Getting the proportion of each gender in each city category.
table(data$Gender, data$City_Category)
# Most of the males and females are from city B.

# Bar plot for Gender
ggplot(data, aes(x = Gender)) +
  geom_bar(fill = c("green", "skyblue"), color = "black") +
  labs(title = "Distribution of Gender",
       x = "Gender",
       y = "Fequency") +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) + # Adding labels to bars
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Bar plot for City Category 
ggplot(data, aes(x = City_Category)) +
  geom_bar(fill = c("red", "green", "skyblue"), color = "black") + # Custom colors for bars and border
  labs(title = "Distribution of City Category",
       x = "City Category",
       y = "Frequency") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) + # Adding labels to bars
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

##### Marital Status

# Getting frequency of the marital status distribution.
marital_status <- table(data$Marital_Status)
print(marital_status)

# Getting the proportion of the marital status
marital_status / 159000

# 41.1 % of the customers are married while 58.9% are not married.

# Marital status by Gender
table(data$Gender, data$Marital_Status)
# More proportion of the male are unmarried.

# A bar plot showing the relationship between Gender and Marital status
ggplot(data, aes(x = Marital_Status, fill = Gender)) +
  geom_bar(position = "dodge") + 
  labs(title = "Gender Distribution by Marital Status",
       x = "Marital Status",
       y = "Count",
       fill = "Gender") +
  scale_fill_manual(values = c("red", "green"))+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5), # Centering the title
  )

#####  Stay in current city years

# Histogram for Stay_In_Current_City_Years
ggplot(data, aes(x = Stay_In_Current_City_Years)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Distribution of Stay In Current City Years",
       x = "Years",
       y = "Frequency") +
  theme_minimal()

# From the histogram, it is evident that most of the customers have stayed more than a year in their current city.

# Boxplot for Age and Purchase variables         

# Set up the plotting area
par(mfrow = c(1, 2))

# Define the labels for the boxplots
labels <- colnames(numeric_data)[2:ncol(numeric_data)]
title <- c("Age", "Purchase")
for (k in 2:ncol(numeric_data)) {
  boxplot(numeric_data[[k]], main = title[k-1], col = "green")
}

################## TASK 2

# We want to fit a simple linear regression model to investigate the dependence of the purchase amount on age.

# Let x be Age, and y, the purchase amount
x <- data$Age_num
y <- data$Purchase
n = length(x)
x_bar <- sum(x)/n           # mean of Age
y_bar <- sum(y)/n           # mean of purchase

# Computing the intercept(beta0_hat) and the slope(beta1_hat) of the model.
beta1_hat <- sum((x - x_bar)*y)/sum((x - x_bar)^2)
beta0_hat <- y_bar - beta1_hat * x_bar
print(paste("The intercept is", beta0_hat))
print(paste("The slope is", beta1_hat))

# Now, to compute the uncertainties.
sigma_2 <- sum(((y - beta0_hat - beta1_hat * x)^2) / (n-2))         # variance of the error term
var_beta0_hat <- sum(x^2) * sigma_2 / (n * sum((x - x_bar)^2))      # variance of beta0_hat
var_beta1_hat <- sigma_2 / sum((x - x_bar)^2)                       # variance of beta1_hat
cov_beta0_beta1 <- -x_bar * sigma_2 /sum((x - x_bar)^2)             # covariance of beta0_hat and beta1_hat
sd_error_beta0 <- sqrt(var_beta0_hat)
sd_error_beta1 <- sqrt(var_beta1_hat)
print(paste("The variance of beta0_hat is", var_beta0_hat))
print(paste("The variance of beta1_hat is", var_beta1_hat))
print(paste("The standard error of beta0_hat is", sd_error_beta0))
print(paste("The standard error of beta1_hat is", sd_error_beta1))
print(paste("The covariance of beta0_hat and beta1_hat is", cov_beta0_beta1))
cov_matrix <- matrix(c(var_beta0_hat, cov_beta0_beta1, cov_beta0_beta1, var_beta1_hat), 2, 2) # covariance matrix
colnames(cov_matrix) <- c("beta0_hat", "beta1_hat")
rownames(cov_matrix) <- c("beta0_hat", "beta1_hat")
print(cov_matrix)


# Finding the fitted values
fitted_y <- beta0_hat + beta1_hat * x

# Finding the residuals

residuals <- y - fitted_y

# Residuals vs Fitted plot: To check for linear association and Homoskedasticity.

plot(fitted_y, residuals, main = "Residuals against Fitted values",
     xlab = "Fitted values", ylab = "Residuals")
abline(h=0, col = "red")

# Looking at the plot of residuals against the fitted values, we have that the linear association
# assumption is not satisfied. Since, the residuals above the line does not balance the ones below. 
# Whereas, the pattern of the residuals are even. Hence, it satisfied the homoskedasticity assumption


# Normal Q-Q plot to check for the normality assumption
qqnorm(residuals)
qqline(residuals, col = "red")


mean_residuals <- mean(residuals)   # mean of resiuals
sd_residuals <- sd(residuals)      # standard deviation of residuals

# Create histogram with normal curve
ggplot(data, aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), binwidth = 800, fill = "green", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean_residuals, sd = sd_residuals), col = "red", size = 1) +
  labs(title = "Histogram of Residuals with Normal Curve",
       x = "Residuals",
       y = "Density") +
  theme_minimal()

# The histogram seems to not follow the shape of the normal curve, it is a far away from it. 
# This is because the age variable over-estimate the purchase amount causing the change in the shape of the 
# histogram. Overall, the residuals is not normal. This is justified by the Q-Q plot. 

#### Validating model
model <- lm(data$Purchase ~ data$Age_num)
summary(model)


