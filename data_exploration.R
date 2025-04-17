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
