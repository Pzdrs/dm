library(ggplot2)

# load data
df <- read.csv("data/GOODS1n.csv")



# Basic structure and summary
str(df)        # Check structure: variable types, factor levels, etc.
summary(df)    # Get summary statistics for each column
head(df)       # View the first few rows
dim(df)        # Get the number of rows and columns
colnames(df)   # Get column names
sapply(df, class)  # Check data types of each column

# Checking for missing values
sum(is.na(df))        # Total missing values
colSums(is.na(df))    # Missing values per column

# Quick EDA (Exploratory Data Analysis)
library(ggplot2)
library(dplyr)

df %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))  # Mean of numeric columns

ggplot(df, aes(x = df[[1]])) + geom_bar()  # Quick bar plot of the first column

# More detailed exploration
pairs(df)  # Scatterplot matrix (for numerical variables)

# Check for duplicate rows
sum(duplicated(df))




# pridame sloupec
revenue_increase <- (marketing$After - marketing$Before) / marketing$Before * 100

marketing$RevenueIncrease <- revenue_increase

plot(marketing$Promotion, marketing$RevenueIncrease,
     main = "Vztah mezi náklady na marketing a vzrůst tržeb",
     xlab = "Marketing costs",
     ylab = "Increase in revenue"
     )

ggplot(marketing)+
  geom_point(aes(
    x = Promotion, 
    y = RevenueIncrease, 
    color = Class
  ))



# Splitnout na trenovaci a testovaci data 70:30

# Natrenovat model s prediktorama
# 1) promotion
# 2) promotion + flag fieldy typu produktu
# 3) promotion + derived flag fieldy pronasobeny s hodnotou promotion

# Evaluatnout nejlepsi model - nejaky statisticky metody na porovnani vysledku + logickej reasoning