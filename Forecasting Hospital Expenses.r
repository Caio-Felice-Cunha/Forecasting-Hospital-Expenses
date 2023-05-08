# Forecasting Hospital Expenses

# Business Problem: Forecast of Hospital expenses

# For this analysis, we will use a dataset simulating hypothetical medical expenses
# for a set of patients spread across 4 US regions.
# This dataset has 1338 observations and 7 variables.

# Step 1 - Collecting the data
expensesdf <- read.csv("expenses.csv")
View(expensesdf)

# Step 2: Exploring and Preparing the Data
# Viewing the variables
str(expensesdf)

# Measures of Central Tendency of the variable expenses
summary(expensesdf$expenses)

# Building a histogram
hist(expensesdf$expenses, main = 'Expenses Histogram', xlab = 'Expenses')

# Region contingency table
table(expensesdf$region)

# Exploring relationships between variables: Correlation Matrix
cor(expensesdf[c("age", "bmi", "children", "expenses")])

# None of the correlations in the matrix are considered strong, but there are some interesting associations.
# For example, age and bmi (BMI) seem to have a weak positive correlation, meaning that
# with increasing age, body mass tends to increase. There is also a positive correlation
# moderate between age and expenses, in addition to the number of children and expenses. These associations imply
# that as you age, body mass and number of children increase, the expected cost of health insurance goes up.

# Visualizing relationships between variables: Scatterplot
# Realize that there is no clear relationship between the variables
pairs(expensesdf[c("age", "bmi", "children", "expenses")])


# Scatterplot Matrix
#install.packages("psych")
library(psych)

# This plot provides more information about the relationship between variables
pairs.panels(expensesdf[c("age", "bmi", "children", "expenses")])


# Step 3: Training the model (using the training data)
?lm
model <- lm(expenses ~ ., data = expensesdf)

# Visualizing the coefficients
model

# Anticipating medical expensesdf
?predict

# Here we check the expenses predicted by the model which must be equal to the training data
forecast1 <- predict(model)
View(forecast1)

# Predicting expenses with test data
expensesdftest <- read.csv("expenses-test.csv")
View(expensesdftest)
forecast2 <- predict(model, expensesdftest)
View(forecast2)


# Step 4: Evaluating the Model's Performance
# More details about the model
summary(model)


# Step 5: Optimizing model performance


# Adding a variable with twice the value of ages
expensesdf$age2 <- expensesdf$age ^ 2

# Adding an indicator for BMI >= 30
expensesdf$bmi30 <- ifelse(expensesdf$bmi >= 30, 1, 0)

View(expensesdf)

# Creating the final model
model_v2 <- lm(expenses ~ age + age2 + children + bmi + sex +
                   bmi30 * smoker + region, data = expensesdf)

summary(model_v2)

# test data

expensesdftest <- read.csv("expenses-test.csv")
View(expensesdftest)
forecast3 <- predict(model, expensesdftest)
class(forecast3)
View(forecast3)


