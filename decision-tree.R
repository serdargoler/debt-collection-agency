## Importing packages
library(readxl)
library(tidyverse)
library(writexl)
library(caTools)
library(data.table)
library(caret)
library(rpart)
library(rpart.plot)

# Importing dataset
dca <- read_excel("DCA_Old.xlsx")
head(dca)

# dividing the data into train and test set.
set.seed(123)
dca_train <- dca %>% dplyr::sample_frac(.75)
dca_test  <- dplyr::anti_join(dca, dca_train, by = 'ID')

## Changing type of features.

dca_train <- mutate(dca_train, Employer=factor(Employer),
                    Insolvency=factor(Insolvency),
                    BET=factor(BET),
                    Principal=as.numeric(Principal),
                    Interest=as.numeric(Interest),
                    Age=as.numeric(Age))

dca_test <- mutate(dca_test, Employer=factor(Employer),
                   Insolvency=factor(Insolvency),
                   BET=factor(BET),
                   Principal=as.numeric(Principal),
                   Interest=as.numeric(Interest),
                   Age=as.numeric(Age))

## There was MISSING value in "Age".

sum(is.na(dca_train))
sum(is.na(dca_test))

## We convert them with mean of "Age" variable.

dca_train$Age[is.na(dca_train$Age)] <- mean(dca_train$Age, na.rm = TRUE)
dca_test$Age[is.na(dca_test$Age)] <- mean(dca_test$Age, na.rm = TRUE)

sum(is.na(dca_train))
sum(is.na(dca_test))

## Changing type of features.

dca_train <- mutate(dca_train, Person=factor(Person))
dca_train <- mutate(dca_train, Region=factor(Region))

dca_test <- mutate(dca_test, Person=factor(Person))
dca_test <- mutate(dca_test, Region=factor(Region))

## Creating CF% variable.
dca_train$cf <- round(dca_train$Cashflow/dca_train$TotalClaim*100, 2)
head(dca_train)

# See whether there is outlier or not.
boxplot(dca_train$Cashflow, main="Boxplot of Cashflow" , ylab="Cashflow", col="blue", outcol="red")

# see whether any zeros ("O") in the dataset. If there is any zero we can't get log of variable. But we can take square of it. 
colSums(dca_train == 0)

dca_train$cashflow_sq <- dca_train$Cashflow^2

# After transformed the variable.
boxplot(dca_train$cashflow_sq, main="Boxplot of Cashflow Square" , ylab="Cashflow_Sq", col="blue", outcol="red")

boxplot(dca_train$TotalClaim)

dca_train$ltotal_claim <- log(dca_train$TotalClaim)

boxplot(dca_train$ltotal_claim)

boxplot(dca_train$Claim_Age)

dca_train$lclaim_age <- log(dca_train$Claim_Age)

boxplot(dca_train$lclaim_age)

boxplot(dca_train$Age)

dca_train$l_age <- log(dca_train$Age)

boxplot(dca_train$l_age)

# One hot encodings (dummify person variable)
for(unique_value in unique(dca_train$Person)){
  
  
  dca_train[paste("DPerson", unique_value, sep = "_")] <- ifelse(dca_train$Person == unique_value, 1, 0)
  
}

head(dca_train)

# to interact two factor variables we have to change employer variable as numeric.
dca_train$Employer <- as.numeric(as.character(dca_train$Employer))

# create a new variable for working men
dca_train$Employer_M <- dca_train$Employer * dca_train$DPerson_1

## Changing type of features.
dca_train <- mutate(dca_train, DPerson_1=factor(DPerson_1))
dca_train <- mutate(dca_train, DPerson_2=factor(DPerson_2))
dca_train <- mutate(dca_train, DPerson_3=factor(DPerson_3))
dca_train <- mutate(dca_train, DPerson_4=factor(DPerson_4))
dca_train <- mutate(dca_train, Employer_M=factor(Employer_M))

sum(is.na(dca_train))

#creating the first model.
model1 <- lm(cf ~ ltotal_claim + Employer_M + Claim_Age + BET + Insolvency, data = dca_train)
summary(model1)

#find Cook's distance for each observation in the dataset
cooksD <- cooks.distance(model1)

# Plot Cook's Distance with a horizontal line at 4/n to see which observations
#exceed this thresdhold
n <- nrow(dca_train)
plot(cooksD, main = "Cooks Distance for Influential Obs")
abline(h = 4/n, lty = 2, col = "steelblue") # add cutoff line

#identify influential points
influential_obs <- as.numeric(names(cooksD)[(cooksD > (4/n))])

#define new data frame with influential points removed
outliers_removed <- dca_train[-influential_obs, ]

model2 <- lm(cf ~ ltotal_claim + Employer_M + Claim_Age + BET + Insolvency, data = outliers_removed)
summary(model2)

# Transforming new variables for test set.
dca_test$ltotal_claim <- log(dca_test$TotalClaim)
## One hot encodings
for(unique_value in unique(dca_test$Person)){ 
  dca_test[paste("DPerson", unique_value, sep = "_")] <- ifelse(dca_test$Person == unique_value, 1, 0)
}
dca_test$Employer <- as.numeric(as.character(dca_test$Employer))
dca_test$Employer_M <- dca_test$Employer * dca_test$DPerson_1
dca_test <- mutate(dca_test, DPerson_1=factor(DPerson_1))
dca_test <- mutate(dca_test, DPerson_2=factor(DPerson_2))
dca_test <- mutate(dca_test, DPerson_3=factor(DPerson_3))
dca_test <- mutate(dca_test, Employer_M=factor(Employer_M))

## Creating CF% variable for test set.
dca_test$cf <- round(dca_test$Cashflow/dca_test$TotalClaim*100, 2)

# predict the cashflow percentages for new claims
dca_test$cf_new <- predict(model2, dca_test)

# calculating new cashflows
dca_test$Cashflow_new <- round((dca_test$TotalClaim * dca_test$cf_new/100),digits = 2)
head(dca_test)

data.frame(R2 = R2(dca_test$Cashflow_new, dca_test$Cashflow),
           RMSE = RMSE(dca_test$Cashflow_new, dca_test$Cashflow),
           MAE = MAE(dca_test$Cashflow_new, dca_test$Cashflow))


# Compute R-squared
SSE = sum((dca_test$Cashflow - dca_test$Cashflow_new)^2)
SST = sum((dca_test$Cashflow - mean(dca_train$Cashflow))^2)
1 - SSE/SST

head(dca_train)
# Regression Tree

fit <- rpart(cf ~ TotalClaim + Region + Age, data = dca_train)

rpart.plot(fit, extra=101)

predict_unseen <- predict(fit, dca_test, type = 'vector')

unique(predict_unseen)








