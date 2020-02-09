#################################################
## Nik S.
## HarvardX: PH125.9x Data Science: Capstone OWN_PROJECT (HarvardX: PH125.9x Data Science: Capstone) 
## GitHub repository is here: https://github.com/nik-labs/Own_Project
## Using R 3.6.2 version 
###############################################################

######################################################################
# Introduction - Own_Project - Metropolitan Housing
# NOTE: dataset accessed from my own GitHub website
######################################################################
## Aim is to train a machine learning algorithm using the inputs from metropolitan housing data to accurately predict the housing prices in its surrounding areas from the metropolitan center.

## Note: this process could take a couple of minutes because it is loading such as caret packages

# Package installs
if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(forcats)) install.packages("forcats")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(stringr)) install.packages("stringr")

# Loading libraries as needed
library(dplyr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(forcats)
library(ggplot2)
library(stringr)
library(caret)
library(readr)

# Obtain Metropolitan House Pricing dataset from my GitHub website
urlfile="https://raw.githubusercontent.com/nik-labs/Own_Project/master/housingdata.csv"
mydata<-read_csv(url(urlfile))

# Rename mydata to dataframe called dataset
dataset.df <- mydata

# set the column names in the dataset
colnames(dataset.df) <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")
## Definitions of Columns are as follows:
# Column 1: CRIM = per capita crime rate by town
# Column 2: ZN = proportion of residential land zoned for lots > 25,000 sq.ft
# Column 3: INDUS = proportion of non-retail business acres per town
# Column 4: CHAS = Charles River dummy variable (1 if tract bounds river; else 0)
# Column 5: NOX = nitric oxides concentration (10 ppm)
# Column 6: RM = average number of rooms per dwelling
# Column 7: AGE = proportion of owner-occupied units built prior to 1940
# Column 8: DIS = weighted distances to five Boston employment centres
# Column 9: RAD = index of accessibility to radial highways
# Column 10: TAX = full-value property-tax rate per $10k
# Column 11: PTRATIO = pupil-teacher ratio by town
# Column 12: B = 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
# Column 13: LSTAT = % lower status of the population
# Column 14: MEDV = Median value of owner-occupied homes in $1000’s


# dimensions of dataset
dim(dataset.df)

# Display the class of the object dataset
class(dataset.df)



######################################################################
# METHODS/ANALYSIS - Metropolitan Housing Dataset
######################################################################

# Now further inspect data to understand each columns staistical significance
# Data presented will include min and max values, median, mean and quartiles values
summary(dataset.df)

# Depiction of MEDV histogram distribution (is the distribution normal?)
hist(dataset.df$MEDV)


# Employ scatterplot to view attributes with an illustration of correlation or lack there of.
plot(dataset.df[,c(3,5,6,11,13,14)],pch=3)


# Correlation of each independent variable with the dependent variable and to also see whether a feature has near zero variance within each column
cor(dataset.df,dataset.df$MEDV)


# Calulate/confirm whether a variable has a near zero variance 
nzvariance <- nearZeroVar(dataset.df, saveMetrics = TRUE)
sum(nzvariance$nzvariance)


# Validation set will be 30% of Metropolitan Housing data
dataset.scale <- cbind(scale(dataset.df[1:13]), dataset.df[14])

set.seed(1, sample.kind="Rounding")
#Do data partitioning
inTrain <- createDataPartition(y = dataset.scale$MEDV, p = 0.70, list = FALSE)
training <- dataset.scale[inTrain,]
testing <- dataset.scale[-inTrain,]

#############################################################################
# Final Results Comparing two Regression Models to isolate the best approach
#############################################################################

# Model 1:Simple Linear Regression Model
# This approach will set all variables as independent variables with the exception of MEDV. The model will be trained and used to predict the outcome of the dependent variable MEDV. THe Root-Mean Squared Error (RMSE) will test the accuracy of this model using the formula

set.seed(1, sample.kind="Rounding")
fit.lm <- lm(MEDV~.,data = training)

#verify co-efficients
data.frame(coef = round(fit.lm$coefficients,2))

# Make a prediction on dataset and calculate respective Root-mean squared error (RMSE)
set.seed(1, sample.kind="Rounding")
pred.lm <- predict(fit.lm, newdata = testing)
rmse.lm <- sqrt(sum((pred.lm - testing$MEDV)^2)/
                   length(testing$MEDV))
                   
c(RMSE = rmse.lm, R2 = summary(fit.lm)$r.squared)




# Model 2:Implement Random Forest Model to MEDV
# This approach will consider MEDV as the ouput from all features


suppressMessages(library(randomForest))
set.seed(1, sample.kind="Rounding")

fit.rf <- randomForest(formula = MEDV ~ ., data = training)
set.seed(1, sample.kind="Rounding")
pred.rf <- predict(fit.rf, testing)

rmse.rf <- sqrt(sum(((pred.rf) - testing$MEDV)^2)/
                   length(testing$MEDV))
c(RMSE = rmse.rf, pseudo_R2 = mean(fit.rf$rsq))

#Plotting Actual Price vs. Estimated Price
plot(pred.rf,testing$MEDV, xlab = "Predicted Price", ylab = "Actual Price", pch = 3)







