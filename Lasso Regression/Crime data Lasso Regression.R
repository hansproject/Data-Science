install.packages("glmnetUtils")
library(glmnetUtils)
library("rpart")
library("rpart.plot")
#library(dplyr)
# Read file
crime_data <- read.csv("C:/Users/ndeff/Downloads/crimedata.csv",
         sep=",",stringsAsFactors = FALSE, header=T,na.strings=c("#NUM!,","?"))

# Omit missing values
#crime_data <- na.omit(crime_data)

# Split data into 80% training and 20% testing
# number of rows of car dataset
n=nrow(crime_data)

# Set a fixed random seed to allow replicability of the results
set.seed(50)

#80% sample
train_sample = sample(1:n, floor(n*0.80))
train_data   = crime_data[train_sample, ] 
x_train      = subset(crime_data[train_sample, ],select = -c(ViolentCrimesPerPop))
y_train      = subset(crime_data[train_sample, ],select = c(ViolentCrimesPerPop))

test_data   = crime_data[-train_sample, ] 
x_test = subset(crime_data[-train_sample, ],select = -c(ViolentCrimesPerPop))
y_test = subset(crime_data[-train_sample, ],select = c(ViolentCrimesPerPop))

# Train the model
#lars_model <- glmnet(x=as.matrix(x_train),y= as.matrix(y_train), alpha=1,
#                    standardize=TRUE)
lars_model <- glmnet(ViolentCrimesPerPop~.,train_data, alpha=1,
                     standardize=TRUE)

# lambda_sel <- 10^seq(-5,5,10/dim(train_data)[1])
plot(lars_model, xvar = "lambda", label = TRUE)  

pred_train <- predict(lars_model, newdata = x_train, 
                      na.action = na.pass)  #predict(NA) as NA
pred_test <- predict(lars_model, newdata = x_test, 
                      na.action = na.pass)  #predict(NA) as NA

RMSE_train <- sqrt(colMeans((pred_train- c(unlist(y_train)))^2, na.rm = TRUE))
RMSE_test  <- sqrt(colMeans((pred_test- c(unlist(y_test)))^2, na.rm = TRUE))

#___________________________________________________
# 1) Plot RMSE for training and testing data
plot(x=log10(lars_model[["lambda"]]), y=RMSE_train, 
     xlab="log(lambda)",
     ylab="RMSE", 
     type="o", lty=1, pch=20,  col="blue")  

lines(x=log10(lars_model[["lambda"]]), y=RMSE_test, 
      xlab="log(lambda)",
      ylab="RMSE", 
      type="o", lty=1, pch=20,  col="red")
legend(-0.5,600,c("RMSE_train"," RMSE_test"),
       col=c("blue","red"),
       lty = c(1,1), pch = c(16,16))

#___________________________________________________
# 2)Plot difference between train and testing errror
lars_model$lambda[match(min(abs(RMSE_train- RMSE_test)),abs(RMSE_train- RMSE_test))]

plot(x=log10(lars_model[["lambda"]]), y=abs(RMSE_train- RMSE_test), 
     xlab="log(lambda)",
     ylab="Difference", 
     type="o", lty=1, pch=20,  col="blue", main = "|RMSE train - RMSE test|")

#___________________________________________________
# 3) Selecting the optimal model based on the diagrams in Problems 1 and 2
# We want to select the model that gives the ?? value that gives
# the lowest abs(RMSE_train- RMSE_test) error
# The line below return the position of lambda value gives the minimum error
position <- match(min(abs(RMSE_train- RMSE_test)),abs(RMSE_train- RMSE_test))

# Get the corresponding beta values
lars_model$beta[,position]