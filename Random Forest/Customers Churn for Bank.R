#library(dplyr)

# ggplot2 and cowplot for plotting  
library(ggplot2)
library(cowplot)

# Contains the randomForest algorithm 
library(randomForest)

# DataExplorer creates a preview of the dataset
library(DataExplorer)

# Caret is used to predict the customers in the testing set
library(caret)

# Read file
bank_customers <- read.csv("D:/Important files and pics/Data Science/Random Forest/Churn_Modelling.csv",
                       sep=",",stringsAsFactors = FALSE, header=T,na.strings=c("#NUM!,","?"))
# Show a preview of the bank customer dataset 
introduce(bank_customers)
plot_intro(bank_customers)
bank_customers[1:5,]

# Convert Exited column from continous to discrete value
bank_customers[,14]<- as.factor(bank_customers[,14])
# number of rows of car dataset
n=nrow(bank_customers)
#80% sample
train_sample = sample(1:n, floor(n*0.80))
train_data = bank_customers[train_sample, ]
test_data = bank_customers[-train_sample, ]

set.seed(12)
rf <- randomForest(Exited ~ CreditScore+ Age+ Tenure+ Balance+ NumOfProducts+ HasCrCard, data = train_data,
                   type = classification, importance = TRUE, proximity = TRUE)
p1 <- predict(rf, test_data)
p1[1:60]
