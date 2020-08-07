
# Project 5 Part 2
#______________
# Load libraries
install.packages("arules")
library(arules)
library("pracma")
library("stringr")
library("rpart")
library("rpart.plot")
library("flextable")
library("knitr")

car_data = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data",
                      sep = ",",header = FALSE)
# Get number of rows and columns
n_rows = dim(car_data)[1]
n_cols = dim(car_data)[2]
# Data cleaning
#______________
# Show unique values in column
unique(car_data[,1])
# Show unique values in column
unique(car_data[,2])
# Show unique values in column
unique(car_data[,3])
car_data[,3] <- data.frame(str_replace_all(unlist(car_data[,3]),"5more","5 or more"))
unique(car_data[,3])
# Show unique values in column
unique(car_data[,4])
# Show unique values in column
unique(car_data[,5])
# Show unique values in column
unique(car_data[,6])
# Show unique values in column
unique(car_data[,7])
#____________________________________________
# 1) Plot the tree
names(car_data) = c("buying","maint","doors","persons","lug_boot","safety","class.values")
rtree = rpart(class.values ~ ., data = car_data)#default split="gini")
rpart.plot(rtree,main="Decision tree for car_data")
# Show tree
rtree

#____________________________________________
# 2) Predict output given car1 and car2
car1 = data.frame(buying="vhigh", maint ="vhigh", doors="2", persons="2", lug_boot ="small",
                  safety="low")
predict(rtree, car1, type = "class")

car2 = data.frame(buying="med", maint ="med", doors="4", persons="4", lug_boot ="small",
                  safety="med")
predict(rtree, car2, type = "class")
#____________________________________________
# 3) Determining criteria making a car good
# left hand side  = lhs. lhs could be any other of the 6 attributes such as buying, safety.
# right hand side = rhs
# support of {lhs} -> {rhs = good} 
# apriori create a set of rules then compute their support and confidence in the car_data
# parameter takes a list which can contains:
# - support   : get rules whose minimum support is a specified value
# - confidence: get rules whose minimum confidence is a specified value
rules = apriori(car_data, 
                appearance = list(rhs = "class.values=good"),
                parameter  = list(minlen=7, support = 0.0001, confidence = 0.0001))
inspect(rules)
#____________________________________________
# 4) Split data into 80% training and 20% testing
#sum(predict(rtree, car_data, type = "class") != car_data$class.values)/nrow(car_data)
# number of rows of car dataset
n=nrow(car_data)
#80% sample
train_sample = sample(1:n, floor(n*0.80))
train_data = car_data[train_sample, ]
test_data = car_data[-train_sample, ]
#check the dimensin of the training and testing data
dim(train_data)
dim(test_data)

# Create a file with 3 columns were data will be added
# Column 1 will be for depth
# Column 2 for training_error
# Column 3 for testing_error
file_header    <- c("depth","training_error","testing_error")
column1        <- c()
column2        <- c()
column3        <- c()
# Compute the error
# Accept 2 arguments:
#   - depth: depth of the tree that will be train to compute the error
#   - error: Type of error that will be computed("training error" or "testing error")
compute_error = function(depth, error){
    #Train the model
   fit_tree = rpart(class.values ~ ., data = train_data, maxdepth = depth)
   if(strcmp(tolower(error),"training error")){
        #compute the training error
      train_predict = predict(fit_tree, train_data, type = "class")
      return (sum(train_predict != train_data$class.values)/nrow(train_data))
   }
     
    else{
        #compute the testing error
       test_predict = predict(fit_tree, test_data, type = "class")
       return (sum(test_predict != test_data$class.values)/nrow(test_data))
      }
}
for(i in 1:8){
  cur_depth = i
  column1 <- c(column1, cur_depth)
  column2 <- c(column2, compute_error(cur_depth,"training error"))
  column3 <- c(column3, compute_error(cur_depth,"testing error"))
}
results = data.frame(matrix(c(column1,column2,column3),nrow = length(column1), ncol = 3))
names(results) = file_header
table = flextable(results,col_keys = names(results),
                                cwidth = 2, cheight = 2, theme_fun = theme_tron_legacy)
table <- color(table,color = "black", part = "body")
table <- bg(table,bg = "white")
table
