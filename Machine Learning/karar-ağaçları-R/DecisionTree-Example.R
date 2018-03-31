                      #### PACKAGE ISSUES ####
install.packages(caret) # The package that is used to create a tree Classification and Regression Tree
install.packages(rpart.plot) # will used for visualization
library(caret)
library(rpart.plot)
#To model a classifier for evaluating the acceptability of car using its given features

                      #### KNOWING DATA ####
str(car_data) #checking the structure of data
anyNA(car_data) # this method is used to check that data contains missing values or not. NA means Not Available
summary(car_data) #it will show the summary of dataset


                      #### MODELING DATA ####
set.seed(3033) #random number generator - RNG
#basically set.seed() function will help to reuse the same set of random variables

intrain <- createDataPartition(y = car_data$Evaluation, p= 0.7, list = FALSE) #The caret package provides a method createDataPartition() 
# for partitioning our data into train and test set. The target value is Evaluation
# We are using p=0.7. It means that data split should be done in 70:30 ratio. (shows percentage) FALSE for not returning a list

training <- car_data[intrain,] #putting the data from data source to training data
testing <- car_data[-intrain,] #putting the data from data source to training data - remaining data is saved as testing


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) # controls computational nuances of the train() method
# the method 'repeatedcv is repeated cross-validation methos. 

set.seed(3333)
dtree_fit <- train(Evaluation ~., data = training, method = "rpart", #Recursive Partitioning and Regression Trees, actually rpart is a package like caret.
                   parms = list(split = "information"), #information means "information gain - entropy method"
                   trControl=trctrl,
                   tuneLength = 10)
dtree_fit #checking the result of train() methods and will display the "cp" that is complexity parameter for our 
                      #### PREDICTION ####
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2) # used to visualize tree
test_pred <- predict(dtree_fit, newdata = testing) #predict the target variable for whole test dataset
confusionMatrix(test_pred, testing$Evaluation ) #check accuracy
