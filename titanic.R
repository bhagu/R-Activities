# Loading the Train and Test Data

train <- read.csv("train.csv")
test <- read.csv("test.csv")

# train is a data frame with 891 observation and 12 variables
# test is a data frame with 418 observation and 11 variables

names(train)
# [1] "PassengerId" "Survived"    "Pclass"      "Name"        "Sex"        
# [6] "Age"         "SibSp"       "Parch"       "Ticket"      "Fare"       
# [11] "Cabin"       "Embarked"   

str(train)
str(test)

#How many people survived? - Train Data set
table(train$Survived)

#proportions -survival
prop.table(table(train$Survived))

#Geneder wise survival
table(train$Sex,train$Survived)

#  Output
#  0   1
#  female  81 233
#  male   468 109


#Gender wise survival/death propotions
prop.table(table(train$Sex,train$Survived),margin=1)

#  Output
#  0         1
#  female 0.2579618 0.7420382
#  male   0.8110919 0.1889081
#  Observation - Female survival ratio is higher than male survival ration

# Considering Age as a factor for survival

#to check whether children were saved first
train$Child <- NA
train$Child[train$Age<18] <- 1
train$Child[train$Age>=18] <- 0

#Comparison
table(train$Child,train$Survived)

#proportion
prop.table(table(train$Child, train$Survived), 1)

#First prediction
test_one <- test
#Initiate Survived to 0
test_one$Survived <- 0

#Set Survived to 1 if Sex equals "female"
test_one$Survived[test$Sex =="female"] <- 1


#Decision Tree Algorithm
#Load the rpart package
library("rpart")

#Build a decision tree my_tree_two:You want to predict Survived based on Pclass, Sex, Age, SibSp, Parch, Fare and Embarked.
#Use the train data to build the tree
#Use method to specify that you want to classify.

my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked , data = train, method = "class")

# Visualize the decision tree using plot() and text()
plot(my_tree_two)
text(my_tree_two)

# Load in the packages to build a fancy plot
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Time to plot your fancy tree
fancyRpartPlot(my_tree_two)


# Make predictions on the test set
my_prediction <- predict(my_tree_two, test, type ="class")

# Finish the data.frame() call
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Use nrow() on my_solution
nrow(my_solution)

# Finish the write.csv() call
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

# Change this command
my_tree_three <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
                     data = train, method = "class", control = rpart.control(minsplit = 50, cp = 0))
  
# Visualize my_tree_three
fancyRpartPlot(my_tree_three)

#Here we Overfitted the Tree! Which is dangerous as it is valid for your training set and the story would be different when you apply this on a test set

#Reengineering the Titanic Dataset


# Create train_two
train_two <- train
train_two$family_size <- train_two$SibSp + train_two$Parch + 1

# Finish the command
my_tree_four <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked +family_size, 
                      data = train_two, method = "class")

# Visualize your new decision tree
fancyRpartPlot(my_tree_four)

#Family size is not part of Decision tree - so no major impact







