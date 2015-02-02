#FILE AND LIBRIRY INPUT

library (rpart)  #decision tree library
library(rattle)
library(rpart.plot)  #decision tree plot library
library(RColorBrewer) #decsion tree plot library

setwd("~/Desktop/Data Projects/Kaggle_Titanic")
train <- read.csv("~/Desktop/Data Projects/Kaggle_Titanic/train.csv")
test <- read.csv("~/Desktop/Data Projects/Kaggle_Titanic/test.csv")

# 1. FEATURE ENGINEERING - CREATE TITLES COLUMN

train$Name [1] # access the first row by using the index

test$Survived <- NA
combi <- rbind(train, test)  # stack train and test data sets together for consistent feature engineering
View(combi)

combi$Name <- as.character(combi$Name) #format Name into strings from factor (norminal data) for text manipulation

strsplit (combi$Name[1], split='[,.]' ) [[1]][2]  #string split function to cut out title from the name string; the outcome is a nested list

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split ='[,.]')[[1]][2]}) 
#create a new column in combi table
#extract all title from names by applying split function on all rows and extract the 2nd element from nested list

combi$Title [1]  #the result indicates that there is a space in front of the title value

combi$Title <- sub(' ', '', combi$Title)  #replace all spaces in the Title column

table (combi$Title)  #display the breakdown by title value; find if there are anything we can combine

#combine all similar titles to reduce the factors
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir' )] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'theCountess', 'Jonkheer')] <- 'Lady'
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

combi$Title <- factor(combi$Title)
train <- combi [1:891,]
test <- combi [892:1309,]


# 2. FEATURE ENGINEERING - ADD FAMILY SIZE 


# 3. DEVELOP FIT MODEL, PREDICTION, AND OUTPUT

fit <- rpart(Survived ~ Pclass+Sex+Age+Fare+Title, data = train, method="class")
Prediction <- predict (fit, test, type = 'class')
submit <- data.frame (PassengerID = test$PassengerId, Survived = Prediction)
write.csv (submit, file = "feature_engineered.csv", row.names = FALSE)
fancyRpartPlot (fit)

table(train$Cabin)


