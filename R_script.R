# 1.SET UP ENVIRONMENT AND IMPORT DATA

setwd("~/Desktop/Data Projects/Kaggle_Titanic")
train <- read.csv("~/Desktop/Data Projects/Kaggle_Titanic/train.csv")
test <- read.csv("~/Desktop/Data Projects/Kaggle_Titanic/test.csv")
View(train)
View(test)

# 2.BASIC ANALYSIS ON TRAIN DATA SET

table(train$Survived)  #live-dead breakdown for categorical data
prop.table(table(train$Survived))  #lived-dead statistics


# 3.MAKE SOME SIMBLE PREDICTION AND APPEND TO TEST DATA SET

test$Survived <- rep(0, 418) #insert 0 to new column Survived 418 times based on row counts in Environment

# 4. OUTPUT TEST DATA SET FOR KAGGLE SUBMISSION

submit <- data.frame(PassengerID = test$PassengerId, Survived = test$Survived)
write.csv (submit, file = "theyallperish.csv", row.names=FALSE)

# 5. MORE ACCURATE MODEL - ANALYSIS ON GENDER AND SURVIVAL STATS

summary (train$Sex) #same value breakdown as table(train$Sex)
prop.table(table(train$Sex, train$Survived)) #notice numbers do not add up to 100. This is because this function take each entry in the table and divides by total number of rows

prop.table(table(train$Sex, train$Survived), 1) #this will return % proportion per groups (ie. female & male)

# 6. REFINE THE PREDICTION BY TAKING GENDER INTO ACCOUNT
test$Survived <- 0 #assign all survival status to 0
test$Survived [test$Sex =='female'] <- 1  #alter the survival status to 1 if the passenger is female

submit <-data.frame(PassengerID = test$Passenger, Survived = test$Survived)
write.csv (submit, file = "ladyallsurvived.csv", row.names = FALSE)


# 7. REFINE THE PREDICITON BY ADDING IN AGE VARIABLE
summary(train$Age) #breakdown of continuous data (it's almost useless to draw proportion table)

# it's difficult to associate continuous data. let's create some bins

train$Child <- 0   #create a bin for Child since they were likely survived
train$Child [train$Age < 18] <- 1  

aggregate (Survived ~ Child + Sex, data = train, FUN=sum)  #the result is the sum of the number of child survivers (Survived = 1) in all combinatin of Age and Sex
aggregate (Survived ~ Child + Sex, data = train, FUN=length) #the result is the total child counts of record in each combination by Age and Sex

#now we want to know the proportion of child survival in each Age/Sex category 
aggregate (Survived ~ Child + Sex, data = train, FUN = function(x) {sum(x)/length(x)})

#result tells that female children are more likely to survive, which does not add net new parameter to our old prediciton

# 8. REFINE THE PREDICTION BY USING CLASS and FARE
summary (train$Fare)  #the result indicates there are 3 main bins for the continuous fare variable
prop.table(table(train$Pclass)) #the result indicates that there are more 

train$Fare2 <- '30+'
train$Fare2 [train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2 [train$Fare < 20 & train$Fare >= 10 ] <- '10-20'
train$Fare2 [train$Fare <10] <-'<10'

aggregate (Survived ~ Pclass + Fare2 + Sex, data = train, FUN = function (x) {sum(x)/length(x)})
# the result indicates that female in 3rd class who paid more than $20 fare didn't do so well

test$Survived <- 0
test$Survived [test$Sex == 'female'] <- 1
test$Survived [test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <-0

submit <- data.frame (PassengerID = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "someladyperish.csv", row.names=FALSE)









