#IMPORT RECURSIVE PARTITIONING AND REGRESSION TREES 

library (rpart)
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

setwd("~/Desktop/Data Projects/Kaggle_Titanic")
train <- read.csv("~/Desktop/Data Projects/Kaggle_Titanic/train.csv")
test <- read.csv("~/Desktop/Data Projects/Kaggle_Titanic/test.csv")

#USE RPART FUNCTION TO ANALYZE RELATIONSHIP BETWEEN SURVIVED AND OTHER VARIABLES
fit <- rpart(Survived ~ Pclass+Sex+Age+Fare, data = train, method="class")

plot (fit)
text(fit)
fancyRpartPlot (fit)

Prediction <- predict(fit, test, type = "class")  #class method outputs 0 and 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv (submit, file = "myfirsttree.csv", row.names = FALSE)   #score = 0.78469


