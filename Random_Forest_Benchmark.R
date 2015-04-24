rm(list=ls(all=TRUE))
gc(reset=TRUE)

#library packages used
library(lubridate)
library(caret)
library(readr)

setwd('C:/Users/cdron/Desktop/Cleanup/Misc/kaggle/Bike_Sharing')


# The competition datafiles are in the directory ../input
# Read competition data files:
train <- read.csv("train.csv")
test <- read.csv("test.csv")

## Add hour column
train$hour = cos(2*hour(ymd_hms(train$datetime))*pi/24)
train$count= log(train$count + 0.1)
test$hour = cos(2*hour(ymd_hms(test$datetime))*pi/24)

## Remove the columns which are not identified in test data
train$casual <- NULL
train$registered <- NULL

## public leadership board score is 0.64107

## no. of columns in train & test
c.train <- ncol(train)
c.test <- ncol(test)

##Random Forest
set.seed(24601)
rf <- train(count ~., data=train[,2:c.train],method="rf",
            trControl=trainControl(method="cv",number=5),
            prox=TRUE,allowParallel=TRUE)
print(rf)

##Make prediction
prediction <- predict(rf, test[,2:c.test])

#Make Submission


submission <- data.frame(datetime=test$datetime, count=(exp(prediction)-0.01))
head(submission)
write.csv(submission,"submission_24th_3.csv", row.names=FALSE, quote=FALSE)

# Write to the log:
cat(sprintf("Training set has %d rows and %d columns\n", nrow(train), ncol(train)))
cat(sprintf("Test set has %d rows and %d columns\n", nrow(test), ncol(test)))
