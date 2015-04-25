### Public Leadership 0.60999
rm(list=ls(all=TRUE))
gc(reset=TRUE)

#library packages used
library(lubridate)
library(caret)
install.packages('readr')
library(readr)
library(randomForest)


setwd('/opt/soft/Bike_Sharing')


# The competition datafiles are in the directory ../input
# Read competition data files:
train <- read.csv("train.csv")
test <- read.csv("test.csv")
head(train)

test$casual <- 1
test$registered <- 1
test$count <- 1
r.train <- nrow(train)
r.test <- nrow(test)

myData <- rbind(train,test)
rm(train,test)
c.mydata <- nrow(myData)

## Feature Engineering

featureEngineering <- function(df){
  #Convert season,  holiday, weather, workingday and weather into factors
  names <- c("season","holiday","weather","workingday")
  df[,names]= lapply(df[,names], factor)
  
  df$datetime <- as.character(df$datetime)
  df$datetime <- strptime(df$datetime, format="%Y-%m-%d %T", tz ="EST")
  
  #convert hours to cosine values
  df$hour <- cos(2*hour(ymd_hms(df$datetime))*pi/24)
  
  #day of the week
  df$weekday <- as.factor(weekdays(df$datetime))
  df$weekday <- factor(df$weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
  
  df$year <- as.integer(substr(df$datetime,1,4))
  df$year <- as.factor(df$year)
  
  #return full features data frame
  return(df)
}

myData <-featureEngineering(myData)
head(myData)
myData$casual <- log(myData$casual + 0.01)
myData$registered <- log(myData$registered + 0.01)

#####RANDOM FOREST STARTS HERE#########
#variables
myNtree = 500
myMtry = 5
myImportance = TRUE
#set the random seed
set.seed(415)
#fit and predict casual
casualFit <- randomForest(casual ~ hour + year + humidity + temp + atemp + workingday + weekday, data=myData[1:r.train,-c(1,11,12)], ntree=myNtree, mtry=myMtry, importance=myImportance)
casual_df <- predict(casualFit, myData[(r.train+1):c.mydata,-c(1,11,12)])
#fit and predict registered
registeredFit <- randomForest(registered ~ hour + year + season + weather + workingday + humidity + weekday + atemp, data=myData[1:r.train,-c(1,10,12)], ntree=myNtree, mtry=myMtry, importance=myImportance)
registered_df <- predict(registeredFit, myData[(r.train+1):c.mydata,-c(1,10,12)])
#add both columns into final count, round to whole number
count <- round(exp(casual_df)+ exp(registered_df) + 0.02, 0)



tail(myData[1:r.train,-c(1,11,12)])
head(myData[(r.train+1):c.mydata,-c(1,10,12)])
name(myData[(r.train+1):c.mydata,c(11)])

myData[1:r.train,-c(1,11,12)]


submission <- data.frame(datetime=myData$datetime[(1+r.train):c.mydata], count=count)
head(submission)
write.csv(submission,"submission_25th_1.csv", row.names=FALSE, quote=FALSE)

# Write to the log:
cat(sprintf("Training set has %d rows and %d columns\n", nrow(train), ncol(train)))
cat(sprintf("Test set has %d rows and %d columns\n", nrow(test), ncol(test)))
