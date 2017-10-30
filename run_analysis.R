### Loading the preffered and required library as per learning from the cource

library(data.table)
library(dplyr)

## Download the data from the given url to default library and extracted 


## Reading the data set with read.table function

### Reading feature.txt file
featureNames <- read.table("UCI HAR Dataset/features.txt")
featureNames


### reading activity file
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

activityLabels


#####  Now,

## Reading training Data
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)

activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)


featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)


##### Reading test Data

subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)

activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)

featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)


####
#### Objective 1--Merges the training and the test sets to create one data set.
####


### using rbind function to combining the data set since there are no unique ID

subject <- rbind(subjectTrain, subjectTest)

activity <- rbind(activityTrain, activityTest)

features <- rbind(featuresTrain, featuresTest)


## Now naming of the colums is required using colnames function 

colnames(features) <- t(featureNames[2])

colnames(activity) <- "Activity"

colnames(subject) <- "Subject"

#####

#### Merge the Data


completeData <- cbind(features,activity,subject)


#### Objective one Complete





#### Objective -2 -Extracts only the measurements on the mean and standard deviation for each measurement. 


## Use grep function 
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

requiredColumns <- c(columnsWithMeanSTD, 562, 563)

dim(completeData)

extractedData <- completeData[,requiredColumns]


dim(extractedData)

extractedData <- completeData[,requiredColumns]

dim(extractedData)

### Objective 2 Complete


#### Objective 3- Uses descriptive activity names to name the activities in the data set


### 

extractedData$Activity <- as.character(extractedData$Activity)

for (i in 1:6){
        extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

extractedData$Activity <- as.factor(extractedData$Activity)

## Objective Complete

#### Objective - 4- Appropriately labels the data set with descriptive variable names. 


names(extractedData)

### Now replacing all the all the acronym with the fullform word


names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))

names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))

names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))

names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))

names(extractedData)<-gsub("^t", "Time", names(extractedData))

names(extractedData)<-gsub("^f", "Frequency", names(extractedData))

names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))

names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)

names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)

names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)

names(extractedData)<-gsub("angle", "Angle", names(extractedData))

names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))



#### Checking all the name again
names(extractedData)

#### Objective Complete



######Objective 5- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


### 
extractedData$Subject <- as.factor(extractedData$Subject)

extractedData <- data.table(extractedData)

### Using Aggregate function to bring all the variable in the tidy dataset

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)

tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]

write.table(tidyData, file = "Tidy.txt", row.names = FALSE)

## Objective 5 Complete



