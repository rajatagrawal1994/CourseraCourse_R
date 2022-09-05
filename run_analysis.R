# Imports the libraries
library(dplyr)
library(stringr)

# Reads the test and train datasets. Also reads the feature mapping
test <- read.table("./test/X_test.txt")
train <- read.table("./train/X_train.txt")

testactivitylabels <- read.table("./test/y_test.txt")
trainactivitylabels <- read.table("./train/y_train.txt")

featname  <- read.table("features.txt", colClasses = "character")

# Maps the activity labels with designated records
names(testactivitylabels) <- "activitylabel"
names(trainactivitylabels) <- "activitylabel"
test <- cbind(test, testactivitylabels)
train <- cbind(train, trainactivitylabels)

# Maps the subjects
testsub <- read.table("./test/subject_test.txt")
trainsub <- read.table("./train/subject_train.txt")
names(testsub) <- "subject"
names(trainsub) <- "subject"
test <- cbind(test, testsub)
train <- cbind(train, trainsub)


# 1. Merges both the train and test datasets into "merged" datasets,
#    and removes the existing test and train datasets
merged <- rbind(test,train)
rm(test,train,testactivitylabels,trainactivitylabels,trainsub,testsub)

# Maps the column names to the merged dataset
featvec <- featname$V2
featvec <- tolower(featvec)
featvec <- gsub("\\()","",featvec)
featvec <- gsub("-","",featvec)
names(merged) <- c(featvec,"activitylabel","subject")
rm(featname,featvec)

# 2. Extracts only the mean and standard deviation columns
mean <- setdiff(grep("mean",names(merged)),grep("meanfreq",names(merged)))
angle <- grep("angle",names(merged))
meanonlycolumns <- setdiff(mean,angle)
stddevaitioncolumns <- grep("std",names(merged))
merged <- merged[,c(meanonlycolumns,stddevaitioncolumns,562,563)]
rm(mean,angle,meanonlycolumns,stddevaitioncolumns)

# 3. Adds descriptive names to content of activity label column
merged$activitylabel <- sub("1","walking",merged$activitylabel)
merged$activitylabel <- sub("2","walking_upstairs",merged$activitylabel)
merged$activitylabel <- sub("3","walking_downstairs",merged$activitylabel)
merged$activitylabel <- sub("4","sitting",merged$activitylabel)
merged$activitylabel <- sub("5","standing",merged$activitylabel)
merged$activitylabel <- sub("6","laying",merged$activitylabel)

# 4. Appropriately labels the data set with descriptive variable names
renamecolumns <- function(x){ colnames(x) <- gsub("^t", "triaxial", colnames(x)); x }
merged <- renamecolumns(merged)
renamecolumns <- function(x){ colnames(x) <- gsub("acc", "acceleration", colnames(x)); x }
merged <- renamecolumns(merged)
renamecolumns <- function(x){ colnames(x) <- gsub("gyro", "gyrometer", colnames(x)); x }
merged <- renamecolumns(merged)
renamecolumns <- function(x){ colnames(x) <- gsub("std", "stdandarddeviation", colnames(x)); x }
merged <- renamecolumns(merged)

# 5.Find average values wrt subject and activity
##---------------------------------------------------
average_values <- merged %>% 
  group_by(activitylabel,subject) %>% 
  summarise_each(funs(mean))

write.table(average_values, "./tidyset.txt", row.names = FALSE)
