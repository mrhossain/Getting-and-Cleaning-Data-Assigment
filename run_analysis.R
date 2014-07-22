#This is Getting and Cleaning Data Project 
#I try to follow all instruction from Instructor

#Here is my Implementation

# Q1. Merges the training and  test  data sets.

#load Train Data

#trainData <- read.table("C:\\Users\\Razib\\rp\\UCI HAR Dataset\\train\\X_train.txt");
trainData <- read.table("train\\X_train.txt")


trainlab <- read.table("train\\y_train.txt")
#trainlab <- read.table("C:\\Users\\Razib\\rp\\UCI HAR Dataset\\train\\y_train.txt")

trainsub <- read.table("train\\subject_train.txt")
#trainsub <- read.table("C:\\Users\\Razib\\rp\\UCI HAR Dataset\\train\\subject_train.txt")

#End Train Data Load

#Load Test Data

testData <- read.table("test\\X_test.txt")
#testData <- read.table("C:\\Users\\Razib\\rp\\UCI HAR Dataset\\test\\X_test.txt")

testlab <- read.table("test\\y_test.txt")
#testlab <- read.table("C:\\Users\\Razib\\rp\\UCI HAR Dataset\\test\\y_test.txt") 
testsub <- read.table("test\\subject_test.txt")
#testsub <- read.table("C:\\Users\\Razib\\rp\\UCI HAR Dataset\\test\\subject_test.txt")

#End Test Data Load

#Join train & test Data set

joinData <- rbind(trainData, testData)
joinLabel <- rbind(trainlab, testlab) 
joinSubject <- rbind(trainsub, testsub)
 

#End Tasks 1



# Q2. Extracts the mean and standard deviation for each.

#Load Features
#features <- read.table("C:\\Users\\Razib\\rp\\UCI HAR Dataset\\features.txt")
features <- read.table("features.txt")

mean_and_Std_idx <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(mean_and_Std_idx) 
joinData <- joinData[,mean_and_Std_idx]

names(joinData) <- gsub("\\(\\)", "", features[mean_and_Std_idx, 2])
names(joinData) <- gsub("mean", "Mean", names(joinData))
names(joinData) <- gsub("std", "Std", names(joinData))
names(joinData) <- gsub("-", "", names(joinData))

#End Tasks




# Q3.Applied descriptive activity names to name the activities in the data set.

activity <- read.table("activity_labels.txt")
#activity <- read.table("C:\\Users\\Razib\\rp\\UCI HAR Dataset\\activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activitylab <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activitylab
names(joinLabel) <- "activity"

#End Tasks


# Q4 Appropriately labels the data set with descriptive activity names. 

names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)
#write.table(cleanedData, "merged_data.txt") 
write.table(cleanedData, "C:\\Users\\Razib\\rp\\merged_data.txt") 

#End Tasks


# Q5. Create second independent tidy data set with average each variable for each activity subject.


library(reshape2)

activity_labels <- #read.table("C:\\Users\\Razib\\rp\\UCI HAR Dataset\\activity_labels.txt",col.names=c("activity_id","activity_name"))
activity_labels <- read.table("activity_labels.txt",col.names=c("activity_id","activity_name"))


#features <- read.table("C:\\Users\\Razib\\rp\\UCI HAR Dataset\\features.txt")
features <- read.table("features.txt")
feature_names <-  features[,2]


#testdata <- read.table("C:\\Users\\Razib\\rp\\UCI HAR Dataset\\test\\X_test.txt")
testdata <- read.table("test\\X_test.txt")
colnames(testdata) <- feature_names


#traindata <- read.table("C:\\Users\\Razib\\rp\\UCI HAR Dataset\\train\\X_train.txt")
traindata <- read.table("train\\X_train.txt")
colnames(traindata) <- feature_names

#test_subject_id <- read.table("C:\\Users\\Razib\\rp\\UCI HAR Dataset\\test\\subject_test.txt")
test_subject_id <- read.table("test\\subject_test.txt")
colnames(test_subject_id) <- "subject_id"
#test_activity_id <- read.table("C:\\Users\\Razib\\rp\\UCI HAR Dataset\\test\\y_test.txt")
test_activity_id <- read.table("test\\y_test.txt")
colnames(test_activity_id) <- "activity_id"


#train_subject_id <- read.table("C:\\Users\\Razib\\rp\\UCI HAR Dataset\\train\\subject_train.txt")
train_subject_id <- read.table("train\\subject_train.txt")
colnames(train_subject_id) <- "subject_id"


#train_activity_id <- read.table("C:\\Users\\Razib\\rp\\UCI HAR Dataset\\train\\y_train.txt")
train_activity_id <- read.table("train\\y_train.txt")
colnames(train_activity_id) <- "activity_id"

test_data <- cbind(test_subject_id , test_activity_id , testdata)
train_data <- cbind(train_subject_id , train_activity_id , traindata)
all_data <- rbind(train_data,test_data)
mean_col_idx <- grep("mean",names(all_data),ignore.case=TRUE)
mean_col_names <- names(all_data)[mean_col_idx]
std_col_idx <- grep("std",names(all_data),ignore.case=TRUE)
std_col_names <- names(all_data)[std_col_idx]
meanstddata <-all_data[,c("subject_id","activity_id",mean_col_names,std_col_names)]
descrnames <- merge(activity_labels,meanstddata,by.x="activity_id",by.y="activity_id",all=TRUE)
data_melt <- melt(descrnames,id=c("activity_id","activity_name","subject_id"))
mean_data <- dcast(data_melt,activity_id + activity_name + subject_id ~ variable,mean)
#write.table(mean_data,"C:\\Users\\Razib\\rp\\tidy_mean_data.txt")
write.table(mean_data,"tidy_mean_data.txt")

#End This Assigment




