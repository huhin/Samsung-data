##The purpose of this script is to collect and clean the samsung data set and to prepare tidy data that can be used for later analysis.
setwd("~/R")

##collect data from link and save into local drive
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file (fileurl, "dataset.zip")
unzip ("dataset.zip")

##merge the training and test sets to create one data set
testfile= read.table ("./UCI HAR Dataset//test/X_test.txt")
trainfile = read.table ("./UCI HAR Dataset//train/X_train.txt")
featureList = read.table ("./UCI HAR Dataset//features.txt")
for (i in 1: length(names (testfile))) {
  names(testfile)[i] <- as.character(featureList[i,2])
  names (trainfile)[i]<-as.character(featureList[i,2])
}

##extract only mean and standard deviation for each measurement
feature_mean <- grepl("-mean()", featureList[,2] )
feature_std<- grepl ("-std()", featureList[,2])
feature_mean_std= (feature_mean + feature_std)==1
test_mean_std = as.data.frame(testfile[,feature_mean_std])
train_mean_std = as.data.frame(trainfile[,feature_mean_std])
mergedData= rbind (test_mean_std, train_mean_std)

##use descriptive activity names to name the activities in the data set
subject_test<- read.table ("./UCI HAR Dataset//test/subject_test.txt")
activity_test<- read.table("./UCI HAR Dataset//test/y_test.txt")
subject_train<- read.table ("./UCI HAR Dataset//train/subject_train.txt")
activity_train<- read.table("./UCI HAR Dataset//train/y_train.txt")
subject_all <- rbind (subject_test, subject_train)
activity_all <- rbind (activity_test, activity_train)
mergedAll= cbind (mergedData, c(subject_all, activity_all))
names(mergedAll)[80]<- "subject"
names(mergedAll)[81]<- "activity"
activity_label<- read.table ("./UCI HAR Dataset/activity_labels.txt")
library (plyr)
mergedAll[,81] <-mapvalues (mergedAll[,81],from= c(1:6), to = c("walking", "walking_up", "walking_down", "sitting", "standing", "laying"))

##create a second, independent tidy data set with average of each variable for each activity and each subject
mergedAll[,82]<-paste (mergedAll[,80], mergedAll[,81], sep="+")
temp_all =NULL
for (i in 1:79){
  temp <-tapply (mergedAll[,i], mergedAll[,82], mean)
  temp_all <- cbind (temp_all, temp)
}
mergedFinal <- data.frame (temp_all)
for (i in 1: 79) {
  names(mergedFinal)[i] <- names (mergedAll[i])
}
###write.csv (mergedFinal, file="mergedFinal.csv", sep=",")
write.table (mergedFinal, file="mergedFinal.txt", col.names=NA,sep="\t")
