## To download the files
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
## Save it in current working directory and folder dataweek4
destfile<-paste0(getwd(),"/","dataweek4.zip")
download.file(fileUrl, destfile, method="curl")
#unzip file
unzip("dataweek4.zip",list = TRUE)
setwd("..//UCI HAR Dataset")

## Libraries Used
library(dplyr)
library(data.table)

## Reading files
#Reading supporting Metadata 
##Reading features
featuresNames<-read.table("features.txt")
#Reading activity Labels
activityLabels<-read.table("activity_labels.txt", header = FALSE)

#Reading files for the train set
featuresTrain<-read.table("./train/X_train.txt",header = FALSE)
activityTrain<-read.table("./train/Y_train.txt",header=FALSE)
subjectTrain<-read.table("./train/subject_train.txt",header=FALSE)

bodyAccX_train<-read.table("./train/Inertial Signals/body_acc_x_train.txt",header=FALSE)
bodyAccY_train<-read.table("./train/Inertial Signals/body_acc_y_train.txt",header=FALSE)
bodyAccZ_train<-read.table("./train/Inertial Signals/body_acc_z_train.txt",header=FALSE)
bodyGyroX_train<-read.table("./train/Inertial Signals/body_gyro_x_train.txt",header=FALSE)
bodyGyroY_train<-read.table("./train/Inertial Signals/body_gyro_y_train.txt",header=FALSE)
bodyGyroZ_train<-read.table("./train/Inertial Signals/body_gyro_z_train.txt",header=FALSE)
totalAccX_train<-read.table("./train/Inertial Signals/total_acc_x_train.txt",header=FALSE)
totalAccY_train<-read.table("./train/Inertial Signals/total_acc_y_train.txt",header=FALSE)
totalAccZ_train<-read.table("./train/Inertial Signals/total_acc_z_train.txt",header=FALSE)

#Reading files for the test set
featuresTest<-read.table("./test/X_test.txt", header = FALSE)
activityTest<-read.table("./test/Y_test.txt", header=FALSE)
subjectTest<-read.table("./test/subject_test.txt", header=FALSE)

bodyAccX_test<-read.table("./test/Inertial Signals/body_acc_x_test.txt",header=FALSE)
bodyAccY_test<-read.table("./test/Inertial Signals/body_acc_y_test.txt",header=FALSE)
bodyAccZ_test<-read.table("./test/Inertial Signals/body_acc_z_test.txt",header=FALSE)
bodyGyroX_test<-read.table("./test/Inertial Signals/body_gyro_x_test.txt",header=FALSE)
bodyGyroY_test<-read.table("./test/Inertial Signals/body_gyro_y_test.txt",header=FALSE)
bodyGyroZ_test<-read.table("./test/Inertial Signals/body_gyro_z_test.txt",header=FALSE)
totalAccX_test<-read.table("./test/Inertial Signals/total_acc_x_test.txt",header=FALSE)
totalAccY_test<-read.table("./test/Inertial Signals/total_acc_y_test.txt",header=FALSE)
totalAccZ_test<-read.table("./test/Inertial Signals/total_acc_z_test.txt",header=FALSE)

##Rename colomuns in train set
names(featuresTrain)<-featuresNames$V2
##Rename colomuns in test set
names(featuresTest)<-featuresNames$V2

#rename colomun in activities set in train
names(activityTrain) <- "activity"
#rename colomun in activitys set in test
names(activityTest) <- "activity"

#rename colomun in subject set in train
names(subjectsTrain)<-"subject"
#rename colomun in subject set in test
names(subjectTest)<-"subject"

# add subjects, acitivities and features sets for train set
sub_act_feat_train=cbind(subjectTrain,activityTrain,featuresTrain)
# add subjects, acitivities and features sets for test set
sub_act_feat_test=cbind(subjectTest,activityTest,featuresTest)

#question 1 : subActFeatures_both is the merges the training and the test sets
subActFeatures_both<-rbind(sub_act_feat_train,sub_act_feat_test)
dim(subActFeatures_both)

#question 2 subActMeanStd is extraction from subActFeatures_both on the mean and standard deviation for each measurement
subActMeanStd_both<-subActFeatures_both%>%select(matches('mean|std|activity|subject'))
dim(subActMeanStd_both)

#question 3 : subActMeanStd_both with descriptive activities names
subActMeanStd_both$activity <-as.character(subActMeanStd_both$activity)
for (i in 1:6){subActMeanStd_both$activity[subActMeanStd_both$activity == i] <-as.character(activityLabels[i,2])}
##subActFeatures_both_descAct <- subActFeatures_both %>% arrange(activities) %>% mutate(activities = as.character(factor(activities, levels=1:6, labels= activities_names$V2)))
## to check: names fo the variables in subActMeanStd_both
names(subActMeanStd_both)

#question 4:  Appropriately labels the data set with descriptive variable names
## Examining the Data, the following acronyms can be replaced:
##Acc can be replaced with Accelerometer
##Gyro can be replaced with Gyroscope
##BodyBody can be replaced with Body
##Mag can be replaced with Magnitude
##Character f can be replaced with Frequency
##Character t can be replaced with Time

names(subActMeanStd_both)<-gsub("Acc", "Accelerometer", names(subActMeanStd_both))
names(subActMeanStd_both)<-gsub("Gyro", "Gyroscope", names(subActMeanStd_both))
names(subActMeanStd_both)<-gsub("BodyBody", "Body", names(subActMeanStd_both))
names(subActMeanStd_both)<-gsub("Mag", "Magnitude", names(subActMeanStd_both))
names(subActMeanStd_both)<-gsub("^f", "Frequency", names(subActMeanStd_both))
names(subActMeanStd_both)<-gsub("tBody", "TimeBody", names(subActMeanStd_both))
names(subActMeanStd_both)<-gsub("-mean()", "Mean", names(subActMeanStd_both), ignore.case = TRUE)
names(subActMeanStd_both)<-gsub("-std()", "STD", names(subActMeanStd_both), ignore.case = TRUE)
names(subActMeanStd_both)<-gsub("-freq()", "Frequency", names(subActMeanStd_both), ignore.case = TRUE)

#to check: names of the variables in subActMeanStd_both
names(subActMeanStd_both)

#question 5 tidy data set with the average of each variable for each activity and each subject
# set subject as a factor variable
subActMeanStd_both$subject <-as.factor(subActMeanStd_both$subject)
subActMeanStd_both <-data.table(subActMeanStd_both)

# tidydata is created as a data set with average for each activity and subject. Later, 
#it is ordered and write it to data fily that contains the processed data.
tidydata <-aggregate(.~subject + activity, subActMeanStd_both ,mean)
tidydata <-tidydata[order(tidydata$subject,tidydata$activity),]
write.table(tidydata, file= "Tidy.txt",row.names = FALSE)