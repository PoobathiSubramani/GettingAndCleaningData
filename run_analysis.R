#assignment 4

#paths for the input files/folders
datapath <- "/Users/boopathi/Datascience/R/assignments/assignment4/UCI HAR Dataset/" 
trainingSet <- paste0(datapath,"train/X_train.txt")
trainingLabel <- paste0(datapath,"train/y_train.txt")
trainingSubject <- paste0(datapath, "train/subject_train.txt")
features <- paste0(datapath, "features.txt")
activityLabel <- paste0(datapath, "activity_labels.txt")
testLabel <- paste0(datapath,"test/y_test.txt")
testSet <- paste0(datapath, "test/X_test.txt")
testSubject <- paste0(datapath, "test/subject_test.txt")

#READING the files
#read the input files - training data, features, training label, activity label
trainingSet.f <- NULL; trainingSet.f <- read.table(trainingSet, header = F, sep = "") # training data - contains the measurements
features.f <- NULL; features.f <- read.table(features, header = F, sep = "") # features - contains the headers for training data
trainingLabel.f <- NULL; trainingLabel.f <- read.fwf(trainingLabel, widths = 1) # training label - contains the activity rows
activityLabel.f <- NULL; activityLabel.f <- read.table(activityLabel, header = F, sep = "") # activity label for training label rows
trainingSubject.f <- NULL; trainingSubject.f <- read.table(trainingSubject, header = F, sep = "") #subject rows for training set
testLabel.f <- NULL; testLabel.f <- read.table(testLabel, header = F, sep = "") # test label - contains the activity rows
testSet.f <- NULL; testSet.f <- read.table(testSet, header = F, sep = "") # test data - contains the measurements
testSubject.f <- NULL; testSubject.f <- read.table(testSubject, header = F, sep = "") #subject rows for test set

#clean the header names in the features.f dataset
colnames(features.f) <- c("sno","headerName")
features.f$headerName <- sub("\\(\\)","",features.f$headerName) #replace () with ""
features.f$headerName <- sub(",",":",features.f$headerName) #replace , with ""
features.f$headerName <- sub("\\(","_",features.f$headerName) #replace , with ""
features.f$headerName <- gsub("\\)","",features.f$headerName) #replace , with ""

#update the col names for subject files
colnames(testSubject.f) <- c("subject")
colnames(trainingSubject.f) <- c("subject")

#name the columns in the activityLabel dataset
colnames(activityLabel.f) <- c("activityCode", "activityName")

#add activity name (activityLabel) for each activity code (trainingLabel)
colnames(trainingLabel.f) <- c("activityCode") #apply column name
trainingLabel.f$activityName <- sapply(trainingLabel.f$activityCode,FUN = function(x) activityLabel.f$activityName[x])

#add activity name (activityLabel) for each activity code (testLabel)
colnames(testLabel.f) <- c("activityCode") #apply column name
testLabel.f$activityName <- sapply(testLabel.f$activityCode,FUN = function(x) activityLabel.f$activityName[x])

#update the header names for the trainingSet dataset
colnames(trainingSet.f) <- features.f$headerName

#combine the Activity columns (trainingLable) and Subject column with measuresments (trainingSet)
training <- NULL; training <- cbind(cbind(trainingSubject.f,trainingLabel.f), trainingSet.f)

#update the header names for the testSet dataset
colnames(testSet.f) <- features.f$headerName

#combine the Subect column and Activity columns (testLable)  with measuresments (testSet)
test <- NULL; test <- cbind(cbind(testSubject.f, testLabel.f),testSet.f)

#combine training and test datasets
fulldataset <- rbind(training, test)

#get std and mean columns from the column list
selcols <- NULL; selcols <- features.f[grep("std|mean$|mean-", features.f$headerName),]

#mean and std cols only from the full data set
std_mean_data <-NULL; std_mean_data <- fulldataset[,c("subject","activityName",unlist(selcols$headerName))]


#SUMMARIZE
library(dplyr)
std_mean_data_summary <- std_mean_data %>% group_by_("subject","activityName") %>% 
    summarize_at(.vars = unlist(selcols$headerName),
                 .funs = "mean")
                                   
#update the column names based on the new definition
colnames(std_mean_data_summary) <- sub("^t|^f","mean_",colnames(std_mean_data_summary))                                   

#create a file with the tidy data subset
write.table(std_mean_data_summary, file = paste0(datapath,"data_summary.txt"), row.names = F )


