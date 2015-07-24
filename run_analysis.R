#run_analysis.R

# Raw data is located in a zip file. 
# File can be downloaded from:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# Unzip file in working directory

library(dplyr)

# Set path varibale relative to working directory to shorten commands
# when reading in data so files don't need to be manually moved to 
# present working directory.

filepath <- "getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset"

# Read in features.txt
features <- read.table(file.path(filepath, "features.txt"))
# Create a character vector using column 2 of features 
# to be used as descriptive column names
features_vec <- as.character(features[,2]) 

# Read in activity labels
activity_labels <- read.table(file.path(filepath, "activity_labels.txt"))

# Read in subject_train and subject_test
subject_train <- read.table(file.path(filepath, "train/subject_train.txt"))
subject_test <- read.table(file.path(filepath, "test/subject_test.txt"))

# Combine the rows of subject_train and subject_test
subject <- bind_rows(subject_train, subject_test)

# Read in x_train and x_test
x_train <- read.table(file.path(filepath, "train/x_train.txt"))
x_test <- read.table(file.path(filepath, "test/x_test.txt"))

# Combine the rows of x_train and x_test
x <- bind_rows(x_train, x_test)

# Read in y_train and y_test
y_train <- read.table(file.path(filepath, "train/y_train.txt"))
y_test <- read.table(file.path(filepath, "test/y_test.txt"))

# Combine the rows of y_train and y_test
y <- bind_rows(y_train, y_test)

# y contains the activities as numbers for each observation -
# join y with activity_labels to convert numbers to decriptive names
activities <- inner_join(y, activity_labels, by = "V1")
# Throw away number column leaving only descriptive names of activities for each observation
activities <- activities[,-1]

# Combine subject, activites and x into complete dataframe
completedata <- cbind(subject, activities, x)

# Label columns of new, complete data frame
colnames(completedata) <- c("Subjects", "Activities", features_vec)

#Extract only columns with mean and standard deviation while keeping Subjects and activities 
extracteddata <- completedata[,grepl("Subjects|Activities|mean()|std()", colnames(completedata))]

#Remove meanFreq()
extracteddata <- extracteddata[,!grepl("meanFreq()", colnames(extracteddata))]

assignmentdata <- group_by(extracteddata, Subjects, Activities) %>% summarise_each(funs(mean))
write.table(assignmentdata, file = "assignmentdata.txt")



