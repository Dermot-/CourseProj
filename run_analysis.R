# run_analysis.R
#
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard
#    deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the
#    data set
# 4. Appropriately labels the data set with descriptive variable
#    names. 

# From the data set in step 4, creates a second, independent tidy
# data set with the average of each variable for each activity and
# each subject.

library(dplyr)

# Load the test data (x, y and subjects) and combine into one table

testpath <- "./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/"
trainpath <- "./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/"
shortpath <- "./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/"

testx <- read.table(paste(testpath,"x_test.txt", sep= ""), header = FALSE,)
testy <- read.table(paste(testpath,"y_test.txt", sep= ""), header = FALSE,)
testsub <- read.table(paste(testpath,"subject_test.txt", sep= ""), header = FALSE,)

alltest <- cbind(testsub,testy,testx)

# Now load and merge the three sets of training data
trainx <- read.table(paste(trainpath,"x_train.txt", sep= ""), header = FALSE,)
trainy <- read.table(paste(trainpath,"y_train.txt", sep= ""), header = FALSE,)
trainsub <- read.table(paste(trainpath,"subject_train.txt", sep= ""), header = FALSE,)
#trainx <- read.table(
#        "./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt",
#        header = FALSE,)
#trainy <- read.table(
#        "./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt",
#        header = FALSE,)
#trainsub <- read.table(
#        "./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt",
#        header = FALSE,)
alltrain <- cbind(trainsub,trainy,trainx)

# Read the 'features' file
features <- read.table(paste(shortpath, "features.txt", sep =""),
         header = FALSE,)

# Read the activities file
activities <- read.table(paste(shortpath, "activity_labels.txt",
         sep = ""), header = FALSE,)

# combine the two data sets into one new set
alldata <- rbind(alltrain, alltest)

# Remove unwanted data sets to save on memory usage
rm(trainx, trainy, trainsub, alltrain, testx, testy, testsub, alltest)

# Apply the supplied column names to the data
names(alldata) <- c("subject","activity", as.character(features[,2]))

# Identify the columns that contain mean or standard deviation data
stdmean <- grep("std",names(alldata))
stdmean <- c(1,2,stdmean, grep("mean",names(alldata)))

# Create a new table with subject and activity and just the mean
# and std dev data
cutdata <- alldata[,stdmean]

# Add descriptive activity names to the data set
cutdata <- mutate(cutdata, act_desc = activities[alldata$activity,2])

# Tidy the Column names
# Remove the (), the parentheses symbols
tempnames <- names(cutdata)
tempnames <- sub("\\(\\)", "", tempnames)
# Remove the "-", dash symbol and 
# capitalise the following s or M in std or mean.
tempnames <- sub("-s", "S", tempnames)
tempnames <- sub("-m", "M", tempnames)
tempnames <- sub("-", "", tempnames)
# expand t and f to Time and Freq
tempnames <- sub("tBody", "TimeBody", tempnames)
tempnames <- sub("tGravity", "TimeGravity", tempnames)
tempnames <- sub("fBody", "FreqBody", tempnames)
names(cutdata) <- tempnames

newtable <- cutdata %>%
        group_by(act_desc, subject) %>%
        summarise_each(funs(mean), -activity)
newt2 <- cutdata %>%
        group_by(act_desc) %>%
        summarise_each(funs(mean), -activity)
newt2$subject = 0

newtable <- rbind(newtable,newt2)

# Write the table to an output file
write.table(cutdata, file = 
      "./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/ra output.txt")

write.table(newtable, file = 
      "./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/NewTable.txt",
      row.name=FALSE)

