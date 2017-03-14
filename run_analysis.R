
if (!file.exists("./data")){dir.create("./data")}

library(downloader)
download("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip ",dest="./data/dat22.zip", mode="wb") 
unzip ("./data/dat22.zip", exdir = "./data")

library(dplyr)
library(tidyr)

NameMeasurement <- read.table("./data/UCI HAR Dataset/features.txt")
str(NameMeasurement)

# STEP 1:
# PREPARE THE TRAIN DATA
# ----------------------
# Extract the participants information
dat_training <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
str(dat_training)
# Extract activities information
dat_name <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
colnames(dat_name) <- c("Activity_ID", "Activity")
dat <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
str(dat)
# Merge Participants and Activities
dat_training <- cbind(dat_training,dat)
colnames(dat_training) <- c("Subject_ID", "Activity_ID")
total <- merge(dat_training,dat_name,by="Activity_ID") %>%
   select(Subject_ID, Activity)
str(total)
# Extract the measurements
dat_train_m <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
colnames(dat_train_m) <- NameMeasurement[,2]
str(dat_train_m)
# Select the mean and standard deviation of each of the measurement
dat_train_ms <- dat_train_m[ ,c(1:6,41:46,81:86,121:126,161:166,201,202,214,215,227,228,240,241,253,254,266:271,345:350,424:429,503,504,516,517,529,530,542,543)]
str(dat_train_ms)
data_t <- cbind(total,dat_train_ms)
training <- data_t %>%
  gather(measurement, value, -(Subject_ID:Activity)) %>%
  separate(col = measurement, into = c("Feature","Stats", "Axis"), convert = TRUE)
training[,3] <- as.factor(training[,3])
training[,4] <- as.factor(training[,4])
training[,5] <- as.factor(training[,5])
# add a temporary row_ID to fixe issue by spreading the Stats
training$row <- 1:nrow(total)
training <- spread(training, Stats, value) 
training <- select(training, -row)

# STEP 2:
# PREPARE THE TEST DATA
# ----------------------
# Extract the participants information
dat_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
str(dat_test)
# Extract activities information
dat_name <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
colnames(dat_name) <- c("Activity_ID", "Activity")
dat <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
str(dat)
# Merge Participants and Activities
dat_test <- cbind(dat_test,dat)
colnames(dat_test) <- c("Subject_ID", "Activity_ID")
total <- merge(dat_test,dat_name,by="Activity_ID") %>%
  select(Subject_ID, Activity)
str(total)
# Extract the measurements
dat_test_m <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
colnames(dat_test_m) <- NameMeasurement[,2]
str(dat_test_m)
# Select the mean and standard deviation of each of the measurement
dat_test_ms <- dat_test_m[ ,c(1:6,41:46,81:86,121:126,161:166,201,202,214,215,227,228,240,241,253,254,266:271,345:350,424:429,503,504,516,517,529,530,542,543)]
str(dat_test_ms)
data_t <- cbind(total,dat_test_ms)
test <- data_t %>%
  gather(measurement, value, -(Subject_ID:Activity)) %>%
  separate(col = measurement, into = c("Feature","Stats", "Axis"), convert = TRUE)
test[,3] <- as.factor(test[,3])
test[,4] <- as.factor(test[,4])
test[,5] <- as.factor(test[,5])
str(test)

# add a temporary row_ID to fixe issue by spreading the Stats
test$row <- 1:nrow(total)
test <- spread(test, Stats, value) 
test <- select(test, -row)
# STEP 3
# MERGE TRAINING AND TEST
DataSet <- rbind(training, test)
# STEP 4
# CREATE THE DATASET WITH AVERAGE BY GROUPED VARIABLE
DataSet_small <- DataSet %>%
   group_by(Subject_ID, Activity, Feature, Axis) %>%
   summarize(Average = mean(mean), Average_Std = mean(std)) %>%
   print

View(DataSet_small) 

write.table(DataSet_small, file = "./data/Submission_Week4.txt",  row.name=FALSE) 


