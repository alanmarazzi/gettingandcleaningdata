# Script Coursera

# Open the main tables, train and test datasets, in two different variables
X_train <- read.table("~/R/Coursera/Esame Getting and cleaning data/UCI HAR Dataset/train/X_train.txt", 
quote="\"", comment.char="")
X_test <- read.table("~/R/Coursera/Esame Getting and cleaning data/UCI HAR Dataset/test/X_test.txt", 
quote="\"", comment.char="")

# Merge the datasets into one dataframe appending rows
data <- rbind(X_train, X_test)

# It's always a good habit to make room in memory for new variables removing unused ones
rm(X_test)
rm(X_train)

# Subset the dataframe keeping only the columns measuring mean and sd. The number of the columns can be retrieved in features.txt
require(dplyr)
data <- select(data, num_range("V", c(1:6, 41:46, 81:86, 161:166, 201:202, 214:215, 227:228, 
240:241, 253:254, 266:271, 345:350, 424:429, 503:504, 516:517, 529:530, 542:543), width = 1))

# Load activity labels in R
labstrain <- read.table("~/R/Coursera/Esame Getting and cleaning data/UCI HAR Dataset/train/y_train.txt", 
quote="\"", comment.char="")
labstest <- read.table("~/R/Coursera/Esame Getting and cleaning data/UCI HAR Dataset/test/y_test.txt", 
quote="\"", comment.char="")

# Merge activity labels into one dataframe
activity <- rbind(labstrain, labstest)

# Add a column to the original dataframe with activity codes
data$activity <- as.vector(activity$V1)

# Always make rook in memory
rm(activity, labstest, labstrain)

# Load subject codes in R
subjtrain <- read.table("~/R/Coursera/Esame Getting and cleaning data/UCI HAR Dataset/train/subject_train.txt", 
quote="\"", comment.char="")
subjtest <- read.table("~/R/Coursera/Esame Getting and cleaning data/UCI HAR Dataset/test/subject_test.txt", 
quote="\"", comment.char="")

# Merge subject codes
subj <- rbind(subjtrain, subjtest)

# Add a column to the dataset with subject codes
data$subj <- as.vector(subj$V1)

# Always make room
rm(subj, subjtest, subjtrain)

# Read activity labels in R and add a column to data with the proper activity label
actlabs <- read.table("~/R/Coursera/Esame Getting and cleaning data/UCI HAR Dataset/activity_labels.txt", 
quote="\"", comment.char="")
actlabs <- rename(actlabs, activity = V1)
data <- merge(data, actlabs, by = "activity", all.x = TRUE)
rm(actlabs)

# Get the average of every feature grouped by subject and activity
data <- data %>% group_by(V2.y, subj) %>% summarize_each(funs(mean))
data <- rename(data, V2 = V2.x)

# Read the features table in R and rename the columns of the dataset with their corresponding feature
features <- read.table("~/R/Coursera/Esame Getting and cleaning data/UCI HAR Dataset/features.txt", 
quote="\"", comment.char="", stringsAsFactors=FALSE)
features$V1 <- as.character(features$V1)
features$V1 <- paste("V", features$V1, sep = "")
features <- filter(features, V1 %in% names(data))
names(data)[4:63] <- features$V2
data <- rename(data, 
                activity = V2.y,
                activitycode = activity
                )
rm(features)

# Melt the dataset by activity and subject to a tall form
library(reshape2)
data <- select(data, -activitycode)
data <- melt(data, id.vars = c("activity", "subj"))

# Write the new tidy dataset as a txt table
write.table(data, "acc_tidy.txt", sep = " ", row.names = FALSE)


# The assignment is done, but just for fun I added a plot of the dataset
library(ggplot2)
gg <- ggplot(data, aes(variable, value, colour = as.factor(subj)))+geom_point()+
facet_wrap(~activity)+theme(
legend.position = "none", axis.text.x = element_blank(), panel.grid = element_blank(), 
panel.background = element_blank(), axis.ticks.x = element_blank())

pdf("accelerometer.pdf", w = 15, h = 12)
gg
dev.off()

# After running the script you will find 2 new files in your working directory: the tidy dataset in a txt file, and a pdf of the plot.
