##----------------------------------------------------------------------------

## Author: Morta
## Date: 2018-10-31
## Version: 1
## Code for Coursera, Getting and Cleaning Data Course Project

##----------------------------------------------------------------------------

#uploading needed libraries
library(tidyverse)

# you can skip data upload part if you already have data on your working 
# directory
data_folder <- "data"

if (!file.exists(data_folder)){
    dir.create(data_folder)
}

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              "data/wf.zip")

unzip("data/wf.zip", exdir = data_folder)

# creating paths for all needed files
test_folder <- "data/UCI HAR Dataset/test"
train_folder <- "data/UCI HAR Dataset/train"
features_folder <- "data/UCI HAR Dataset"

# reading data
test_x <- read.csv(file.path(test_folder, "X_test.txt"),
                   header = FALSE,
                   sep = "")

test_y <- read.csv(file.path(test_folder, "y_test.txt"),
                   header = FALSE,
                   sep = "")

test_subject <- read.csv(file.path(test_folder, "subject_test.txt"),
                         header = FALSE,
                         sep = "")

train_x <- read.csv(file.path(train_folder, "X_train.txt"),
                    header = FALSE,
                    sep = "")

train_y <- read.csv(file.path(train_folder, "y_train.txt"),
                    header = FALSE,
                    sep = "")

train_subject <- read.csv(file.path(train_folder, "subject_train.txt"),
                          header = FALSE,
                          sep = "")

features <- read.csv(file.path(features_folder, "features.txt"),
                     header = FALSE,
                     sep = "")

activity_labels <- read.csv(file.path(features_folder, "activity_labels.txt"),
                            header = FALSE,
                            sep = "")

# adding names for columns
colnames(test_x) <- features$V2
colnames(test_y) <- "ycol"
colnames(test_subject) <- "subcol"

colnames(train_x) <- features$V2
colnames(train_y) <- "ycol"
colnames(train_subject) <- "subcol"

# combining test and train datasets
test <- cbind(test_y, test_subject, test_x)
train <- cbind(train_y, train_subject, train_x)

full_data <- bind_rows(test, train)

full_data <- as.tbl(full_data)

# leaving only features with mean and std
mean_and_sd <- full_data %>%
    select(ycol, subcol, contains("mean()"), contains("std()"))

# checking how all different names look like, that I could decide what is needed
unique(names(mean_and_sd))

#creating a tidy dataset
mean_and_sd_tidy <- mean_and_sd %>%
    # adding activity names from activity dataset
    left_join(activity_labels, 
              by = c("ycol" = "V1")) %>%
    # renaming activity and subject columns
    rename(activity = V2, 
           subject = subcol) %>%
    # removing activity ID
    select(-ycol) %>%
    # I wanted to work with strings as values in columns, so I gathered the data,
    # spread the values by "-" and then worked with them in such form (for me it
    # was easier to rename everything)
    gather("dimension", "measure", -(c(activity, subject))) %>%
    separate(dimension, 
             into = c("signal", "value", "direction"), 
             sep = "-") %>%
    mutate(domaintype = factor(str_sub(signal, 1, 1)),
           signal = factor(str_sub(signal, 2)),
           value = factor(value),
           direction = factor(direction)) %>%
    # renaming all values to more understandable ones
    mutate(domaintype = ifelse(domaintype == "t", "Time", "Frequency"),
           signal = str_replace_all(signal, "Acc", "Accelerometer"),
           signal = str_replace_all(signal, "Gyro", "Gyroscope"),
           signal = str_replace_all(signal, "Mag", "Magnitude"),
           signal = str_replace_all(signal, "BodyBody", "Body"),
           value = ifelse(value == "mean()", "Mean", "StDev"),
           direction = ifelse(is.na(direction), 
                              "withoutaxis", 
                              paste0(direction, "axis"))) %>%
    # bringing columns back to one
    unite(measuretospread, 
          c("domaintype", "signal", "value", "direction"), 
          sep = "") %>%
    # I wasn't able to spread that column back to separate columns due to the 
    # fact that there are several records for each (as it should be) so I created
    # a dummy variable, which would separate all those records
    group_by(activity, subject, measuretospread) %>%
    mutate(tospreadback = rank(row_number())) %>%
    # spreading back to columns
    spread(measuretospread, measure) %>%
    # removing dummy variable
    select(-tospreadback) %>%
    ungroup()


# creating summary dataset (mean of each variable, grouped by activity and subject)
summary_dataset <- mean_and_sd_tidy %>%
    group_by(activity, subject) %>%
    summarize_all(mean)

# adding an indication that the measures are actually means
names(summary_dataset) <- c("activity", "subject", paste0("MeanOf", names(summary_dataset[,-c(1,2)])))


# creating a txt file with summary dataset
write.table(summary_dataset,
            "tidy_summarized.txt", 
            row.name=FALSE)
