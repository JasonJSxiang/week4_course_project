# Data prep ---------------------------------------------------------------

## 1. load packages ####
library(tidyverse)

## 2. download and load files ####

# check if raw file directory exists, if not, create one
if(!file.exists("raw")) {
    dir.create("raw")
}

# download data to the raw directory
if(!file.exists("raw/zip_file.zip")) {
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    
    download.file(url, "raw/zip_file.zip", method = "curl") 
}

# unzip file
if(!file.exists("raw/UCI HAR Dataset")) {
    unzip("raw/zip_file.zip", exdir = "raw")
}

## 3. file-reading function ####

# Create function that loads data into R (this create a list for test and train
# dataset)
read_files_function <- function(data) {
    # data can only be "test" or "train", check if the input is valid
    if(!data %in% c("test", "train")) {
        stop("invalid data input")
    }
    
    # extract the txt file names in the first-level directory
    file_names_first <- list.files(paste0("raw/UCI HAR Dataset/", data),
                                   pattern = "\\.txt$")
    
    file_names_second <- list.files(paste0("raw/UCI HAR Dataset/", data, "/",
                                           "Inertial Signals"),
                                    pattern = "\\.txt$")
    
    file_names_sum <- c(file_names_first, file_names_second)
    
    # extract the txt files and put them in a list (test and train scenarios)
    if(data == "test") {
        # extract and merge files in the first directory
        test1 <- lapply(file_names_first, function(x) {
            read_table(paste0("raw/UCI HAR Dataset/test/", x),
                       col_names = FALSE)
        })
        
        # extract and merge files in the second directory
        test2 <- lapply(file_names_second, function(x) {
            read_table(paste0("raw/UCI HAR Dataset/test/Inertial Signals/",
                              x),
                       col_names = FALSE)
        })
        
        test <- c(test1, test2)
        
        names(test) <- file_names_sum
        
        test <<- test
        
    } else {
        # extract and merge for first directory
        train1 <- lapply(file_names_first, function(x) {
            read_table(paste0("raw/UCI HAR Dataset/train/", x),
                       col_names = FALSE)
        })
        
        train2 <- lapply(file_names_second, function(x) {
            read_table(paste0("raw/UCI HAR Dataset/train/Inertial Signals/", 
                              x),
                       col_names = FALSE)
        })
        
        train <- c(train1, train2)
        
        names(train) <- file_names_sum
        
        train <<- train
        
    }
}

## 4. load data ####
# load the merged dataset for test
if(!exists("test", where = .GlobalEnv)) {
    read_files_function("test")
}

# load the merged dataset for global
if(!exists("train", where = .GlobalEnv)) {
    read_files_function("train")
}

# load features and info dataset
activity_labels <- read_lines("raw/UCI HAR Dataset/activity_labels.txt") |> 
    str_remove("[1-6]") |> 
    as_tibble() |> 
    mutate(value = tolower(value))

features <- read_lines("raw/UCI HAR Dataset/features.txt") |> 
    as_tibble()

features_info  <- read_lines("raw/UCI HAR Dataset/features_info.txt",
                             skip = 32,
                             n_max = 17) |> 
    as_tibble()



# Data tidy ---------------------------------------------------------------

## main df ####
test_df <- test[[2]]
train_df <- train[[2]]

# format the features info 
features_info <- features_info |> 
    separate(value, into = c("abbr", "full"), sep = ": ")

# clean the features list
features$value <- sapply(features$value, function(x) {
    text <- x |> 
        str_replace("^([0-9]{1,3})",
                    "") |> 
        str_trim() |> 
        str_replace("^t",
                    "time ") |> 
        str_replace("^f",
                    "frequency ") |> 
        str_replace("\\,[0-9]$",
                    "") |> 
        str_replace("BodyAcc",
                    " body acceleration ") |> 
        str_replace_all("-",
                        " ") |> 
        str_replace_all("GravityAcc",
                        "gravity acceleration ")
    
    for(i in seq_along(features_info$full)) {
        search_text <- features_info$abbr[i]
        replace_text <- features_info$full[i]
        
        text <- str_replace(text,
                            fixed(search_text),
                            replace_text)
    }
    
    text <- tolower(text)
})

# extract the formatted col names    
col_names <- features$value

# merge test_df and train_df into w (working file)
w <- bind_rows(
    list(test = test_df, train = train_df),
    .id = "source")

# assign the formatted feature names to col names
names(w)[2:ncol(w)] <- col_names

# extract only cols of mean and standard deviation
sd_cols <- str_which(names(w), "standard deviation")
mean_cols <- str_which(names(w), "mean")
reserved_cols_indices <- c(1, sd_cols, mean_cols)

w <- w[, reserved_cols_indices]

## subject df ####
test_subject_df <- test[[1]] |> 
    rename("subject" = "X1")
train_subject_df <- train[[1]] |> 
    rename("subject" = "X1")

subject_df <- bind_rows(list(test = test_subject_df,
                             train = train_subject_df))

## activities dfs ####
test_activiies_df <- test[[3]] |> 
    rename("activity" = "X1")
train_activities_df <- train[[3]] |> 
    rename("activity" = "X1")

activities_df <- bind_rows(list(test = test_activiies_df,
                                train = train_activities_df))

# assign values to each number in the activities df
activities_df <- activities_df |> 
    mutate(activity = factor(activities_df$activity,
                             levels = as.character(c(1:6)),
                             labels = activity_labels$value))

## col bind all the dfs
result <- bind_cols(c(subject_df, activities_df, w),
                    .name_repair = "check_unique")

# creates a second, independent tidy data set with the average of each variable 
# for each activity and each subject

# create a col list to get the mean of
col_list <- names(result)[4:ncol(result)]

Q5 <- result |>
    group_by(activity, subject) |>
    summarise(across(all_of(col_list), ~mean(., na.rm = TRUE))) 

