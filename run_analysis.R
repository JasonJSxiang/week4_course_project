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

# Create function that loads data into R 
read_files_function <- function(data) {
    # data can only be "test" or "train", check if the input is valid
    if(!data %in% c("test", "train")) {
        stop("invalid data input")
    }
    
    # extract the txt file names
    file_names <- list.files(paste0("raw/UCI HAR Dataset/", data),
                                   pattern = "\\.txt$")
    
    # extract the txt files and put them in a list
    if(data == "test") {
        # extract and merge files in the first directory
        test <- lapply(file_names, function(x) {
            read_table(paste0("raw/UCI HAR Dataset/test/", x),
                       col_names = FALSE)
        })

        names(test) <- file_names
        
        test <<- test
        
    } else {
        # extract and merge for first directory
        train <- lapply(file_names, function(x) {
            read_table(paste0("raw/UCI HAR Dataset/train/", x),
                       col_names = FALSE)
        })
        
        names(train) <- file_names
        
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

# load activity labels
activity_labels <- read_lines("raw/UCI HAR Dataset/activity_labels.txt") |> 
    str_remove("[1-6]") |> 
    as_tibble() |> 
    mutate(value = tolower(value))

# load features 
features <- read_lines("raw/UCI HAR Dataset/features.txt") |> 
    as_tibble()

# load descriptions for features
features_info  <- read_lines("raw/UCI HAR Dataset/features_info.txt",
                             skip = 32,
                             n_max = 17) |> 
    as_tibble()



# Data tidy ---------------------------------------------------------------

## 1. main df ####

# load the main df for both test and train groups
test_df <- test[[2]]
train_df <- train[[2]]

# format the features info so that only relevant lines are kept
features_info <- features_info |> 
    separate(value, into = c("abbr", "full"), sep = ": ")

# clean the features df and expand on abbreviations where applicable 
features$value <- sapply(features$value, function(x) {
    
    # extract each col and conduct preliminary editing with regular expressions
    text <- x |> 
        str_replace("^([0-9]{1,3})", # remove the three leading numbers
                    "") |> 
        str_trim() |> 
        str_replace("^t", # convert the leading "t" to "time"
                    "time ") |> 
        str_replace("^f", # convert the leading "f" to "frequency"
                    "frequency ") |> 
        str_replace("\\,[0-9]$", # remove the tailing comma and number
                    "") |> 
        str_replace("BodyAcc", # replace "BodyAcc" with "body acceleration"
                    " body acceleration ") |> 
        str_replace_all("-", # replace "-" with " "
                        " ") |> 
        str_replace_all("GravityAcc", # replace "GravityAcc" with "gravity acceleration
                        "gravity acceleration ")
    
    # expand all the abbreviated texts
    for(i in seq_along(features_info$full)) {
        search_text <- features_info$abbr[i]
        replace_text <- features_info$full[i]
        
        text <- str_replace(text,
                            fixed(search_text),
                            replace_text)
    }
    
    # convert all the text to lowercase
    text <- tolower(text)
})

# merge the test_df and train_df into w (working file)
w <- bind_rows(
    list(test = test_df, train = train_df),
    .id = "source")

# extract the formatted col names from the features df  
col_names <- features$value

# assign the formatted feature names to col names
names(w)[2:ncol(w)] <- col_names

# extract only cols that include "mean" or "standard deviation"
sd_cols <- str_which(names(w), "standard deviation") # search for col indices for sd
mean_cols <- str_which(names(w), "mean") # search for col indices for mean
reserved_cols_indices <- c(1, sd_cols, mean_cols) # combine the indices and also 
# keeping the first col in the w df which indicates the data source (test or train) 

# apply the col indices to keep only the selected cols
w <- w[, reserved_cols_indices]

## 2. subject df ####
# create the subject df

test_subject_df <- test[[1]] |> # subject df for test group
    rename("subject" = "X1")
train_subject_df <- train[[1]] |> # subject df for train group
    rename("subject" = "X1")

subject_df <- bind_rows(list(test = test_subject_df, # merge two dfs
                             train = train_subject_df))

## 3. activities dfs ####
# create the acitivity df

test_activiies_df <- test[[3]] |> # for test group
    rename("activity" = "X1")
train_activities_df <- train[[3]] |>  # for train group 
    rename("activity" = "X1")

activities_df <- bind_rows(list(test = test_activiies_df, # merged
                                train = train_activities_df))

# label the values in the acctivity df with actual activity labels from the 
# activity_labels df
activities_df <- activities_df |> 
    mutate(activity = factor(activities_df$activity,
                             levels = as.character(c(1:6)),
                             labels = activity_labels$value))

## bind all the dfs (w, subject, activity) to create a final tidy dataset
result <- bind_cols(c(subject_df, activities_df, w),
                    .name_repair = "check_unique")

# creates a second, independent tidy data set with the average of each variable 
# for each activity and each subject

# create a col list of which to get the mean
col_list <- names(result)[4:ncol(result)]

Q5_mean_table <- result |>
    group_by(activity, subject) |>
    summarise(across(all_of(col_list), ~mean(., na.rm = TRUE))) 

write.table(Q5_mean_table, "dataset.csv")

