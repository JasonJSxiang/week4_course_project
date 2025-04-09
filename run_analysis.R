# load packages
library(tidyverse)

# check if raw file directory exists, if not, create one
if(!file.exists("raw")) {
    dir.create("raw")
}

# download data to the raw directory

# only run if the file has already been downloaded
if(!file.exists("raw/zip_file.zip")) {
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    
    download.file(url, "raw/zip_file.zip", method = "curl") 
}

# unzip file
if(!file.exists("raw/UCI HAR Dataset")) {
    unzip("raw/zip_file.zip", exdir = "raw")
}

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
            read_lines(paste0("raw/UCI HAR Dataset/test/", x))
        })
        
        # extract and merge files in the second directory
        test2 <- lapply(file_names_second, function(x) {
            read_lines(paste0("raw/UCI HAR Dataset/test/Inertial Signals/",
                              x))
        })
        
        test <- c(test1, test2)
        
        names(test) <- file_names_sum
        
        test <<- test
        
    } else {
        # extract and merge for first directory
        train1 <- lapply(file_names_first, function(x) {
            read_lines(paste0("raw/UCI HAR Dataset/train/", x))
        })
        
        train2 <- lapply(file_names_second, function(x) {
            read_lines(paste0("raw/UCI HAR Dataset/train/Inertial Signals/", 
                              x))
        })
        
        train <- c(train1, train2)
        
        names(train) <- file_names_sum
        
        train <<- train
        
    }
}

# load the merged dataset for test
if(!exists("test", where = .GlobalEnv)) {
read_files_function("test")
}

# load the merged dataset for global
if(!exists("train", where = .GlobalEnv)) {
read_files_function("train")
}
