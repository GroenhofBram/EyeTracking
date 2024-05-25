# PACKAGES (install first if required!) 
library(tidyverse)

# Directory (open .Rproj file to ensure right one is opened)
setwd("D:/repos/EyeTracking") # This is on my (Bram's) PC

# # # # # NOTE: FUNCTIONS AT THE TOP SO YOU CAN ALWAYS RUN THE CODE # # # # #
# # # # #   USING CRTL + A --> CTRL + ENTER                         # # # # #
##### Functions ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
# Find all .csv/.tsv files
get_filenames <- function(){
  data_csv_filenames <- list.files(path = "Data", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
  data_tsv_filenames <- list.files(path = "Data", pattern = "*.tsv", full.names = TRUE, recursive = TRUE)
  filter_filenames(data_csv_filenames, data_tsv_filenames)
}

# Filter the lists so that only the subjects with corresponding .csv AND .tsv files remain.
filter_filenames <- function(data_csv_filenames, data_tsv_filenames){
  csv_subject_numbers <- gsub("^Data/subject-(\\d+)\\.csv$", "\\1", data_csv_filenames)
  tsv_subject_numbers <- gsub("^Data/subject-(\\d+)_TOBII_output\\.tsv$", "\\1", data_tsv_filenames)
  
  common_subject_numbers <- intersect(csv_subject_numbers, tsv_subject_numbers)
  
  
  data_csv_filenames <<- data_csv_filenames[csv_subject_numbers %in% common_subject_numbers]
  data_tsv_filenames <<- data_tsv_filenames[tsv_subject_numbers %in% common_subject_numbers]
}

fix_events <- function(csv_file_path, tsv_file_path){
  csv_data <- read.csv(csv_file_path)
  tsv_data <- read.delim(tsv_file_path, sep = "\t")
  subject_number <- gsub("^Data/subject-(\\d+)\\.csv$", "\\1", data_csv_filenames[1])
  trial_count <- 60 
  output_file_name <- paste("subject-", "-output.csv", sep = as.character(subject_number))
  
  start_indeces <- grep("start_fix", tsv_data$Event)
  tsv_data_trials_only <- tsv_data[(start_indeces[1]):nrow(tsv_data), ]
  
  # Creating a data frame of the events with their time intervals, this way I can
  #   add them to the existing data frame as a column
  tsv_data_events <- tsv_data %>%
    filter(Event != "") %>%
    tail(-12) %>%
    select(TimeStamp, Event) %>%
    rename(
      timestamp = TimeStamp,
      event = Event
    ) %>%
    mutate(max_event_timestamp = if_else(row_number() == n(),
                                         timestamp * 2,
                                         lead(timestamp) - 1))
  
  ## Using both eye data for now, filtering can always be done later
  tsv_data_trials_only <- tsv_data_trials_only %>%
    mutate("participant" = subject_number) %>%
    mutate(trial = 0, trial_start = 0) %>%
    relocate(participant, trial, trial_start, .before = 1) %>%
    rename(
      timestamp = TimeStamp,
      event = Event,
      x_left = GazePointXLeft,
      y_left = GazePointYLeft,
      validity_left = ValidityLeft,
      x_right = GazePointXRight,
      y_right = GazePointYRight,
      validity_right = ValidityRight
    ) %>%
    select(participant, trial, trial_start, timestamp, event, x_left, y_left,
           validity_left, x_right, y_right, validity_right) %>%
    filter(!is.na(x_left), validity_left == 1, validity_right == 1)
  
  
  
  tsv_data_trials_only <- tsv_data_trials_only %>%
    rowwise() %>%
    mutate(event = correct_event_data(timestamp, tsv_data_events)) %>%
    arrange(timestamp)
  
  
  return(tsv_data_trials_only)
}

# This function enters the right values in the "event" column in tsv_1114_trials_only,
#   based on timestamps from tsv_1114_events. It also calls fix_events()
correct_event_data <- function(ts, events_df) {
  event_row <- events_df %>%
    filter(ts >= timestamp & ts <= max_event_timestamp) %>%
    select(event)
  
  if (nrow(event_row) == 0) {
    return(NA)
  } else {
    return(event_row$event)
  }
}

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####

##### RUNNING SCRIPT ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
# GET DATA
get_filenames()

# CLEAN UP DATA
participant_data <- fix_events(csv_file_path = "Data/subject-1114.csv",
                                  tsv_file_path = "Data/subject-1114_TOBII_output.tsv")
head(participant_data)


# 
csv_data <- read.csv("Data/subject-1114.csv")
tsv_data <- read.delim("Data/subject-1114_TOBII_output.tsv", sep = "\t")

events <- csv_data %>%
  select(audio) %>%
  mutate(trial = row_number())







##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 



