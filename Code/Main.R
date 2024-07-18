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
  # curr_subj <<- csv_file_path
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

# Change the eyetracker data (.csv file) and ready it for merging.
prep_eyetracker_data <- function(csv_data){
  # Niewue kolom toevoegen met start- en eindtijden van trials
  events <- csv_data %>%
    select(audio) %>%
    mutate(trial = row_number(),
           start_name = paste0("start_play_", audio),
           offset_name = paste0("offset_", audio),
           start_trial_time = 0,
           end_trial_time = 0)
  
  # Alle benamingen van begin- en eindtijden van trials
  event_names <- unique(c(events$start_name, events$offset_name))
  
  # Vinden van de start- en eindtijden van tirals
  participant_data_events_only <- participant_data %>%
    filter(event %in% event_names | event %in% event_names) %>%
    group_by(event) %>%
    slice_min(timestamp, with_ties = FALSE) %>%
    ungroup()
  
  # Toevoegen van start- en eindtijden van trials
  events <- events %>%
    left_join(
      participant_data_events_only %>% select(event, timestamp),
      by = c("start_name" = "event")
    ) %>%
    mutate(start_trial_time = ifelse(is.na(timestamp), start_trial_time, timestamp)) %>%
    select(-timestamp) %>%
    left_join(
      participant_data_events_only %>% select(event, timestamp),
      by = c("offset_name" = "event")
    ) %>%
    mutate(end_trial_time = ifelse(is.na(timestamp), end_trial_time, timestamp)) %>%
    select(-timestamp)
  
  
  
  is_within_any_interval <- function(timestamp, intervals) {
    any(map_lgl(intervals, ~ timestamp >= .x$start_trial_time & timestamp <= .x$end_trial_time))
  }
  
  # intervals van timestamps die we willen houden
  intervals <- events %>% 
    transmute(start_trial_time, end_trial_time) %>%
    split(1:nrow(.))
  
  # Filteren zodat alleen die timestamps bewaart worden
  participant_data_relevant <- participant_data %>%
    rowwise() %>%
    filter(is_within_any_interval(timestamp, intervals)) %>%
    ungroup()
  
  events_for_merge <- events %>%
    select(audio, trial, start_trial_time)
  
  
  for (i in 1:nrow(participant_data_relevant)) {
    event_value <- participant_data_relevant$event[i]
    matching_row <- which(endsWith(event_value, events$audio))
    
    if (length(matching_row) > 0) {
      participant_data_relevant$trial[i] <- events$trial[matching_row]
      participant_data_relevant$trial_start[i] <- events$start_trial_time[matching_row]
    }
  }
  
  participant_data_relevant <- participant_data_relevant %>%
    mutate(can_look = FALSE,
           looking_at_correct = FALSE,
           pos1 = "placeholder1",
           pos2 = "placeholder2",
           pos3 = "placeholder3",
           pos4 = "placeholder4",
           timestamp_relative = timestamp - trial_start) %>%
    select(participant, trial, trial_start, timestamp, timestamp_relative, everything())
  
  head(participant_data_relevant)
  return(participant_data_relevant)
}

# Export participant data to a .csv
export_participant_data <- function(participant_data, curr_subj){
  output_dir <- "Data/Output"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  

  relative_path <- sub("^[^/]*/", "", curr_subj)
  

  new_curr_subj <- file.path(output_dir, relative_path)
  

  write.csv(participant_data,
            file = new_curr_subj,
            row.names = FALSE)
}

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####

##### RUNNING SCRIPT ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
generate_participant_data_output(){
  
}
#  GET ALL FILENAMES (only when both .csv and .tsv are present)
get_filenames()

# CLEAN UP AND EXPORT DATA
for (participant_no in seq_along(data_csv_filenames)){
  if (participant_no == 1){
    participant_data <<- fix_events(csv_file_path = data_csv_filenames[participant_no],
                                   tsv_file_path = data_tsv_filenames[participant_no])
    csv_data <<- read.csv(data_csv_filenames[participant_no])
    participant_data <<- prep_eyetracker_data(csv_data)
    
    export_participant_data(participant_data,
                            curr_subj = data_csv_filenames[participant_no])
  }
}


##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 






