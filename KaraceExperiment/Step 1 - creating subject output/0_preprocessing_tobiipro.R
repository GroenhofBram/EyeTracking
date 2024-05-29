library(magrittr) # need to run every time you start R and want to use %>%
library(dplyr)
library(data.table)
library(DataCombine)
library(tidyverse)

setwd("KaraceExperiment/Step 1 - creating subject output") #setting working directory

rm(list = ls()) ## clear global environment
cat("\014") ## clear console

### OPEN EYE DATA AND CLEAN IT ###

## parameters
participant_no <- 58
experiment_type <- "f" # can be f (final), l (last) or m (medial)
if (experiment_type == "l") {
  noftrials <- 32
} else {
  noftrials <- 26
}

## add subject number to tsv file name
file_name <- paste("subject-", "_TOBII_output-ExperimentType.tsv", sep = as.character(participant_no))
## add experiment type to tsv file name
file_name <- gsub("ExperimentType", experiment_type, file_name)

## add subject number to csv file name (os = open sesame)
os_file_name <- paste("subject-", "-ExperimentType.csv", sep = as.character(participant_no))
## add experiment type to csv file name (os = open sesame)
os_file_name <- gsub("ExperimentType", experiment_type, os_file_name)

#create output file names
output_file_name <- paste("subject-", "-ExperimentType-output.csv", sep = as.character(participant_no))
output_file_name <- gsub("ExperimentType", experiment_type, output_file_name)


## read in tsv data
data <- read.delim(file_name, header = TRUE, sep = "\t", skip = 17) #read in text data (first 17 lines are calibration information)

## remove some columns
data <- data[ -c(3:5) ] #information on left eye
data <- data[ -c(6:11) ]
head(data)

## add participant, trial number, and trial start time column
data <- data.frame(participant = participant_no, data) 
data <- data.frame(trial_start = c(0), data)
data <- data.frame(trial = c(0), data)
head(data)

# moves participant column to the beginning
data <- data %>% select(participant, everything())
head(data)

## change column names
names(data)[4] <- "timestamp"
names(data)[5] <- "event"
names(data)[6] <- "x"
names(data)[7] <- "y"
names(data)[8] <- "validity"
head(data)



## clean the data, keep the rows with NA or 1 in validity column
data <- subset(data, validity == 1 | is.na(validity))
head(data)

## round the timestamp column
data$timestamp <- round(as.numeric(data$timestamp), digits = 0)

## reorder timestamp column (because lines before and after start_trial_X in event column is not ordered)
data <- data[order(data$timestamp),]

### WE ARE HERE ### 

## find start of each trial
events <- subset(data, grepl("start_trial", event, fixed=TRUE))
head(events)

## move timestamp column to trial_start column
events$trial_start <- events$timestamp

## find each trial number
events$trial <- as.numeric(gsub("start_trial ", "", events$event))

## increment trial numbers by 1 so first is 1 (not zero)
events$trial <- events$trial + 1

# throw an error number of trials is wrong
if (nrow(events) != noftrials) {
  stop(paste("There are ", as.character(noftrials-nrow(events)), " missing trial/s.", sep = ""))
}

## remove unnecessary columns
events <- events[c("trial", "trial_start")]
head(events)

## assign trial numbers and trial start times to each row
for (row in 1:nrow(events)) {
  
  trial_no <- events[row,]$trial
  trial_start <- events[row,]$trial_start
  trial_end <- events[row+1,]$trial_start-1 # trial_end for each trial will be (trial_start-1) ms of the next trial
  
  if (row < nrow(events)) {
    
    data$trial[data$timestamp >= trial_start & data$timestamp <= trial_end] <- trial_no
    data$trial_start[data$timestamp >= trial_start & data$timestamp <= trial_end] <- trial_start
    
  } else {
    
    data$trial[data$timestamp >= trial_start] <- trial_no
    data$trial_start[data$timestamp >= trial_start] <- trial_start
    
  }
  
}

## remove rows with NA validity
data <- subset(data, !is.na(data$validity))

## remove rows without trial number (these rows are recorded during drift_correct)
data <- subset(data, trial > 0)

## remove event and validity column
data <- data[ -c(5,8) ]

## DATA FROM EYETRACKER IS READY AT THIS POINT
## LINES BELOW THIS POINT WILL MERGE OPENSESAME FILE WITH EYETRACKER DATA

## read in opensesame file
os <- read.delim(os_file_name, header = TRUE, sep = ",") #read in the list that belongs to this participant

## keep necessary columns
os <- os[c("list",
           "trial_order",
           "item",
           "sentence",
           "sentence_audio",
           "verb_position",
           "condition",
           "verb",
           "object1",
           "object2",
           "object3",
           "object1_thematicrole",
           "object2_thematicrole",
           "object3_thematicrole",
           "object1_pic",
           "object2_pic",
           "object3_pic",
           "screen_left",
           "screen_right",
           "screen_bottom",
           "screen_left_role",
           "screen_right_role",
           "screen_bottom_role",
           "animation",
           "animation_enabled",
           "animation_correct_keyboard_response",
           "response_new_keyboard_response_1_2")]

## change column name of "trial order" in os to "trial"
names(os)[2] <- "trial"

## merge eye tracker data with opensesame file
data <- merge(data, os, sort = TRUE)

## save merged data because this is the data that is going to be analyzed throughout figen's phd
#setwd("") #setting working directory
write.table(data, file=output_file_name, sep="\t", quote = FALSE) 
