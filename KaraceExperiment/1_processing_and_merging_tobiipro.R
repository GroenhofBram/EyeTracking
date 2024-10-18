rm(list = ls()) ## clear global environment
cat("\014") ## clear console

## PREREQUISITE FOR THIS SCRIPT IS TO RUN "0_processing.R" ONLY ONCE SO THAT RAW EYETRACKER DATA IS PROCESSED.


## INPUT DATAFRAMES AND VALUES
## NO INPUT DATAFRAMES NOR VALUES NECESSARY. ONLY INPUTS ARE THE PROCESSED DATA FILES


## OPTIONS
verb <- "f" #

pps_to_read <-  c(21,29,1,2,3,30,31,33,34,35,36,37,40,41,42,43,44,45,46,47,48,49,50,52,53,54,55,56,57,58) 
# participants 21,29,1,2,3 are monolinguals but tested with new eye-tracker, the rest are bilinguals
#43 L missing
#
# #####################################################################################################################
# ## THE SCRIPT
setwd("") #setting working directory
#
pps <- list()
#
for (participant in pps_to_read) {
  file_name_data <- c("subject-", participant, "-", verb, "-output.csv")
  file_name <- paste(file_name_data, collapse = "")
  pps[[participant]] <- read.delim(file_name, header = TRUE, sep="\t") #read in each participant
}
#
data <- do.call("rbind", pps)

## remove unnecessary dataframes
rm(pps)

## remove fillers
data <- subset(data, !is.na(condition))

####################################################################################################################################

## find the fixation locations based on the conditions given below for a screen with 1920x1080 resolution 
## the resolution for T120 was 1280x 1024 

data <- data.frame(fixation_location = c(0), data)

data$fixation_location[data$x > 0 & data$x <= 960 & data$y > 0 & data$y <= 540] <- "screen_left"
data$fixation_location[data$x > 960 & data$x < 1920 & data$y > 0 & data$y <= 540] <- "screen_right"
data$fixation_location[data$x >= 480 & data$x <= 1440 & data$y > 540 & data$y < 1080] <- "screen_bottom"

## remove the regions that are outside the boundaries specified above
data <- subset(data, fixation_location != 0)

## add a new column for trial times
data <- data.frame(trial_time = c(0), data)
data$trial_time <- data$timestamp - data$trial_start

## average length of sentence is 5012 in verb-medial (range = 4566 to 5351 ms) 
## average length of sentence is 5533 in verb final (range = 5117 to 5945)
## last experiment has an approximately 1000 ms silence in the middle of the sentence. Also it includes both verb-medial and verb-final sentences
## finally, there is a 1500 ms silence at the end of each sentence and 200 ms silence at the beginning. 200 ms silence is accounted for in the code line 72. So we
## only substract 1500.
## Below we substract 1500 ms silence at the end of the sentences from average sentence length to find the duration of sentences without the silence at the end. For last experiment, we used average verb-final sentence length (5533) + 800 (additional silence in the last experiment) to find the average length of sentence for last experiment. We used verb final sentences as verb medial sentences are on average shorter.
if (verb == "f") {
  sound_file_end <- 5333 - 1500  
} else if (verb == "m") {
  sound_file_end <- 4812 - 1500 
} else if (verb == "l") {
  sound_file_end <- 6333 - 1500 
}

#the first 2000 ms is preview time, 500 ms fixation time, and 200 ms silence at the beginning of the sound file. That means at the beginning of each recording there is 2700 ms before the sentence begins.
trial <- subset(data, trial_time >= 2700 & trial_time <= 2700 + sound_file_end)

## after below line, sentences start when trial_time is zero
trial$trial_time <- trial$trial_time - 2700

## assign a category to fixation locations (target, firstNP, distractor)
trial <- data.frame(fixation_category = c(0), trial)

trial$fixation_category[trial$fixation_location == "screen_left"] <- trial$screen_left_role[trial$fixation_location == "screen_left"]
trial$fixation_category[trial$fixation_location == "screen_right"] <- trial$screen_right_role[trial$fixation_location == "screen_right"]
trial$fixation_category[trial$fixation_location == "screen_bottom"] <- trial$screen_bottom_role[trial$fixation_location == "screen_bottom"]

####################################################################################################################################

# read secondnp start times from another file for verb different conditions
if (verb == "f") {
  secondnp_start <- read.delim("../exp_verbfinal.csv", header = TRUE, sep=",") ## .. is for moving one directory upwards
} else if (verb == "m") {
  secondnp_start <- read.delim("../exp_verbmedial.csv", header = TRUE, sep=",") ## .. is for moving one directory upwards
} else if (verb == "l") {
  secondnp_start <- read.delim("../exp_last.csv", header = TRUE, sep=",")
} else {
  stop("Invalid verb.")
}

## merge window durations with trial dataframe
trial <- merge(trial, secondnp_start, sort = TRUE)

## create a new column for relative (to NP2) trial times
trial <- data.frame(trial_time_centered = c(0), trial)

## trial times relative to the secondNP/verb start time. (0 is start of secondNP for each trial)
trial$trial_time_centered <- trial$trial_time - trial$secondNP_start

# ## remove unnecessary data frames
rm(data, secondnp_start)

write.csv(trial, file = "tobiipro_dataset_verbfinal.csv", row.names = FALSE)
