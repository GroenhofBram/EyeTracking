##### VERB FINAL EXPERIMENT DATA ANALYSIS #####

rm(list = ls()) ## clear global environment
cat("\014") ## clear console

##
setwd("") #set the working directory

# read in the eye-tracking data
eye_data <- read.csv("all_adult_data_verbfinal.csv", head=T)

#keep only the relevant columns in this datafile for the paper:

eye_data <- eye_data[c("list",
           "trial",
           "item",
           "participant",
           "sentence_audio",
           "verb_position",
           "condition",
           "verb",
           "fixation_category",
           "trial_time",
           "trial_time_centered")]

#save the datafile as the final eyedata to share on OSF
write.csv(eye_data, file = "eyedata_verbfinal.csv", row.names = FALSE)

#after this point, the script will be shared on OSF

#read in the eyedata
eye_data <- read.csv("eyedata_verbfinal.csv", head=T)

# read in background variables csv file
other_tasks <- read.delim("background.csv", header = TRUE, sep=",")

#merge these two files together
merged_data <- merge(eye_data, other_tasks, sort = TRUE)
