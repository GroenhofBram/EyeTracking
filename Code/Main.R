# PACKAGES (install first if required!) 
library(tidyverse)

# Directory (open .Rproj file to ensure right one is opened)
setwd("D:/repos/EyeTracking") # This is on my (Bram's) PC

##### Functions
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


# GET DATA
get_filenames()

# MANIPULATE DATA
csv_1114 <- read.csv("Data/subject-1114.csv")
tsv_1114 <- read.delim("Data/subject-1114_TOBII_output.tsv", sep = "\t")
subject_number <- gsub("^Data/subject-(\\d+)\\.csv$", "\\1", data_csv_filenames[1])
trial_count <- 60 
output_file_name <- paste("subject-", "-output.csv", sep = as.character(subject_number))

start_indeces <- grep("start_fix", tsv_1114$Event)
tsv_1114_trials_only <- tsv_1114[(start_indeces[1]):nrow(tsv_1114), ]
tsv_1114_right_and_trials_only <- tsv_1114_trials_only[ -c(3:5) ] #information on left eye
tsv_1114_right_and_trials_only <- tsv_1114_right_and_trials_only[-c(6:11)]

tsv_1114_right_and_trials_only <- tsv_1114_right_and_trials_only %>%
  mutate("participant" = subject_number) %>%
  relocate(participant, .before = 1)

head(tsv_1114_right_and_trials_only)


#karace_example_csv <- read.csv("KaraceExperiment/Step 1 - creating subject output/subject-58-f.csv")
#karace_example_tsv <- read.delim("KaraceExperiment/Step 1 - creating subject output/subject-58_TOBII_output-f.tsv", header =T, sep = "\t", skip = 17)







