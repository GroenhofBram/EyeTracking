# PACKAGES (install first if required!) 
library(tidyverse)
library(stringr)

# # # # # NOTE: FUNCTIONS AT THE TOP SO YOU CAN ALWAYS RUN THE CODE # # # # #
# # # # #   USING CRTL + A --> CTRL + ENTER                         # # # # #
# # # # # NOTE: ENSURE PROPER FILE STRUCTURE                        # # # # #


################################################################################
################################################################################
# PART I: Generates YAOA_merged.csv that contains all information of trials per
#         participant, using YAOA.txt
################################################################################
################################################################################
# Directory 
setwd("D:/repos/EyeTracking") # This is on my (Bram's) PC


##### Functions ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
# Find and read in YAOA.txt
read_and_format_YAOA <- function(){
  participant_item_data <- read.table("Data/001_Part1/Input/YAOA.txt", header = TRUE)

  
  # make variable test, and remove practice items
  participant_item_data$Test<-"T"
  # make subset with test items only
  participant_item_data_T_only <- participant_item_data[participant_item_data$Test == "T", ]
  
  participant_item_data[grepl("prac", participant_item_data_T_only$audio),]$Test <- "P"
  
  participant_item_data_T_only$audio2 <- participant_item_data_T_only$audio
  participant_item_data_T_only <- participant_item_data_T_only %>%
    separate(audio2, c("item", "CondType", "cue1", "cue2"))
  
  return(participant_item_data_T_only)
}

# Adds picture type info 
add_picture_type_info <- function(data){
  data2 <- data
  
  data2$PicTypePos1 <- NA
  data2$PicTypePos2 <- NA
  data2$PicTypePos3 <- NA
  data2$PicTypePos4 <- NA
  
  
  data2[data2$pos1 == "horse.jpg" & data2$item == 1,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "horse.jpg" & data2$item == 1,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "horse.jpg" & data2$item == 1,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "horse.jpg" & data2$item == 1,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "dog.jpg" & data2$item == 1,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "dog.jpg" & data2$item == 1,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "dog.jpg" & data2$item == 1,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "dog.jpg" & data2$item == 1,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "candle.jpg" & data2$item == 2,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "candle.jpg" & data2$item == 2,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "candle.jpg" & data2$item == 2,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "candle.jpg" & data2$item == 2,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "match1.jpg" & data2$item == 2,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "match1.jpg" & data2$item == 2,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "match1.jpg" & data2$item == 2,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "match1.jpg" & data2$item == 2,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "apple.jpg" & data2$item == 3,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "apple.jpg" & data2$item == 3,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "apple.jpg" & data2$item == 3,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "apple.jpg" & data2$item == 3,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "egg.jpg" & data2$item == 3,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "egg.jpg" & data2$item == 3,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "egg.jpg" & data2$item == 3,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "egg.jpg" & data2$item == 3,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "train.jpg" & data2$item == 4,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "train.jpg" & data2$item == 4,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "train.jpg" & data2$item == 4,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "train.jpg" & data2$item == 4,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "car.jpg" & data2$item == 4,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "car.jpg" & data2$item == 4,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "car.jpg" & data2$item == 4,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "car.jpg" & data2$item == 4,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "RADIO1.jpg" & data2$item == 5,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "RADIO1.jpg" & data2$item == 5,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "RADIO1.jpg" & data2$item == 5,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "RADIO1.jpg" & data2$item == 5,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "speaker.jpg" & data2$item == 5,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "speaker.jpg" & data2$item == 5,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "speaker.jpg" & data2$item == 5,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "speaker.jpg" & data2$item == 5,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "lightbulb.jpg" & data2$item == 6,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "lightbulb.jpg" & data2$item == 6,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "lightbulb.jpg" & data2$item == 6,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "lightbulb.jpg" & data2$item == 6,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "torch.jpg" & data2$item == 6,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "torch.jpg" & data2$item == 6,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "torch.jpg" & data2$item == 6,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "torch.jpg" & data2$item == 6,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "elephant.jpg" & data2$item == 7,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "elephant.jpg" & data2$item == 7,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "elephant.jpg" & data2$item == 7,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "elephant.jpg" & data2$item == 7,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "monkey.jpg" & data2$item == 7,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "monkey.jpg" & data2$item == 7,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "monkey.jpg" & data2$item == 7,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "monkey.jpg" & data2$item == 7,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "moon.jpg" & data2$item == 8,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "moon.jpg" & data2$item == 8,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "moon.jpg" & data2$item == 8,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "moon.jpg" & data2$item == 8,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "rocket.jpg" & data2$item == 8,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "rocket.jpg" & data2$item == 8,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "rocket.jpg" & data2$item == 8,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "rocket.jpg" & data2$item == 8,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "tennisracket.jpg" & data2$item == 9,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "tennisracket.jpg" & data2$item == 9,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "tennisracket.jpg" & data2$item == 9,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "tennisracket.jpg" & data2$item == 9,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "bat1.jpg" & data2$item == 9,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "bat1.jpg" & data2$item == 9,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "bat1.jpg" & data2$item == 9,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "bat1.jpg" & data2$item == 9,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "hammer2.jpg" & data2$item == 10,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "hammer2.jpg" & data2$item == 10,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "hammer2.jpg" & data2$item == 10,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "hammer2.jpg" & data2$item == 10,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "wall.jpg" & data2$item == 10,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "wall.jpg" & data2$item == 10,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "wall.jpg" & data2$item == 10,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "wall.jpg" & data2$item == 10,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "zipper.jpg" & data2$item == 11,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "zipper.jpg" & data2$item == 11,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "zipper.jpg" & data2$item == 11,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "zipper.jpg" & data2$item == 11,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "door.jpg" & data2$item == 11,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "door.jpg" & data2$item == 11,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "door.jpg" & data2$item == 11,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "door.jpg" & data2$item == 11,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "seesaw.jpg" & data2$item == 12,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "seesaw.jpg" & data2$item == 12,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "seesaw.jpg" & data2$item == 12,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "seesaw.jpg" & data2$item == 12,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "swing.jpg" & data2$item == 12,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "swing.jpg" & data2$item == 12,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "swing.jpg" & data2$item == 12,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "swing.jpg" & data2$item == 12,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "dragon.jpg" & data2$item == 13,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "dragon.jpg" & data2$item == 13,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "dragon.jpg" & data2$item == 13,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "dragon.jpg" & data2$item == 13,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "queen.jpg" & data2$item == 13,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "queen.jpg" & data2$item == 13,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "queen.jpg" & data2$item == 13,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "queen.jpg" & data2$item == 13,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "crab.jpg" & data2$item == 14,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "crab.jpg" & data2$item == 14,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "crab.jpg" & data2$item == 14,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "crab.jpg" & data2$item == 14,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "shell.jpg" & data2$item == 14,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "shell.jpg" & data2$item == 14,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "shell.jpg" & data2$item == 14,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "shell.jpg" & data2$item == 14,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "watch.jpg" & data2$item == 15,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "watch.jpg" & data2$item == 15,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "watch.jpg" & data2$item == 15,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "watch.jpg" & data2$item == 15,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "clock.jpg" & data2$item == 15,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "clock.jpg" & data2$item == 15,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "clock.jpg" & data2$item == 15,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "clock.jpg" & data2$item == 15,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "cage.jpg" & data2$item == 16,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "cage.jpg" & data2$item == 16,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "cage.jpg" & data2$item == 16,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "cage.jpg" & data2$item == 16,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "mousetrap.jpg" & data2$item == 16,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "mousetrap.jpg" & data2$item == 16,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "mousetrap.jpg" & data2$item == 16,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "mousetrap.jpg" & data2$item == 16,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "mouse.jpg" & data2$item == 17,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "mouse.jpg" & data2$item == 17,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "mouse.jpg" & data2$item == 17,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "mouse.jpg" & data2$item == 17,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "beaver.jpg" & data2$item == 17,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "beaver.jpg" & data2$item == 17,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "beaver.jpg" & data2$item == 17,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "beaver.jpg" & data2$item == 17,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "motorcycle.jpg" & data2$item == 18,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "motorcycle.jpg" & data2$item == 18,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "motorcycle.jpg" & data2$item == 18,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "motorcycle.jpg" & data2$item == 18,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "stoplight.jpg" & data2$item == 18,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "stoplight.jpg" & data2$item == 18,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "stoplight.jpg" & data2$item == 18,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "stoplight.jpg" & data2$item == 18,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "dinosaur.jpg" & data2$item == 19,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "dinosaur.jpg" & data2$item == 19,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "dinosaur.jpg" & data2$item == 19,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "dinosaur.jpg" & data2$item == 19,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "alligator.jpg" & data2$item == 19,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "alligator.jpg" & data2$item == 19,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "alligator.jpg" & data2$item == 19,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "alligator.jpg" & data2$item == 19,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "tiger.jpg" & data2$item == 20,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "tiger.jpg" & data2$item == 20,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "tiger.jpg" & data2$item == 20,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "tiger.jpg" & data2$item == 20,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "zebra.jpg" & data2$item == 20,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "zebra.jpg" & data2$item == 20,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "zebra.jpg" & data2$item == 20,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "zebra.jpg" & data2$item == 20,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "kettle.jpg" & data2$item == 21,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "kettle.jpg" & data2$item == 21,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "kettle.jpg" & data2$item == 21,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "kettle.jpg" & data2$item == 21,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "mug.jpg" & data2$item == 21,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "mug.jpg" & data2$item == 21,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "mug.jpg" & data2$item == 21,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "mug.jpg" & data2$item == 21,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "rain.jpg" & data2$item == 22,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "rain.jpg" & data2$item == 22,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "rain.jpg" & data2$item == 22,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "rain.jpg" & data2$item == 22,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "mushroom.jpg" & data2$item == 22,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "mushroom.jpg" & data2$item == 22,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "mushroom.jpg" & data2$item == 22,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "mushroom.jpg" & data2$item == 22,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "diamond.jpg" & data2$item == 23,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "diamond.jpg" & data2$item == 23,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "diamond.jpg" & data2$item == 23,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "diamond.jpg" & data2$item == 23,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "dime.jpg" & data2$item == 23,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "dime.jpg" & data2$item == 23,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "dime.jpg" & data2$item == 23,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "dime.jpg" & data2$item == 23,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "fishingpole.jpg" & data2$item == 24,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "fishingpole.jpg" & data2$item == 24,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "fishingpole.jpg" & data2$item == 24,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "fishingpole.jpg" & data2$item == 24,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "net.jpg" & data2$item == 24,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "net.jpg" & data2$item == 24,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "net.jpg" & data2$item == 24,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "net.jpg" & data2$item == 24,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "camel.jpg" & data2$item == 25,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "camel.jpg" & data2$item == 25,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "camel.jpg" & data2$item == 25,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "camel.jpg" & data2$item == 25,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "pyramid.jpg" & data2$item == 25,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "pyramid.jpg" & data2$item == 25,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "pyramid.jpg" & data2$item == 25,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "pyramid.jpg" & data2$item == 25,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "balloon.jpg" & data2$item == 26,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "balloon.jpg" & data2$item == 26,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "balloon.jpg" & data2$item == 26,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "balloon.jpg" & data2$item == 26,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "cake.jpg" & data2$item == 26,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "cake.jpg" & data2$item == 26,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "cake.jpg" & data2$item == 26,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "cake.jpg" & data2$item == 26,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "devil.jpg" & data2$item == 27,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "devil.jpg" & data2$item == 27,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "devil.jpg" & data2$item == 27,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "devil.jpg" & data2$item == 27,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "cross.jpg" & data2$item == 27,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "cross.jpg" & data2$item == 27,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "cross.jpg" & data2$item == 27,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "cross.jpg" & data2$item == 27,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "gun.jpg" & data2$item == 28,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "gun.jpg" & data2$item == 28,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "gun.jpg" & data2$item == 28,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "gun.jpg" & data2$item == 28,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "cannon.jpg" & data2$item == 28,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "cannon.jpg" & data2$item == 28,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "cannon.jpg" & data2$item == 28,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "cannon.jpg" & data2$item == 28,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "glasses.jpg" & data2$item == 29,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "glasses.jpg" & data2$item == 29,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "glasses.jpg" & data2$item == 29,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "glasses.jpg" & data2$item == 29,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "window.jpg" & data2$item == 29,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "window.jpg" & data2$item == 29,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "window.jpg" & data2$item == 29,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "window.jpg" & data2$item == 29,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "knife.jpg" & data2$item == 30,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "knife.jpg" & data2$item == 30,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "knife.jpg" & data2$item == 30,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "knife.jpg" & data2$item == 30,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "sword.jpg" & data2$item == 30,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "sword.jpg" & data2$item == 30,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "sword.jpg" & data2$item == 30,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "sword.jpg" & data2$item == 30,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "book.jpg" & data2$item == 31,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "book.jpg" & data2$item == 31,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "book.jpg" & data2$item == 31,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "book.jpg" & data2$item == 31,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "owl.jpg" & data2$item == 31,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "owl.jpg" & data2$item == 31,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "owl.jpg" & data2$item == 31,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "owl.jpg" & data2$item == 31,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "moose.jpg" & data2$item == 32,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "moose.jpg" & data2$item == 32,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "moose.jpg" & data2$item == 32,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "moose.jpg" & data2$item == 32,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "deer.jpg" & data2$item == 32,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "deer.jpg" & data2$item == 32,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "deer.jpg" & data2$item == 32,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "deer.jpg" & data2$item == 32,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "kite.jpg" & data2$item == 33,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "kite.jpg" & data2$item == 33,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "kite.jpg" & data2$item == 33,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "kite.jpg" & data2$item == 33,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "fan.jpg" & data2$item == 33,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "fan.jpg" & data2$item == 33,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "fan.jpg" & data2$item == 33,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "fan.jpg" & data2$item == 33,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "shark.jpg" & data2$item == 34,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "shark.jpg" & data2$item == 34,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "shark.jpg" & data2$item == 34,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "shark.jpg" & data2$item == 34,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "submarine.jpg" & data2$item == 34,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "submarine.jpg" & data2$item == 34,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "submarine.jpg" & data2$item == 34,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "submarine.jpg" & data2$item == 34,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "onion.jpg" & data2$item == 35,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "onion.jpg" & data2$item == 35,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "onion.jpg" & data2$item == 35,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "onion.jpg" & data2$item == 35,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "tear.jpg" & data2$item == 35,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "tear.jpg" & data2$item == 35,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "tear.jpg" & data2$item == 35,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "tear.jpg" & data2$item == 35,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "dog.jpg" & data2$item == 36,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "dog.jpg" & data2$item == 36,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "dog.jpg" & data2$item == 36,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "dog.jpg" & data2$item == 36,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "cat.jpg" & data2$item == 36,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "cat.jpg" & data2$item == 36,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "cat.jpg" & data2$item == 36,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "cat.jpg" & data2$item == 36,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "ring.jpg" & data2$item == 37,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "ring.jpg" & data2$item == 37,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "ring.jpg" & data2$item == 37,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "ring.jpg" & data2$item == 37,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "bride.jpg" & data2$item == 37,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "bride.jpg" & data2$item == 37,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "bride.jpg" & data2$item == 37,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "bride.jpg" & data2$item == 37,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "pizza.jpg" & data2$item == 38,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "pizza.jpg" & data2$item == 38,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "pizza.jpg" & data2$item == 38,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "pizza.jpg" & data2$item == 38,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "spaghetti.jpg" & data2$item == 38,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "spaghetti.jpg" & data2$item == 38,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "spaghetti.jpg" & data2$item == 38,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "spaghetti.jpg" & data2$item == 38,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "tie.jpg" & data2$item == 39,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "tie.jpg" & data2$item == 39,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "tie.jpg" & data2$item == 39,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "tie.jpg" & data2$item == 39,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "desk.jpg" & data2$item == 39,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "desk.jpg" & data2$item == 39,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "desk.jpg" & data2$item == 39,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "desk.jpg" & data2$item == 39,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "stove.jpg" & data2$item == 40,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "stove.jpg" & data2$item == 40,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "stove.jpg" & data2$item == 40,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "stove.jpg" & data2$item == 40,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "pan.jpg" & data2$item == 40,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "pan.jpg" & data2$item == 40,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "pan.jpg" & data2$item == 40,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "pan.jpg" & data2$item == 40,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "saw.jpg" & data2$item == 41,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "saw.jpg" & data2$item == 41,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "saw.jpg" & data2$item == 41,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "saw.jpg" & data2$item == 41,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "hammer.jpg" & data2$item == 41,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "hammer.jpg" & data2$item == 41,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "hammer.jpg" & data2$item == 41,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "hammer.jpg" & data2$item == 41,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "penguin.jpg" & data2$item == 42,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "penguin.jpg" & data2$item == 42,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "penguin.jpg" & data2$item == 42,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "penguin.jpg" & data2$item == 42,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "eskimo.jpg" & data2$item == 42,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "eskimo.jpg" & data2$item == 42,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "eskimo.jpg" & data2$item == 42,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "eskimo.jpg" & data2$item == 42,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "snail.jpg" & data2$item == 43,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "snail.jpg" & data2$item == 43,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "snail.jpg" & data2$item == 43,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "snail.jpg" & data2$item == 43,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "turtle.jpg" & data2$item == 43,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "turtle.jpg" & data2$item == 43,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "turtle.jpg" & data2$item == 43,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "turtle.jpg" & data2$item == 43,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "chair.jpg" & data2$item == 44,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "chair.jpg" & data2$item == 44,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "chair.jpg" & data2$item == 44,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "chair.jpg" & data2$item == 44,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "bench.jpg" & data2$item == 44,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "bench.jpg" & data2$item == 44,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "bench.jpg" & data2$item == 44,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "bench.jpg" & data2$item == 44,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "accordion.jpg" & data2$item == 45,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "accordion.jpg" & data2$item == 45,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "accordion.jpg" & data2$item == 45,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "accordion.jpg" & data2$item == 45,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "piano.jpg" & data2$item == 45,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "piano.jpg" & data2$item == 45,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "piano.jpg" & data2$item == 45,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "piano.jpg" & data2$item == 45,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "telescope.jpg" & data2$item == 46,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "telescope.jpg" & data2$item == 46,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "telescope.jpg" & data2$item == 46,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "telescope.jpg" & data2$item == 46,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "microscope.jpg" & data2$item == 46,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "microscope.jpg" & data2$item == 46,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "microscope.jpg" & data2$item == 46,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "microscope.jpg" & data2$item == 46,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "letter.jpg" & data2$item == 47,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "letter.jpg" & data2$item == 47,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "letter.jpg" & data2$item == 47,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "letter.jpg" & data2$item == 47,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "mailbox.jpg" & data2$item == 47,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "mailbox.jpg" & data2$item == 47,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "mailbox.jpg" & data2$item == 47,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "mailbox.jpg" & data2$item == 47,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "parrot.jpg" & data2$item == 48,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "parrot.jpg" & data2$item == 48,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "parrot.jpg" & data2$item == 48,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "parrot.jpg" & data2$item == 48,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "microphone.jpg" & data2$item == 48,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "microphone.jpg" & data2$item == 48,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "microphone.jpg" & data2$item == 48,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "microphone.jpg" & data2$item == 48,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "trophy.jpg" & data2$item == 49,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "trophy.jpg" & data2$item == 49,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "trophy.jpg" & data2$item == 49,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "trophy.jpg" & data2$item == 49,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "medal.jpg" & data2$item == 49,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "medal.jpg" & data2$item == 49,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "medal.jpg" & data2$item == 49,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "medal.jpg" & data2$item == 49,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "scorpion.jpg" & data2$item == 50,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "scorpion.jpg" & data2$item == 50,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "scorpion.jpg" & data2$item == 50,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "scorpion.jpg" & data2$item == 50,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "snake.jpg" & data2$item == 50,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "snake.jpg" & data2$item == 50,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "snake.jpg" & data2$item == 50,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "snake.jpg" & data2$item == 50,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "washingmachine.jpg" & data2$item == 51,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "washingmachine.jpg" & data2$item == 51,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "washingmachine.jpg" & data2$item == 51,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "washingmachine.jpg" & data2$item == 51,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "vacuum.jpg" & data2$item == 51,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "vacuum.jpg" & data2$item == 51,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "vacuum.jpg" & data2$item == 51,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "vacuum.jpg" & data2$item == 51,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "typewriter.jpg" & data2$item == 52,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "typewriter.jpg" & data2$item == 52,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "typewriter.jpg" & data2$item == 52,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "typewriter.jpg" & data2$item == 52,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "pen.jpg" & data2$item == 52,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "pen.jpg" & data2$item == 52,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "pen.jpg" & data2$item == 52,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "pen.jpg" & data2$item == 52,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "carrot.jpg" & data2$item == 53,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "carrot.jpg" & data2$item == 53,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "carrot.jpg" & data2$item == 53,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "carrot.jpg" & data2$item == 53,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "shoe.jpg" & data2$item == 53,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "shoe.jpg" & data2$item == 53,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "shoe.jpg" & data2$item == 53,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "shoe.jpg" & data2$item == 53,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "nose.jpg" & data2$item == 54,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "nose.jpg" & data2$item == 54,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "nose.jpg" & data2$item == 54,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "nose.jpg" & data2$item == 54,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "ear.jpg" & data2$item == 54,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "ear.jpg" & data2$item == 54,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "ear.jpg" & data2$item == 54,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "ear.jpg" & data2$item == 54,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "stroller.jpg" & data2$item == 55,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "stroller.jpg" & data2$item == 55,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "stroller.jpg" & data2$item == 55,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "stroller.jpg" & data2$item == 55,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "wheelchair.jpg" & data2$item == 55,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "wheelchair.jpg" & data2$item == 55,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "wheelchair.jpg" & data2$item == 55,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "wheelchair.jpg" & data2$item == 55,]$PicTypePos4 <- "Dist"
  
  data2[data2$pos1 == "banana.jpg" & data2$item == 56,]$PicTypePos1 <- "Target"
  data2[data2$pos2 == "banana.jpg" & data2$item == 56,]$PicTypePos2 <- "Target"
  data2[data2$pos3 == "banana.jpg" & data2$item == 56,]$PicTypePos3 <- "Target"
  data2[data2$pos4 == "banana.jpg" & data2$item == 56,]$PicTypePos4 <- "Target"
  
  data2[data2$pos1 == "pineapple.jpg" & data2$item == 56,]$PicTypePos1 <- "Dist"
  data2[data2$pos2 == "pineapple.jpg" & data2$item == 56,]$PicTypePos2 <- "Dist"
  data2[data2$pos3 == "pineapple.jpg" & data2$item == 56,]$PicTypePos3 <- "Dist"
  data2[data2$pos4 == "pineapple.jpg" & data2$item == 56,]$PicTypePos4 <- "Dist"
  
  return(data2)
} 

# Read in cueword information
read_cueword_info <- function(){
  cuewordinfo <- read.table("Data/001_Part1/Input/Tabel_cue_onsettimes_durations.txt", header = TRUE)
  return(cuewordinfo)
}

# Merge the data from YAOA and the cueword information
merge_YAOA_and_cuewords <- function(data, cuewordinfo){
  data$audio <- str_replace(data$audio, ".wav$", "")
  data <- merge(x=data,
                y=cuewordinfo,
                by.x = "audio",
                by.y= "file",
                all.x = TRUE)
  data$subject_nr <- as.character(data$subject_nr)
  return(data)
}

# Adds response infomration to the data, boolean for exporting
add_response_info <- function(data, export){
  #data$subject_nr <- as.numeric(data$subject_nr)
  data <- data %>%
    mutate(
      Q_offsetT = as.numeric(0),
      Resp_acc = "EMPTY",		
      Resp_onsetT = as.numeric(0),
      Resp_dur = as.numeric(0)
    )
  
  files <- list.files(path = "Data/001_Part1/Input/subj_mijntabels",
                      pattern = "^mijntabel_subj-.*\\.wav\\.Table$",
                      full.names = TRUE)
  
  for (file_name in files) {
    m <<- strsplit(file_name, '_')
    
    table_data <- read.table(file_name, header = TRUE, sep = "\t")
    table_data$dur <- as.numeric(table_data$dur)
    table_data$totdur <- as.numeric(table_data$totdur)
    
    subject_nr_string <- m[[1]][4]
    subject_nr <- as.numeric(str_extract(subject_nr_string, "\\d+$"))
    Cond_type <- str_extract(file_name, "(?<=_)[A-Z](?=_)") 
    item <- as.numeric(m[[1]][5])
    
    Q_offsetT <- table_data[2, 4]	
    Resp_acc <- table_data[4, 2]
    Resp_onsetT <- round(table_data[4, 3], 3)
    Resp_dur <- table_data[4, 4]
    
    
    data[data$item == item & data$CondType == Cond_type & data$subject_nr == subject_nr,]$Q_offsetT <- Q_offsetT 
    data[data$item == item & data$CondType == Cond_type & data$subject_nr == subject_nr,]$Resp_acc <- Resp_acc
    data[data$item == item & data$CondType == Cond_type & data$subject_nr == subject_nr,]$Resp_onsetT <- Resp_onsetT 
    data[data$item == item & data$CondType == Cond_type & data$subject_nr == subject_nr,]$Resp_dur <- Resp_dur
  }
  
  data <- data %>%
    select(audio, Source.Name, pos1, pos2, pos3, pos4, subject_nr, item, CondType,
           cue1, cue2, PicTypePos1, PicTypePos2, PicTypePos3, PicTypePos4, onset1,
           dur1, onset2, dur2, totdur)
  
  data$subject_nr <- gsub("[^0-9]", "", data$Source.Name)
  
  if(export == T){
    output_dir <- "Data/001_Part1/Output"
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    write.csv(data,
              file = "Data/001_Part1/Output/YAOA_merged.csv")
  }
  
  return(data)
}


##### RUNNING SCRIPT ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
data <- read_and_format_YAOA()
data <- add_picture_type_info(data)
cuewordinfo <- read_cueword_info()
data <- merge_YAOA_and_cuewords(data, cuewordinfo)
data <- add_response_info(data, export = T) # Export immediately saves data as a .csv

################################################################################
################################################################################
rm(list = ls())                                                                #
# END OF PART I                                                               #
################################################################################
################################################################################


################################################################################
################################################################################





################################################################################
################################################################################
# PART II: Generates .csvs for each participant with each trial and where they look
#           at. Results used in part II to fill in remaining information.
# NOTE: Each subject takes about 2-4 minutes to process on my (Bram's) PC.
################################################################################
################################################################################
# Directory 
setwd("D:/repos/EyeTracking") # This is on my (Bram's) PC

##### Functions ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
# Find all .csv/.tsv files
get_filenames <- function(){
  data_csv_filenames <- list.files(path = "Data/002_Part2/Input", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
  data_tsv_filenames <- list.files(path = "Data/002_Part2/Input", pattern = "*.tsv", full.names = TRUE, recursive = TRUE)
  filter_filenames(data_csv_filenames, data_tsv_filenames)
}

# Filter the lists so that only the subjects with corresponding .csv AND .tsv files remain.
filter_filenames <- function(data_csv_filenames, data_tsv_filenames){
  csv_subject_numbers <- gsub("^Data/002_Part2/Input/subject-(\\d+)\\.csv$", "\\1", data_csv_filenames)
  tsv_subject_numbers <- gsub("^Data/002_Part2/Input/subject-(\\d+)_TOBII_output\\.tsv$", "\\1", data_tsv_filenames)
  
  common_subject_numbers <<- intersect(csv_subject_numbers, tsv_subject_numbers)
  
  
  data_csv_filenames <<- data_csv_filenames[csv_subject_numbers %in% common_subject_numbers]
  data_tsv_filenames <<- data_tsv_filenames[tsv_subject_numbers %in% common_subject_numbers]
}

fix_events <- function(csv_file_path, tsv_file_path){
  # curr_subj <<- csv_file_path
  csv_data <- read.csv(csv_file_path)
  tsv_data <- read.delim(tsv_file_path, sep = "\t")
  
  subject_number <<- gsub("^Data/002_Part2/Input/subject-(\\d+)\\.csv$", "\\1", data_csv_filenames[1])
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

# This function enters the right values in the "event" column in (e.g.) tsv_1114_trials_only,
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
           looking_at_L = "target/distractor/irr",
           looking_at_R = "target/distractor/irr",
           pos1 = "placeholder1",
           pos2 = "placeholder2",
           pos3 = "placeholder3",
           pos4 = "placeholder4",
           timestamp_relative = timestamp - trial_start) %>%
    select(trial, trial_start, timestamp, timestamp_relative, everything())
  
  head(participant_data_relevant)
  return(participant_data_relevant)
}

# Export participant data to a .csv
export_participant_data <- function(participant_data, curr_subj){
  output_dir <- "Data/002_Part2/Output"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  
  # relative_path <- sub("^[^/]*/", "", curr_subj)
  relative_path <- sub(".*/", "", curr_subj)
  
  
  new_curr_subj <- file.path(output_dir, relative_path)
  
  
  write.csv(participant_data,
            file = new_curr_subj,
            row.names = FALSE)
}
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####

##### RUNNING SCRIPT ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
#  GET ALL FILENAMES (only when both .csv and .tsv are present)
get_filenames()

# CLEAN UP AND EXPORT DATA
for (participant_no in seq_along(data_csv_filenames)){
  participant_data <<- fix_events(csv_file_path = data_csv_filenames[participant_no],
                                  tsv_file_path = data_tsv_filenames[participant_no])
  csv_data <<- read.csv(data_csv_filenames[participant_no])
  participant_data <<- prep_eyetracker_data(csv_data)
  
  
  export_participant_data(participant_data,
                          curr_subj = data_csv_filenames[participant_no])
}



##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 



################################################################################
################################################################################
rm(list = ls())                                                                #
# END OF PART II                                                               #
################################################################################
################################################################################


################################################################################
################################################################################
# PART III: Generates .csvs for each participant with each trial and where they look
#           at. Results used in part I & II to fill in remaining information.
################################################################################
################################################################################
# Directory 
setwd("D:/repos/EyeTracking") # This is on my (Bram's) PC





##### Functions ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
# Gets the relevant data files to work with.
get_files <- function(participant_data_dir, YAOA_merged_file){
  participant_data_csvs <<- list.files(path = participant_data_dir, pattern = "*.csv", full.names = TRUE, recursive = TRUE)
  YAOA_data <<- read.csv(YAOA_merged_file)
  
  YAOA_data <<- YAOA_data %>%
    mutate(subject_nr = as.character(subject_nr),
           onset1 = as.numeric(onset1),
           dur1 = as.numeric(dur1),
           onset2 = as.numeric(onset2),
           dur2 = as.numeric(dur2),
           totdur = as.numeric(totdur)) %>%
    select(-X)
}

# Gets the participant number of the participant currently being processed.
get_participant_number <- function(participant){
  participant_number <- str_extract(participant, "(?<=subject-)\\d+")
  return(participant_number)
}

# Gets the current participant data and makes sure all variables are the right
#   type/format.
get_current_participant_data <- function(participant, curr_participant_number){
  curr_participant_df <- read.csv(participant)
  curr_participant_df <- curr_participant_df %>%
    mutate(trial_start = as.numeric(trial_start/1000),
           timestamp = as.numeric(timestamp/1000),
           timestamp_relative = as.numeric(timestamp_relative/1000),
           participant = curr_participant_number)
  
  return(curr_participant_df)
}

# Gets the relevant data from YAOA_data for the current participant
get_current_participant_YAOA_data <- function(YAOA_data, curr_participant_number){
  curr_YAOA_data <- YAOA_data %>%
    filter(curr_participant_number == subject_nr)
  
  return(curr_YAOA_data)
}

# This does all the manipulation. Each part has its own function described below
fill_participant_df <- function(curr_participant_YAOA, curr_participant_df) {
  for (row in 1:nrow(curr_participant_YAOA)) {
    current_audio <- curr_participant_YAOA$audio[row]
    matching_rows <- grepl(current_audio, curr_participant_df$event)
    
    curr_participant_df <- update_positions(curr_participant_df, matching_rows, curr_participant_YAOA, row)
    curr_participant_df <- set_can_look_flag(curr_participant_df, matching_rows, curr_participant_YAOA, row)
  }
  
  for (row in 1:nrow(curr_participant_df)) {
    curr_looking_at_pos_l <- get_curr_looking_at_pos(
      curr_x = round(curr_participant_df$x_left[row],1),
      curr_y = round(curr_participant_df$y_left[row],1),
      curr_participant_df,
      row
    )
    
    curr_looking_at_pos_r <- get_curr_looking_at_pos(
      curr_x = round(curr_participant_df$x_right[row],1),
      curr_y = round(curr_participant_df$y_right[row],1),
      curr_participant_df,
      row
    )
    
    curr_participant_df$looking_at_L[row] <- curr_looking_at_pos_l
    curr_participant_df$looking_at_R[row] <- curr_looking_at_pos_r
  }
  
  
  return(curr_participant_df)
}

# Part of fill_participant_df: Sets can_look to TRUE for when participant can look
#   at the target word based on the audio timing.
set_can_look_flag <- function(curr_participant_df, matching_rows, curr_participant_YAOA, row) {
  cond_type <- curr_participant_YAOA$CondType[row]
  
  if (cond_type == "E") {
    curr_target_word_cue <- curr_participant_YAOA$onset1[row] * 1000
  } else if (cond_type == "L") {
    curr_target_word_cue <- curr_participant_YAOA$onset2[row] * 1000
  } else {
    return(curr_participant_df) 
  }
  
  if (!is.na(curr_target_word_cue)) {
    curr_participant_df[matching_rows & curr_participant_df$timestamp_relative >= curr_target_word_cue, "can_look"] <- TRUE
  }
  
  return(curr_participant_df)
}

# Part of fill_participant_df: Puts target/distractors in the right place.
update_positions <- function(curr_participant_df, matching_rows, curr_participant_YAOA, row) {
  curr_participant_df[matching_rows, c("pos1", "pos2", "pos3", "pos4")] <- 
    curr_participant_YAOA[row, c("PicTypePos1", "PicTypePos2", "PicTypePos3", "PicTypePos4")]
  return(curr_participant_df)
}

# Part of fill_participant_df: Returns where the participant is looking at each
#   interval.
get_curr_looking_at_pos <- function(curr_x, curr_y, curr_participant_df, row) {
  if (curr_x > 0 & curr_x <= 960 & curr_y > 0 & curr_y <= 540) {
    curr_looking_at_imagetype <- curr_participant_df$pos1[row]
    return(curr_looking_at_imagetype)
  } else if (curr_x > 960 & curr_x <= 1920 & curr_y > 0 & curr_y <= 540) {
    curr_looking_at_imagetype <- curr_participant_df$pos2[row]
    return(curr_looking_at_imagetype)
  } else if (curr_x > 0 & curr_x <= 960 & curr_y > 540 & curr_y <= 1080) {
    curr_looking_at_imagetype <- curr_participant_df$pos3[row]
    return(curr_looking_at_imagetype)
  } else if (curr_x > 960 & curr_x <= 1920 & curr_y > 540 & curr_y <= 1080) {
    curr_looking_at_imagetype <- curr_participant_df$pos4[row]
    return(curr_looking_at_imagetype)
  } else {
    return("out_of_bounds") 
  }
}

# Exports the participant_df
export_participant_df <- function(curr_participant_df){
  output_dir <- "Data/003_Part3/Output"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  relative_path <- sub(".*/", "", participant)
  
  new_curr_subj <- file.path(output_dir, relative_path)
  
  write.csv(curr_participant_df,
            file = new_curr_subj,
            row.names = FALSE)
}

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####


##### RUNNING SCRIPT ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
get_files(participant_data_dir = "Data/003_Part3/Input/Participants",
          YAOA_merged_file = "Data/003_Part3/Input/YAOA_merged.csv")

for (participant in participant_data_csvs){
  curr_participant_number <- get_participant_number(participant)
  curr_participant_df <<- get_current_participant_data(participant, curr_participant_number)
  curr_participant_YAOA <<- get_current_participant_YAOA_data(YAOA_data, curr_participant_number)
  curr_participant_df <<- fill_participant_df(curr_participant_YAOA, curr_participant_df)
  export_participant_df(curr_participant_df)
}



##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####









################################################################################
################################################################################
# END OF PART III                                                                #
################################################################################
################################################################################

