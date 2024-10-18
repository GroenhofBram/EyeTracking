rm(list = ls()) ## clear global environment
cat("\014") ## clear console

## merge the verb-final datasets

df1 <- read.csv("t120_dataset_verbfinal.csv", head=T) # read in the dataset from T120 eye-tracker
df2 <- read.csv("tobiipro_dataset_verbfinal.csv", head=T) # read in the dataset from tobiipro eye-tracker

trial <- rbind(df1, df2)
write.csv(trial, file = "all_adult_data_verbfinal.csv", row.names = FALSE)

# ### merge the verb-medial datasets

df3 <- read.csv("t120_dataset_verbmedial.csv", head=T) # read in the dataset from T120 eye-tracker
df4 <- read.csv("tobiipro_dataset_verbmedial.csv", head=T) # read in the dataset from tobiipro eye-tracker

trial2 <- rbind(df3, df4)
write.csv(trial2, file = "all_adult_data_verbmedial.csv", row.names = FALSE)
