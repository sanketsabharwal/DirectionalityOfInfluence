# Function to clear command history
clearhistory <- function() {
  write("", file=".blank")
  loadhistory(".blank")
  unlink(".blank")
}

# Clear command history and work environment
clearhistory()
rm(list = ls())

# Load necessary libraries
library(readr)
library(purrr)
library(dplyr)
library(stringr)
library(fs)
library(lmtest) ## This provides grangertest
library(tseries) # For stationarity test (kpss.test)
dplyr::bind_rows()

# Set global options
options(scipen = 100, digits = 4)

# Set working directory
setwd("C:/San/Projects/DirectionalityOfInfluence/data/Borodin/AllCombos") # set path to folder with csv-files

# Creates a list of all the txt files in the directory
listResults <- dir(pattern = "*.txt")

for (k in 1:length(listResults)){
  data_test <- read.delim(listResults[k],sep =",")[1:6]
  attach(data_test)
  output_all <- data.frame(matrix(nrow = nrow(data_test), ncol = 13)) # Make data frame to store output values
  for (j in 1:nrow(data_test)){
    colnames(output_all) <- c('concert','phrase' ,'musician1', 'musician2', 'texture', 'pair', 'F_M1_M2', 'p_M1_M2','F_M2_M1','p_M2_M1','p_M1_M2_Bin','p_M2_M1_Bin','avgF') # Column headers for output
    data_test$F_M1_M2 <- as.numeric(data_test$F_M1_M2)
    data_test$F_M2_M1 <- as.numeric(data_test$F_M2_M1)
    data_test$p_M1_M2 <- as.numeric(data_test$p_M1_M2)
    data_test$p_M2_M1 <- as.numeric(data_test$p_M2_M1)
    combo<-data_test$data_file[j]
    concert <- word(combo, 1, sep = "_")
    phrase <- word(combo, 2, sep = "_")
    musician1 <- word(combo, 3, sep = "_")
    musician2 <- word(combo, 4, sep = "_")
    texture<-data_test$Texture[j]
    pair<-paste0(musician1,"_",musician2)
    F_M1_M2<-data_test$F_M1_M2[j]
    F_M2_M1<-data_test$F_M2_M1[j]
    p_M1_M2<-data_test$p_M1_M2[j]
    p_M2_M1<-data_test$p_M2_M1[j]
    p_M1_M2_Bin = ifelse(p_M1_M2<0.05,1,0)
    p_M2_M1_Bin = ifelse(p_M2_M1<0.05,1,0)
    avgF <- ((F_M1_M2+F_M2_M1)/2)
    output_all[j , ] <- c(concert, phrase, musician1, musician2, texture, pair, F_M1_M2, p_M1_M2, F_M2_M1, p_M2_M1, p_M1_M2_Bin, p_M2_M1_Bin, avgF)
  }
  write.csv(output_all, file=paste0(concert,"_",phrase,"_Results_GC.csv"), quote=F, row.names=F, na='NA') # Save output to xlsx file with .txt extention in same directory as data file
  detach(data_test)
}



df <- fs::dir_ls(regexp = "\\.csv$") %>%
  map_dfr(read_csv, id='path') %>%
  mutate(thename = str_replace(path, ".tsv","")) %>%
  select(-path)

# Process the data frame
df_final <- df %>%
  mutate(
    musician1 = str_replace(musician1, "c", "m"),
    musician2 = str_replace(musician2, "c", "m"),
    pair = str_replace_all(pair, "c", "m")
  ) %>%
  select(
    Concert = concert,
    Phrase = phrase,
    Musician1 = musician1,
    Musician2 = musician2,
    Texture = texture,
    Pair = pair,
    F_M1_M2 = F_M1_M2,
    p_M1_M2 = p_M1_M2,
    F_M2_M1 = F_M2_M1,
    p_M2_M1 = p_M2_M1,
    p_M1_M2_Bin = p_M1_M2_Bin,
    p_M2_M1_Bin = p_M2_M1_Bin,
    avgF = avgF
  )

# Save the final data frame to a CSV file. Name it as per your group, in this case it will be Final_Borodin_Results_GC.csv
write.csv(df_final, file=paste0("Final_Borodin_Results_GC.csv"), quote=F, row.names=F, na='NA') # Save output to a csv file in the same directory as data file