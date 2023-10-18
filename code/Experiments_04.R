# Script for doing Granger Causality analysis on M1 & M2 time series
clearhistory <- function() {
  write("", file=".blank")
  loadhistory(".blank")
  unlink(".blank")
}
clearhistory()

rm(list=ls()) # Clear work Environment
options(scipen = 100, digits = 4)

library(readr)
library(purrr)
library(dplyr)
library(stringr)

setwd("C:/San/PhD/Project/Causality/GrangerCausality/PetersCode/GrangerCausality-Peter/ICMPCanalysis2/Results/Borodin_New/AllCombos") # set path to folder with csv-files

df <- fs::dir_ls(regexp = "\\.csv$") %>%
  map_dfr(read_csv, id='path') %>%
  mutate(thename = str_replace(path, ".tsv","")) %>%
  select(-path)

# Select and rename columns in one step
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

# Save the final dataframe to a CSV file
write.csv(df_final, file=paste0("Final_Borodin_Results_GC.csv"), quote=F, row.names=F, na='NA') # Save output to a csv file in the same directory as data file