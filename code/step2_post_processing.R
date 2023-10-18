# Load necessary libraries
library(stringr)
library(lmtest) # This provides grangertest
library(tseries) # For stationarity test (kpss.test)

# Function to clear command history
clear_history <- function() {
  write("", file = ".blank")
  loadhistory(".blank")
  unlink(".blank")
}

# Clear the command history and workspace
clear_history()
rm(list = ls()) # Clear work Environment
options(scipen = 100, digits = 4)


# Function for Granger Causality analysis on M1 & M2 time series
granger_analysis_bivariate <- function(working_directory) {
  # Set the working directory
  setwd(working_directory) # Set path to folder with csv-files
  
  # List all text files in the directory
  listResults <- dir(pattern = "*.txt") # Creates a list of all the csv files in the directory
  
  # Loop through all the files and process each file
  for (k in seq_along(listResults)) {
    data_test <- read.delim(listResults[k], sep = ",")[1:6]
    attach(data_test)
    
    # Initialize an empty dataframe to store all output values
    output_all <- data.frame(matrix(nrow = nrow(data_test), ncol = 13))
    
    # Loop through all the rows in the current data file
    for (j in seq_len(nrow(data_test))) {
      colnames(output_all) <- c('concert', 'phrase', 'musician1', 'musician2', 'texture', 'pair', 'F_M1_M2', 'p_M1_M2', 'F_M2_M1', 'p_M2_M1', 'p_M1_M2_Bin', 'p_M2_M1_Bin', 'avgF') # Column headers for output
      combo <- data_test$data_file[j]
      concert <- word(combo, 1, sep = "_")
      phrase <- word(combo, 2, sep = "_")
      musician1 <- word(combo, 3, sep = "_")
      musician2 <- word(combo, 4, sep = "_")
      texture <- data_test$Texture[j]
      pair <- paste0(musician1, "_", musician2)
      F_M1_M2 <- as.numeric(data_test$F_M1_M2[j])
      F_M2_M1 <- as.numeric(data_test$F_M2_M1[j])
      p_M1_M2 <- as.numeric(data_test$p_M1_M2[j])
      p_M2_M1 <- as.numeric(data_test$p_M2_M1[j])
      p_M1_M2_Bin = ifelse(p_M1_M2 < 0.05, 1, 0)
      p_M2_M1_Bin = ifelse(p_M2_M1 < 0.05, 1, 0)
      avgF <- ((F_M1_M2 + F_M2_M1) / 2)
      output_all[j, ] <- c(concert, phrase, musician1, musician2, texture, pair, F_M1_M2, p_M1_M2, F_M2_M1, p_M2_M1, p_M1_M2_Bin, p_M2_M1_Bin, avgF)
    }
    # Save output to csv file in the same directory as data file
    write.csv(output_all, file = paste0(concert, "_", phrase, "_Results_GC.csv"), quote = F, row.names = F, na = 'NA')
    detach(data_test)
  }
}

# Set the pause time for visualization
PausePlot = .1 # Time to pause to visualize M1 & M2 plots (set to 0 for no pause)

# Define the working directory
working_directory <- "C:/San/Projects/DirectionalityOfInfluence/data/Borodin/AllCombos"

# Call the Granger Causality analysis function
granger_analysis_bivariate(working_directory)