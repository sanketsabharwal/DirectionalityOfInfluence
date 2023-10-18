# Script for doing Granger Causality analysis on M1 & M2 time series
clearhistory <- function() {
  write("", file=".blank")
  loadhistory(".blank")
  unlink(".blank")
}
clearhistory()

rm(list=ls())  # Clear work Environment

library(lmtest)  # This provides grangertest
library(tseries)  # For stationarity test (kpss.test)
library(psd)  # For pre-whitening process to remove non-stationarity in a uni-variate time-series

# Initialize variables for Granger Causality test, plotting pause time, texture description, and output file name
GCord = 30  # Set order parameter for Granger Causality
PausePlot = .1  # Time to pause to visualize M1 & M2 plots (set to 0 for no pause)
Texture="H"

# Select the folder name basis the folder being analyzed
folderName="BorodinPart4_File1"
# folderName="BrahmsConcertPart1_File1"

# Select by uncommeting the basis the file under analysis

# Combinations for Borodin
combinations <- combn(1:4, 2, simplify = FALSE)

# Combinations for Brahms
# combinations <- combn(1:5, 2, simplify = FALSE)

# Set working directory
workingDir <- paste0("C:/San/PhD/Project/Causality/GrangerCausality/PetersCode/GrangerCausality-Peter/ICMPCanalysis2/DataForCausalityStudies/Borodin_New/", folderName)
setwd(workingDir)  # Set path to folder with csv-files

# Initialize an empty dataframe to store all results
output_all <- data.frame()

# Loop through all the combinations and process each file
for(combo in combinations){
  combo_suffix <- paste0("_c", combo[1], "_c", combo[2])
  fileNameConc <- paste0(folderName, combo_suffix)
  
  if (!file.exists(paste0(fileNameConc, ".csv"))) next  # Skip if file does not exist
  
  data <- read.csv(paste0(fileNameConc, ".csv"))
  
  attach(data)
  
  orderM1=0
  orderM2=0
  
  tempM1 <- M1
  tempM2 <- M2

  repeat{
    tempM1 <- diff(log(tempM1))
    rM1 <- adf.test(tempM1)
    orderM1 <- orderM1+1;
    
    if (rM1[['p.value']]<0.05 ){
      print("M1 is now stationary")
      print(paste0("Order is ",orderM1))
      break}
  }
  
  
  repeat{
    tempM2 <- diff(log(tempM2))
    rM2 <- adf.test(tempM2)
    orderM2 <- orderM2+1
    
    if (rM2[['p.value']]<0.05 ){
      print("M2 is now stationary")
      print(paste0("Order is ",orderM2))
      break}
  }
  

  M1_M2 <- grangertest(tempM1, tempM2, order=GCord)  # Use temporary variables
  F_M1_M2 <- M1_M2[-1, "F"]
  p_M1_M2 <- M1_M2[-1, "Pr(>F)"]
  
  M2_M1 <- grangertest(tempM2, tempM1, order=GCord)  # Use temporary variables
  F_M2_M1 <- M2_M1[-1, "F"]
  p_M2_M1 <- M2_M1[-1, "Pr(>F)"]
  
  detach(data)
  
  # Append the results to the output dataframe
  output_row <- data.frame(data_file = fileNameConc, Texture = Texture, F_M1_M2 = F_M1_M2, p_M1_M2 = p_M1_M2,
                           F_M2_M1 = F_M2_M1, p_M2_M1 = p_M2_M1, M1_stationarity_p = rM1$p.value, M2_stationarity_p = rM2$p.value)
  
  output_all <- rbind(output_all, output_row)
}

# Save the output to a file
outputFileName <- paste0(folderName, "_GrangerOutput_All.txt")
write.csv(output_all, file=outputFileName, quote=F, row.names=F, na='NA')
print("Results successfully saved")