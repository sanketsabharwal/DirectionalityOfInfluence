# Script for doing Granger Causality analysis on M1 & M2 time series
clearhistory <- function() {
  write("", file=".blank")
  loadhistory(".blank")
  unlink(".blank")
}
clearhistory()

rm(list=ls()) # Clear work Environment
library(stringr)
library(lmtest) ## This provides grangertest
library(tseries) # For stationarity test (kpss.test)
#library(TSA) #For pre-whitening process to remove non-stationarity in a bi-variate time-series
PausePlot = .1; # Time to pause to visualize M1 & M2 plots (set to 0 for no pause)
options(scipen = 100, digits = 4)

setwd("C:/San/PhD/Project/Causality/GrangerCausality/PetersCode/GrangerCausality-Peter/ICMPCanalysis2/Results/Borodin_New/AllCombos") # set path to folder with csv-files
listResults <- dir(pattern = "*.txt") # Creates a list of all the csv files in the directory

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
