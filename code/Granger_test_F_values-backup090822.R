# Script for doing Granger Causality analysis on M1 & M2 time series

rm(list=ls()) # Clear work Environment

library(lmtest) ## This provides grangertest
library(tseries) # For stationarity test (kpss.test)
#library(TSA) #For pre-whitening process to remove non-stationarity in a bi-variate time-series
library(psd) #For pre-whitening process to remove non-stationarity in a uni-variate time-series
GCord = 30; # Set order parameter for Granger Causality
PausePlot = .05; # Time to pause to visualize M1 & M2 plots (set to 0 for no pause)
Texture="H"
outputFileName="GrangerOutput-NP.txt"

setwd("C:/San/PhD/Project/Causality/GrangerCausality/PetersCode/GrangerCausality-Peter/ICMPCanalysis2/DataForCausalityStudies/Brahms_New/BrahmsConcertPart1_File1") # set path to folder with csv-files
listcsv <- dir(pattern = "*.csv") # Creates a list of all the csv files in the directory

output_all <- data.frame(matrix(nrow = length(listcsv), ncol = 8)) # Make dataframe to store output values
colnames(output_all) <- c('data_file','Texture' ,'F_M1_M2', 'p_M1_M2', 'F_M2_M1', 'p_M2_M1', 'M1 stationarity p', 'M2 stationarity p') # Column headers for output

for (k in 1:length(listcsv)){
  data1 <- read.csv(listcsv[k])
  tapno <- c(1:nrow(data1))
  data1 <- cbind(data1, tapno)
  rm(tapno)
  attach(data1)
  
  ## Visualize M1 and M2 series
  
  par(mfrow = c(2,1))
  plot(M1, type = "l", ylab = "M1 - trajectory", xlab = "Frame") 
  plot(M2, type = "l", ylab = "M2 - trajectory", xlab = "Frame")
  
  Sys.sleep(PausePlot) # Pause to visualize M1 & M2 plots
  
  ## Test stationarity
  # Can assume that the time series is NOT trend stationary if p < .05
  # If ADF p < 0.05 the series is stationary
  stat_M1 <- adf.test(M1) 
  stat_M2 <- adf.test(M2)
  
  ## Assessing influence of M1 on M2 & vice versa using Granger Causality
  
  M1_M2 <- grangertest(M1, M2, order=GCord) 
  F_M1_M2 <- M1_M2[-1, "F"]
  p_M1_M2 <- M1_M2[-1, "Pr(>F)"]

  M2_M1 <- grangertest(M2, M1, order=GCord) 
  F_M2_M1 <- M2_M1[-1, "F"]
  p_M2_M1 <- M2_M1[-1, "Pr(>F)"]

  output_all[k , ] <- c(listcsv[k], Texture, F_M1_M2, p_M1_M2, F_M2_M1, p_M2_M1, stat_M1$p.value, stat_M2$p.value)

detach(data1)
}

write.csv(output_all, file=outputFileName, quote=F, row.names=F, na='NA') # Save output to csv file with .txt extention in same directory as data file

# Arrange output for analysis and plotting

library(reshape2)

output_F <- melt(output_all, id.var = 'data_file', measure.vars = c('F_M1_M2', 'F_M2_M1'), variable.name = 'Direction', value.name = "F_value")
output_F$F_value <- as.numeric(output_F$F_value)
# Plot results ----

# Raincloud plots
library(ggplot2)
library(forcats)
library(ggsci)
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
#source("/Applications/R/geom_flat_violin.R")

raincloud_theme = theme(
  text = element_text(size = 14),
  axis.title.x = element_text(size = 24, vjust = 0),
  axis.title.y = element_text(size = 24, vjust = 2),
  axis.text = element_text(size = 20, color = 'black'),
  #axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=20),
  legend.text=element_text(size=20),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 32),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

GC_F_plot <- output_F %>%
  mutate(Direction = fct_relevel(output_F$Direction, "F_M1_M2", "F_M2_M1")) %>%
  ggplot(aes(y = F_value, x = Direction, fill = Direction)) +
  #ggplot(aes(y = log(F_value), x = Direction, fill = Direction)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = F_value, x = Direction, fill = Direction), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  #geom_point(aes(y = log(F_value), x = Direction, fill = Direction), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  guides(fill = "none") +
  guides(color = "none") +
  scale_color_ucscgb() +
  scale_fill_ucscgb() +
  #coord_flip() +
  labs(title="Granger F values", 
       x="Direction", y = "F value")+
  scale_x_discrete("Direction", labels = c("F_M1_M2" = "M1 > M2","F_M2_M1" = "M2 > M1")) +
  theme_bw() +
  raincloud_theme

GC_F_plot
ggsave("GC_F_plot.png", dpi=600)