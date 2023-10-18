# Script for doing Granger Causality analysis on M1 & M2 time series
clearhistory <- function() {
  write("", file=".blank")
  loadhistory(".blank")
  unlink(".blank")
}
clearhistory()

rm(list=ls()) # Clear work Environment
options(scipen = 100, digits = 4)

library(lmtest) ## This provides grangertest
library(tseries) # For stationarity test (kpss.test)
#library(TSA) #For pre-whitening process to remove non-stationarity in a bi-variate time-series
PausePlot = .1; # Time to pause to visualize M1 & M2 plots (set to 0 for no pause)

# Specify the file name of the aggregated data
folderName = "BorodinPart4_File1"
aggFileName <- paste0(folderName,"_GrangerOutput_All")

workingDir <- paste0("C:/San/PhD/Project/Causality/GrangerCausality/PetersCode/GrangerCausality-Peter/ICMPCanalysis2/DataForCausalityStudies/Borodin_New/", folderName)
setwd(workingDir)  # Set path to folder with csv-files

# Read the long data file
output_all <- read.delim(paste0(aggFileName,".txt"), sep = ",")

# Arrange output for analysis and plotting
library(reshape2)

output_F <- melt(output_all, id.var = 'data_file', measure.vars = c('F_M1_M2', 'F_M2_M1'), variable.name = 'Direction', value.name = "F_value")
output_F$F_value <- as.numeric(output_F$F_value)
write.csv(output_F, file=paste0(folderName,"_GC_Allcombos_FValues.txt"), quote=F, row.names=F, na='NA') # Save output to xlsx file with .txt extention in same directory as data file

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
  #ggplot(aes(y = F_value, x = Direction, fill = Direction)) +
  ggplot(aes(y = log(F_value), x = Direction, fill = Direction)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  #geom_point(aes(y = F_value, x = Direction, fill = Direction), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_point(aes(y = log(F_value), x = Direction, fill = Direction), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  guides(fill = "none") +
  guides(color = "none") +
  scale_color_ucscgb() +
  scale_fill_ucscgb() +
  #coord_flip() +
  labs(title=paste0(folderName," | Texture: ",output_all$Texture[1]), 
       x="Direction", y = "F value")+
  scale_x_discrete("Direction", labels = c("F_M1_M2" = "M1 > M2","F_M2_M1" = "M2 > M1")) +
  theme_bw() +
  raincloud_theme

GC_F_plot
ggsave(paste0(folderName,"_",output_all$Texture[1],"_GC_F_plot.png"), dpi=600)
