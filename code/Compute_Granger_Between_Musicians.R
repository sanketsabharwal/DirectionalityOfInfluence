# Load the necessary libraries for Granger Causality analysis, data manipulation, and visualization
library(lmtest)
library(tseries)
library(psd)
library(reshape2)
library(ggplot2)
library(forcats)
library(ggsci)

# Initialize variables for Granger Causality test, plotting pause time, texture description, and output file name
GCord <- 30
PausePlot <- .05
Texture <- "H"
outputFileName <- "GrangerOutput-NP.txt"
dirPath <- "C:/San/PhD/Project/Causality/GrangerCausality/PetersCode/GrangerCausality-Peter/ICMPCanalysis2/DataForCausalityStudies/Brahms_New/BrahmsConcertPart1_File1"

# Set working directory and get list of CSV files
setwd(dirPath)
listcsv <- dir(pattern = "*.csv")

# Prepare output dataframe
output_all <- data.frame(matrix(nrow = length(listcsv), ncol = 8))
colnames(output_all) <- c('data_file', 'Texture', 'F_M1_M2', 'p_M1_M2', 'F_M2_M1', 'p_M2_M1', 'M1 stationarity p', 'M2 stationarity p')


# Process each CSV file in the directory
for (k in 1:length(listcsv)){
  # Read the current CSV file and add a 'tapno' column representing the row number
  data1 <- read.csv(listcsv[k])
  tapno <- c(1:nrow(data1))
  data1 <- cbind(data1, tapno)
  rm(tapno)
  attach(data1)
  
  # Visualize the M1 and M2 time series from the current CSV file
  par(mfrow = c(2,1))
  plot(M1, type = "l", ylab = "M1 - trajectory", xlab = "Frame") 
  plot(M2, type = "l", ylab = "M2 - trajectory", xlab = "Frame")
  
  # Pause to allow visualization of the plots
  
  Sys.sleep(PausePlot)
  
  # Test the stationarity of the M1 and M2 time series using the Augmented Dickey-Fuller (ADF) test
  # Can assume that the time series is NOT trend stationary if p < .05, and if ADF p < 0.05 the series is stationary
  stat_M1 <- adf.test(M1) 
  stat_M2 <- adf.test(M2)
  
  # Assess the Granger Causality from M1 to M2 and from M2 to M1, using the specified order (GCord)
  
  M1_M2 <- grangertest(M1, M2, order=GCord) 
  F_M1_M2 <- M1_M2[-1, "F"]
  p_M1_M2 <- M1_M2[-1, "Pr(>F)"]
  
  M2_M1 <- grangertest(M2, M1, order=GCord) 
  F_M2_M1 <- M2_M1[-1, "F"]
  p_M2_M1 <- M2_M1[-1, "Pr(>F)"]
  
  # Store the F-statistic and p-value of the Granger Causality tests, along with the p-values of the ADF stationarity tests, in the output dataframe
  output_all[k , ] <- c(listcsv[k], Texture, F_M1_M2, p_M1_M2, F_M2_M1, p_M2_M1, stat_M1$p.value, stat_M2$p.value)
  
  detach(data1)
}

# Save the results of the Granger Causality analysis to a CSV file
write.csv(output_all, file=outputFileName, quote=F, row.names=F, na='NA')

# Reshape the output dataframe for plotting
output_F <- melt(output_all, id.var = 'data_file', measure.vars = c('F_M1_M2', 'F_M2_M1'), variable.name = 'Direction', value.name = "F_value")
output_F$F_value <- as.numeric(output_F$F_value)


# Load a custom plotting function for creating flat violin plots
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

# Define theme for plotting
raincloud_theme <- theme(
  text = element_text(size = 14),
  axis.title.x = element_text(size = 24, vjust = 0),
  axis.title.y = element_text(size = 24, vjust = 2),
  axis.text = element_text(size = 20, color = 'black'),
  legend.title = element_text(size = 20),
  legend.text = element_text(size = 20),
  legend.position = "right",
  plot.title = element_text(lineheight = .8, face = "bold", size = 32),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
  axis.line.y = element_line(colour = 'black', size = 0.5, linetype = 'solid')
)

# Create a plot of the Granger Causality F-statistics, grouped by direction of causality (M1 to M2 or M2 to M1)
GC_F_plot <- output_F %>%
  mutate(Direction = fct_relevel(Direction, "F_M1_M2", "F_M2_M1")) %>%
  ggplot(aes(y = F_value, x = Direction, fill = Direction)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = F_value, x = Direction, fill = Direction), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  guides(fill = "none") +
  guides(color = "none") +
  scale_color_ucscgb() +
  scale_fill_ucscgb() +
  labs(title = "Granger F values", 
       x = "Direction", y = "F value") +
  scale_x_discrete("Direction", labels = c("F_M1_M2" = "M1 > M2", "F_M2_M1" = "M2 > M1")) +
  theme_bw() +
  raincloud_theme

# Save the plot to a PNG file
GC_F_plot
ggsave("GC_F_plot.png", dpi=600)