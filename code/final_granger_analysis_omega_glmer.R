# Analysis of Omega Ensemble Granger Causality (GC) data
# Runs linear mixed models (LMM) and binomial generalized mixed models (GLMM)
# Select whether to analyze with Bonferroni correction (BFC) or uncorrected (UC) in lines 54/55 & lines 184/185 

rm(list=ls()) # Clear work Environment

# Libraries & options ----------
options(scipen = 100, digits = 4)

library(afex) 
library(dplyr)
library(forcats)
library(ggeffects) 
library(ggplot2)
library(ggsci)
library(huxtable)
library(lme4) 
library(lsmeans)
library(readr)
library(reshape2)
library(Rmisc)
library(rstatix)
library(stargazer) 
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
# source("~/Documents/R/geom_flat_violin.R") # set own path!!!

# Set working directory ---------
setwd("~/Documents/Work-MARCS/EXPERIMENTS/Omega Ensemble/Omega Ensemble Rehearsal/Analysis_2020/Sanket_analysis/Granger Causality") # set path to folder with csv-files

# Set path to Final Brahms Results and Final Borodin Results files. Placeholder left below.
data_brahms <- data.frame(read_csv('~/Documents/..<enter your path here>../Final_Brahms_Results_GC.csv'))
data_borodin <- data.frame(read_csv('~/Documents/..<enter your path here>../Final_Borodin_Results_GC.csv'))

## Select 'concert' data to process
data_brms <- data_brahms
data_brdn <- data_borodin

# Add columns with binary values following Bonferroni correction: p = .05/K (where K is the number of GC tests, one per musician pairing, per excerpt) 
# For Brahms Quintet (K = 10): corrected critical p-value = .005
# For Borodin Quartet (K = 6): corrected critical p-value = .008
data_brms$p_M1_M2_BFC <- with(data_brms, ifelse(p_M1_M2 < .005, 1, 0))
data_brms$p_M2_M1_BFC <- with(data_brms, ifelse(p_M2_M1 < .005, 1, 0))
data_brdn$p_M1_M2_BFC <- with(data_brdn, ifelse(p_M1_M2 < .008, 1, 0))
data_brdn$p_M2_M1_BFC <- with(data_brdn, ifelse(p_M2_M1 < .008, 1, 0))

# Combine both pieces into 1 data frame
data_both <- rbind(data_brms, data_brdn)

# Set up for P value analysis
dataPv_UC <- melt(data_both, id.vars = c('piece', 'part', 'phrase', 'texture', 'pair'), measure.vars = c('p_M1_M2_Bin', 'p_M2_M1_Bin'), variable.name = 'direction', value.name = "p_sig")
# With Bonferroni correction
dataPv_BFC <- melt(data_both, id.vars = c('piece', 'part', 'phrase', 'texture', 'pair'), measure.vars = c('p_M1_M2_BFC', 'p_M2_M1_BFC'), variable.name = 'direction', value.name = "p_sig")

# Select whether to analyze with Bonferroni correction (BFC) or uncorrected (UC) !!!! Comment-out (#) the one you don't want
dataPv <- dataPv_UC # Uncorrected
#dataPv <- dataPv_BFC # With Bonferroni correction

dataPv$phrase <- as.factor(dataPv$phrase) # 'Phrase' here refers to each excerpt extracted from the performances

## Analysis of PROPORTIONS of significant GC values -------
# Linear Mixed Effects Model (LMM) analysis

# Compute proportions across pairs
data_p_prop <- dataPv %>% group_by(piece, part, phrase, texture, direction) %>% summarise_at(vars(p_sig), mean)
data_p_prop <- data.frame(data_p_prop)

# Run Shapiro-Wilk test of normality. Can assume normality if p > .05
data_p_prop %>%
  group_by(texture) %>%
  shapiro_test(p_sig) # Neither Homophonic nor Polyphonic proportion data are normally distributed. This justifies GLMER.

# Run LMM
Pv_long_prop <- lmer(p_sig ~ 1 + texture + (1 | direction) + (1 | piece/part/phrase), data = data_p_prop,
                     control = lmerControl(optimizer = "bobyqa"), REML = FALSE)
Pv_long_prop.output <- summary(Pv_long_prop) # Print model summary to console
Pv_long_prop.output 
capture.output(Pv_long_prop.output, file = "ProportionSigGC.txt") # Write output to text file

# save model output as a text file
class(Pv_long_prop) <- "lmerMod"
stargazer(Pv_long_prop, type = "text", title='LMER Summary Statistics - Proportion Significant', out='Pv_long_prop_table.txt')

# Compute confidence intervals
Pv_long_prop.ci <- confint(Pv_long_prop)
Pv_long_prop.ci
capture.output(Pv_long_prop.ci, file = "ProportionSigGC_CI.txt")

# Now run a reduced model excluding the fixed factor of 'texture'.
Pv_long_prop_reduced <- lmer(p_sig ~ 1 + (1 | direction) + (1 | piece/part/phrase), data = data_p_prop,
                        control = lmerControl(optimizer = "bobyqa"), REML = FALSE)
summary(Pv_long_prop_reduced) # Print model summary to console

# Compare full and reduced models to see whether the model that includes texture explains more variance in GC
Pv_long_prop.anova <- anova(Pv_long_prop, Pv_long_prop_reduced) # Check output to see whether the Chi-square value is significant
Pv_long_prop.anova
capture.output(Pv_long_prop.anova, file = "ProportionSigGC_model_eval.txt") # Write output to text file

# Raincloud plot ----

summary_data_p_prop <- summarySE(data = data_p_prop, measurevar = "p_sig", groupvars = "texture", 
                                       na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)

# set up the theme here for raincloud plots 
raincloud_theme = theme(
  text = element_text(size = 14),
  axis.title.x = element_text(size = 24, vjust = 0),
  axis.title.y = element_text(size = 24, vjust = 2),
  axis.text = element_text(size = 20, color = 'black'),
  legend.title=element_text(size=20),
  legend.text=element_text(size=20),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 28),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

GC_p_plot_long_prop <- data_p_prop  %>%
  mutate(texture = fct_relevel(data_p_prop$texture, "H", "P")) %>%
  ggplot(aes(y = p_sig, x = texture, fill = texture)) +
  geom_flat_violin(position = position_nudge(x = .175, y = 0), alpha = .8, show.legend = FALSE) +
  geom_point(aes(y = p_sig, x = texture, fill = texture), position = position_jitter(width = .06, height = 0), shape = 21, size = 2, color = "black", alpha = 0.5, show.legend = FALSE) +
  geom_errorbar(data = summary_data_p_prop, aes(x = texture, y = p_sig, ymin = p_sig-ci, ymax = p_sig+ci), position = position_nudge(c(-.175, -.175)), colour = "black",
                width = 0.08, size = 0.6) +
  geom_point(data = summary_data_p_prop, aes(x = texture, y = p_sig, fill = texture), position =
               position_nudge(c(-.175, -.175)), shape = 21, size = 5, colour = "black", fill = c('orange', 'royalblue'), show.legend = FALSE) +
  expand_limits(x = 3) +
  scale_color_ucscgb() +
  scale_fill_ucscgb() +
  scale_fill_manual(values=c('orange', 'royalblue')) +
  labs(x="Texture", y = "Proportion")+
  scale_x_discrete("Texture", labels = c("H" = "Homophonic","P" = "Polyphonic")) +
  theme_bw() +
  raincloud_theme

GC_p_plot_long_prop
ggsave("GC_p_plot_long_prop.png", dpi=600)


# Binomial Generalized Linear Mixed Effects Model (GLMM) on binary data ----
# Binary values indicate whether (1) or not (0) each individual Granger test was statistically significant.

Pv_binary.glme <- glmer(p_sig ~ 1 + texture + (1 | direction/pair) + (1 | piece/part/phrase),  family=binomial, data = dataPv,
                           control = glmerControl(optimizer = "bobyqa"))
Pv_binary.output <- summary(Pv_binary.glme)
Pv_binary.output
capture.output(Pv_binary.output, file = "Pv_binary_GLMER.txt") # Write output to text file

# Compute confidence intervals (does not run for Bonferroni corrected data)
Pv_binary.glme.ci <- confint(Pv_binary.glme) 
Pv_binary.glme.ci
capture.output(Pv_binary.glme.ci, file = "BinaryGLMM_SigGC_CI.txt")

# Reduced model with random effects only
Pv_binary_random.glme <- glmer(p_sig ~ 1 + (1 | direction/pair) + (1 | piece/part/phrase),  family=binomial, data = dataPv,
                                control = glmerControl(optimizer = "bobyqa"))
Pv_binary_random.output <- summary(Pv_binary_random.glme)
Pv_binary_random.output
capture.output(Pv_binary_random.output, file = "Pv_binary_random_GLMER.txt") # Write output to text file

# Compare the full and reduced model
Pv_binary_comparison <- anova(Pv_binary.glme, Pv_binary_random.glme)
Pv_binary_comparison
capture.output(Pv_binary_comparison, file = "Pv_binary_comparison.txt") # Write output to text file

# Make a table
Pv_binary_GLMER.table <- huxreg(Pv_binary.glme, Pv_binary_random.glme, statistics = NULL, number_format = 2)
Pv_binary_GLMER.table
huxtable::quick_docx(Pv_binary_GLMER.table, file = "Pv_binary_GLMER_table.docx") # Make Word table (not standardized coefficients)


# Analysis of effects of MELODY INSTRUMENT ------
# Linear Mixed Effects Model (LMM) and Analysis of Variance (ANOVA) with planned contrasts

# First need to add column to indicate whether GC data in each row are for: 
# (1) melody influencing other (Melody_to_Other), (2) other influencing melody (Other_to_Melody), or other influencing other (Other_to_Other), in Homophonic textures
# or 'Mixed' in polyphonic textures

dataPv_melody_UC <- melt(data_both, id.vars = c('piece', 'part', 'phrase', 'texture', 'pair', 'musician1', 'musician2', 'melody_instrument'), measure.vars = c('p_M1_M2_Bin', 'p_M2_M1_Bin'), variable.name = 'direction', value.name = "p_sig")
# With Bonferroni
dataPv_melody_BFC <- melt(data_both, id.vars = c('piece', 'part', 'phrase', 'texture', 'pair', 'musician1', 'musician2', 'melody_instrument'), measure.vars = c('p_M1_M2_BFC', 'p_M2_M1_BFC'), variable.name = 'direction', value.name = "p_sig")

# Select whether to analyze with Bonferroni correction (BFC) or uncorrected (UC) !!!! Comment-out (#) the one you don't want
#dataPv_melody <- dataPv_melody_UC
dataPv_melody <- dataPv_melody_BFC 


dataPv_melody$leadership <- ifelse(dataPv_melody$melody_instrument == 'Mixed', 'Mixed',
                                   ifelse(dataPv_melody$melody_instrument == dataPv_melody$musician1 & grepl("p_M1_M2", dataPv_melody$direction), 'Melody_to_Other',
                                          ifelse(dataPv_melody$melody_instrument == dataPv_melody$musician2 & grepl("p_M2_M1", dataPv_melody$direction), 'Melody_to_Other',
                                                 ifelse(dataPv_melody$melody_instrument == dataPv_melody$musician2 & grepl("p_M1_M2", dataPv_melody$direction), 'Other_to_Melody',
                                                        ifelse(dataPv_melody$melody_instrument == dataPv_melody$musician1 & grepl("p_M2_M1", dataPv_melody$direction), 'Other_to_Melody', 'Other_to_Other')))))
dataPv_melody$leadership <- as.factor(dataPv_melody$leadership)
#dataPv_melody <- melt(data_both, id.vars = c('piece', 'part', 'phrase', 'texture', 'pair', 'leadership'), measure.vars = c('p_M1_M2_Bin', 'p_M2_M1_Bin'), variable.name = 'direction', value.name = "p_sig")

# Compute proportion data
data_p_prop_melody <- dataPv_melody %>% group_by(piece, part, phrase, leadership) %>% summarise_at(vars(p_sig), mean)
data_p_prop_melody <- data.frame(data_p_prop_melody)

data_p_prop_melody$leadership <- as.factor(data_p_prop_melody$leadership)

# Run Shapiro-Wilk test of normality. Can assume normality if p > .05
data_p_prop_melody %>%
  group_by(leadership) %>%
  shapiro_test(p_sig) # Most data are not normally distributed. This justifies GLMER

# First run the full model to test the effect of the fixed factor 'leadership' on whether or not GC is significant  
Pv_long_melody <- lmer(p_sig ~ 1 + leadership + (1 | piece/part/phrase), data = data_p_prop_melody,
                       control = lmerControl(optimizer = "bobyqa"), REML = FALSE)
Pv_long_melody.output <- summary(Pv_long_melody) # Print model summary to console
Pv_long_melody.output
capture.output(Pv_long_melody.output, file = "ProportionSigGC_Melody.txt") # Write output to text file

# save model output as a text file
class(Pv_long_melody) <- "lmerMod"
stargazer(Pv_long_melody, type = "text", title='LMER Summary Statistics - Proportion Significant', out='Pv_long_melody_table.txt')

# Pairwise comparisons (exhaustive - don't use; use planned orthogonal contrasts instead)
Melody_Pairwise.comp <- lsmeans(Pv_long_melody, pairwise ~ leadership, mult.name = "Leadership", pbkrtest.limit = 9999)
Melody_Pairwise.comp
write.table(Melody_Pairwise.comp, file = "SigGC_Melody_pairwise_comparisons.txt", sep = "") 

# ANOVA with orthogonal planned contrasts: (1) Homophonic vs Polyphonic; (2) Pairing with Melody vs No Melody; (3) Melody-to-Other vs Other-to-Melody
levels(data_p_prop_melody$leadership) # Check order of conditions (important for specifying contrast coefficients)
Melody_PlannedComp <- lsmeans(Pv_long_melody, "leadership")
HvsP = c(1,-3,1,1) # Contrast comparing Homophonic with Polyphonic - this comparison is redundant with the main analysis
MELvsNoMEL = c(1,0,1,-2) # Contrast comparing pairings including Melody player vs pairings excluding Melody player
MOvsOM = c(1,0,-1,0) # Contrast comparing Melody-to-Other vs Other-to-Melody
Contrasts = list(HvsP, MELvsNoMEL, MOvsOM)
Melody_PlannedComp.output <- contrast(Melody_PlannedComp, Contrasts, adjust="none") # No need for adjust for multiple comparisons since contrasts are planned & orthogonal
capture.output(Melody_PlannedComp.output, file = "SigGC_Melody_planned_contrasts.txt")
Melody_PlannedComp.output

# Compute confidence intervals
Melody_PlannedComp.output.ci <- confint(Melody_PlannedComp.output)
Melody_PlannedComp.output.ci
capture.output(Melody_PlannedComp.output.ci, file = "Melody_PlannedComp.output_CI.txt")

# Now run a reduced model excluding the fixed factor of 'leadership'.
Pv_long_melody_reduced <- lmer(p_sig ~ 1 + (1 | piece/part/phrase), data = data_p_prop_melody,
                               control = lmerControl(optimizer = "bobyqa"), REML = FALSE)
summary(Pv_long_melody_reduced) # Print model summary to console

# Compare full and reduced models to see whether the model that includes texture explains more variance in GC
Pv_long_melody.anova <- anova(Pv_long_melody, Pv_long_melody_reduced) # Check output to see whether the Chi-square value is significant
Pv_long_melody.anova
capture.output(Pv_long_melody.anova, file = "ProportionSigGC_Melody_model_eval.txt") # Write output to text file

# Raincloud plot  

summary_melody_p_prop <- summarySE(data = data_p_prop_melody, measurevar = "p_sig", groupvars = "leadership", 
                                 na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)

GC_p_plot_leadership <- data_p_prop_melody  %>%
  mutate(leadership = fct_relevel(data_p_prop_melody$leadership, "Melody_to_Other", "Other_to_Melody", "Other_to_Other", "Mixed")) %>%
  ggplot(aes(y = p_sig, x = leadership, fill = leadership)) +
  geom_flat_violin(position = position_nudge(x = .175, y = 0), alpha = .8, show.legend = FALSE) +
  geom_point(aes(y = p_sig, x = leadership, fill = leadership), position = position_jitter(width = .06, height = 0), shape = 21, size = 2, color = "black", alpha = 0.5, show.legend = FALSE) +
  geom_errorbar(data = summary_melody_p_prop, aes(x = leadership, y = p_sig, ymin = p_sig-ci, ymax = p_sig+ci), position = position_nudge(c(-.175, -.175, -.175, -.175)), colour = "black",
                width = 0.08, size = 0.6) +
  geom_point(data = summary_melody_p_prop, aes(x = leadership, y = p_sig, fill = leadership), position =
               position_nudge(c(-.175, -.175, -.175, -.175)), shape = 21, size = 4, colour = "black", fill = c('#A2B59F', 'lightgrey', 'darkgrey', '#B59F9F'), show.legend = FALSE) +
  expand_limits(x = 3) +
  scale_fill_manual(values=c('#A2B59F', 'lightgrey', 'darkgrey', '#B59F9F')) +
  labs(x="Direction of Influence", y = "Proportion")+
  scale_x_discrete("Direction of Influence", labels = c("Melody_to_Other" = "Melody\non Other", "Other_to_Melody" = "Other\non Melody", "Other_to_Other" = "Other\non Other", "Mixed" = "Mixed\nPolyphonic")) +
  theme_bw() +
  raincloud_theme

GC_p_plot_leadership
ggsave("GC_p_plot_leadership.png", dpi=600)


# Binomial Generalized Linear Mixed Effects Model (GLMM) on binary data for MELODY INSTRUMENT ----
# Binary values indicate whether (1) or not (0) each individual Granger test was statistically significant.

Pv_binary_melody.glme <- glmer(p_sig ~ 1 + leadership + (1 | direction/pair) + (1 | piece/part/phrase),  family=binomial, data = dataPv_melody,
                        control = glmerControl(optimizer = "bobyqa"))
Pv_binary_melody.output <- summary(Pv_binary_melody.glme)
Pv_binary_melody.output
capture.output(Pv_binary_melody.output, file = "Pv_binary_melody_GLMER.txt") # Write output to text file

# Find best-fitting random effects model
Pv_binary_melody_random.glme <- glmer(p_sig ~ 1 + (1 | direction/pair) + (1 | piece/part/phrase),  family=binomial, data = dataPv_melody,
                                       control = glmerControl(optimizer = "bobyqa"))
Pv_binary_melody_random.output <- summary(Pv_binary_melody_random.glme)
Pv_binary_melody_random.output
capture.output(Pv_binary_random.output, file = "Pv_binary_random_GLMER.txt") # Write output to text file

# Compare the full and reduced model
Pv_binary_melody_comparison <- anova(Pv_binary_melody.glme, Pv_binary_melody_random.glme)
Pv_binary_melody_comparison
capture.output(Pv_binary_melody_comparison, file = "Pv_binary_melody_comparison.txt") # Write output to text file

# Make a table
Pv_binary_melody_GLMER.table <- huxreg(Pv_binary_melody.glme, Pv_binary_melody_random.glme, statistics = NULL, number_format = 2)
Pv_binary_melody_GLMER.table
huxtable::quick_docx(Pv_binary_melody_GLMER.table, file = "Pv_binary_melody_GLMER_table.docx") # Make Word table (not standardized coefficients)

# Orthogonal planned contrasts: (1) Homophonic vs Polyphonic; (2) Pairing with Melody vs No Melody; (3) Melody-on-Other vs Other-on-Melody
levels(dataPv_melody$leadership) # Check order of conditions (important for specifying contrast coefficients)
Melody_PlannedComp.glme <- lsmeans(Pv_binary_melody.glme, "leadership")
HvsP = c(1,-3,1,1) # Contrast comparing Homophonic with Polyphonic - this comparison is redundant with the main analysis
MELvsNoMEL = c(1,0,1,-2) # Contrast comparing pairings including Melody player vs pairings excluding Melody player
MOvsOM = c(1,0,-1,0) # Contrast comparing Melody-on-Other vs Other-on-Melody
Contrasts = list(HvsP, MELvsNoMEL, MOvsOM)
Melody_PlannedComp.glme.output <- contrast(Melody_PlannedComp.glme, Contrasts, adjust="none") # No need for adjust for multiple comparisons since contrasts are planned & orthogonal
capture.output(Melody_PlannedComp.glme.output, file = "SigGC_Melody_GLMM_planned_contrasts.txt")
Melody_PlannedComp.glme.output

# Compute confidence intervals
Melody_PlannedComp.glme.output.ci <- confint(Melody_PlannedComp.glme.output)
Melody_PlannedComp.glme.output.ci
capture.output(Melody_PlannedComp.glme.output.ci, file = "Melody_GLMM_PlannedComp.output_CI.txt")
