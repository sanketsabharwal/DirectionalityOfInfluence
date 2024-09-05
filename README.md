# Directionality of Influence - Granger Causality Analysis Repository

This repository contains scripts and instructions for performing a three-step Granger Causality analysis on M1 & M2 time series data. The analysis is split into three separate R scripts, each with its specific task within the process. Here M1 and M2 stands for Musician 1 and Musician 2 since our use case for this paper is a musical ensemble. The scripts are located in the `code` folder.

The analysis is divided into three major steps, each within its own script file within the `code` folder.

## Getting Started

1. Ensure you have R installed on your machine.
2. Clone this repository to your local machine.
3. Open your R environment and set the working directory to the cloned repository folder.
4. Install the necessary libraries

## Prerequisites

Ensure you have the following R libraries installed:
- `lmtest`
- `tseries`
- `psd`
- `stringr`
- `readr`
- `purrr`
- `dplyr`
- `fs`

You can install any missing libraries using the following command in R:

```r
install.packages("library_name")
```

## Directory Structure

- `/code`: This folder contains the three script files for each step of the analysis.
- `/data`: This is where our data resides. It should contain a folder for each dataset (e.g., `Borodin`, `Brahms` etc.) with the necessary CSV files. Each folder contains the data for each piece performed. Within each csv file (for eg. BorodinPart1_File1_c1_c2.csv) the position time series of each performer has been placed under the columns M1 and M2 respectively. The c1 and c2 represents the arragagement of musician 1 under column 1, and musician 2 under column.
- `/selected_phrases`: This directory contains a file that lists the selected phrases along with their corresponding mapped melody instruments.
- `/example_results`: This folder contains exemplar final CSV files that have been processed to include mappings to melody instruments. These files serve as inputs for the script final_granger_analysis_omega_glmer.r located in the code folder. Executing this script with these files will yield the statistical analysis and generate the corresponding rainbow plots.

## Step 1: Granger Causality Test

1. Navigate to the `code` directory.
2. Open the script file for Step 1.
3. Specify the `folderName` and `combinations` parameters to match your dataset and the combinations of time series you wish to analyze.
   - `folderName`: The name of the folder containing your CSV files (e.g., `BorodinPart4_File1`).
   - `combinations`: The combinations of time series to analyze (e.g., `combn(1:4, 2, simplify = FALSE)` for Borodin).
4. Execute the script. This will produce a `GrangerOutput_All.txt` file inside the specified folder.

## Step 2: Bivariate Analysis

1. Ensure the `GrangerOutput_All.txt` file is present in the specified folder from Step 1.
2. Open the script file for Step 2.
3. Update the `working_directory` variable to point to the folder containing the `GrangerOutput_All.txt` file.
4. Execute the script. This will process the Granger results and generate a CSV file named `[FolderName]_GC_Allcombos` (e.g., `BorodinPart4_File1_GC_Allcombos`).

## Step 3: Aggregation and Formatting

1. After running Step 2 for all required folders, gather the output CSV files ending with `_GC_Allcombos` and place them inside a new folder named `AllCombos` within the dataset folder (e.g., `Borodin/AllCombos`).
2. Open the script file for Step 3.
3. Update the `working_directory` variable to point to the `AllCombos` folder.
4. Execute the script. This will aggregate the data and produce a final CSV file named `Final_[DatasetName]_Results_GC.csv` (e.g., `Final_Borodin_Results_GC.csv`).
5. Open the final CSV file and manually map the melody instrument to each file as the last column.


## Omega Ensemble Granger Causality Analysis

This section provides steps to execute the script `final_granger_analysis_omega_glmer.r` which performs an analysis on the Omega Ensemble Granger Causality data. Before executing this script, ensure that the CSV files located in the `example_results` folder have been updated according to the instructions in the "Aggregation and Formatting" section.

### Execution Steps:

1. Ensure that R is running and open the script file `final_granger_analysis_omega_glmer.r` located in the `code` folder.
2. Update the following lines to point to the correct file paths on your machine:
   
   - data_brahms <- data.frame(read_csv('C:/..<enter your folder>../DirectionalityOfInfluence/example_results/Final_Brahms_Results_GC.csv'))
   - data_borodin <- data.frame(read_csv('C:/..<enter your folder>../DirectionalityOfInfluence/example_results/Final_Borodin_Results_GC.csv'))
   - Change the file paths `C:/..<enter your folder>../DirectionalityOfInfluence/example_results/Final_Brahms_Results_GC.csv'` and `C:/..<enter your folder>../DirectionalityOfInfluence/example_results/Final_Borodin_Results_GC.csv'` to the actual paths where the CSV files are located on your machine.
3. Once the file paths have been updated, execute the script. This script will run the necessary statistical analysis and generate the corresponding plots as specified in the script.

### Result Analysis:

The script `final_granger_analysis_omega_glmer.r` performs various statistical analyses including Linear Mixed Effects Models and Generalized Linear Mixed Effects Models to analyze the Granger Causality data. It computes proportions of significant Granger Causality values, compares the proportions across different conditions, and plots the results using raincloud plots. Also, it analyzes the effects of melody instruments on Granger Causality, among other analyses.

The results of the analyses are saved in text files and png plots in the working directory, which is the folder `outputs` and plots are saved as PNG files as specified in the script. You can review these files to interpret the findings from the analysis.

### Notes and Troubleshooting:

The folder "AllCombos" is placed within both "borodin" and "brahms" directories located in the "data" folder, serving as a placeholder to provide researchers an indication of what to expect.

If you encounter any issues while executing the script, ensure that:
- All the required libraries are installed.
- The file paths are correctly specified.
- The R environment is properly set up.

In case of further issues or inquiries regarding the analysis, feel free to contact me on sanket@sabharwal.dev or refer to the comments within the script for additional information.

If you found this repo useful please cite us! :)
```
Sabharwal SR, Breaden M, Volpe G, Camurri A, Keller PE (2024) Leadership dynamics in musical groups: Quantifying effects of musical structure on directionality of influence in concert performance videos. PLoS ONE 19(4): e0300663. https://doi.org/10.1371/journal.pone.0300663
```
