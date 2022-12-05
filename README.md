# AMUSE-SEFASI-Sharing
This repository contains the code and data needed to replicate the results of our paper "Drivers of antibiotic use in semi-intensive poultry farms: evi-dence from a survey in Senegal"

This repository contains thre files:

survey_responses.xlsx --> the (anonymised) survey responses from our data collection exercise
Data cleaning (sharing).R --> an R script which cleans the survey responses and generates the dataset used in our analysis
Data analysis (sharing).R --> an R script which sues the cleaned dataset to generate our results

To run the code, simply clone the repository to your desktop. First, run the R script "Data cleaning (sharing)", which will generate the cleaned dataset. 
Then, run the R script "Data analysis (sharing)", which will generate and save our results.

In order to replicate our results, you will need to install R and Rstudio, as well as the following R packages:
Plry
data.table
readxl
stargazer
tidyverse
xlsx
ggplot2
reshape2
here
patchwork
dplyr
doBy
Hmisc
corrplot
multcomp
sampleSelection
