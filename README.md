# Depression Prevention Descriptive Dashboard

**Date of Release:** 10/29/2025  
**Title:** Findings From School-Based Depression Prevention Research<br>
**Repo Authors:** Aden Bhagwat & Shaina Trevino 

## **🔹 Overview**

This repository contains the code and data to produce our interactive [Depression Prevention Descriptive Dashboard](https://hedco-institute.shinyapps.io/depression_descriptive_dashboard/). 

This repository follows **[AEA Data and Code Availability Standards](https://datacodestandard.org/)** and includes:
- Datasets used to generate reported results.
- R code necessary to reproduce quantitative results reported.
- Computational environment details to ensure reproducibility.


## **🔹 Data and Code Availability Statement**
### **Data Sources**
The data used in this dashboard include study characteristics and effect size information from our larger [living systematic review on school-based depression prevention](https://github.com/HEDCO-Institute/Depression_Prevention_Overview).
- Datasets from the living review used for this dashboard reflect a fixed version of the data, captured during deployment. 
- Metadata (variable names and descriptions) are provided as the first tab of the "Depression_Overview" data files. Variables in the `app_data.csv` file are from the "Depression_Overview" data files so the metadata for those apply to the app_data.csv file.

The following datasets used for the dashboard are available in the `data` subfolder:


| Data File | Description | Data Structure |
|-----------|-------------|-----------| 
| `app_data.csv` | Extracted descriptive data for eligible reviews | One row per eligible review | 
| `Depression_Overview_Meta_Analysis_Primary_Study_Data.xlsx` | Extracted descriptive data for eligible primary studies | One row per eligible primary study |
| `Depression_Overview_Meta_Analysis_Data.xlsx` | Extracted effect size data for eligible primary studies | One row per effect size (outcome + group + timepoint)|
<br>

### **Code**
The scripts used to generate this dashboard are in the `code` subfolder:
- `app.R` contains all the code to load the app data and generate the dashboard
- `data_cleaning.R` contains code to load raw data, clean, and export the `app_data.csv` file for the dashboard


### **Handling of Missing Data**
- Missing values in the datasets are coded as `-999`, `Not Reported`, or `NA`, indicating that those values were not reported in studies/reviews.

## **🔹 Instructions for Replication**

### **Data Preparation and Analysis**
To replicate our results: 

**If you have Rstudio and Git installed and connected to your GitHub account:**

1. Clone the [repository](https://github.com/HEDCO-Institute/Depression_Prevention_Demographics) to your local machine ([click for help](https://book.cds101.com/using-rstudio-server-to-clone-a-github-repo-as-a-new-project.html#step---2))
1. Open the `Depression_Descriptive_Dashboard.Rproj` R project in R Studio (this should automatically activate the `renv` environment)
1. Open and run the `app.R` script 

**If you need to install or connect R, Rstudio, Git, and/or GitHub:**

1. [Create a GitHub account](https://happygitwithr.com/github-acct.html#github-acct)
1. [Install R and RStudio](https://happygitwithr.com/install-r-rstudio.html)
1. [Install Git](https://happygitwithr.com/install-git.html)
1. [Link Git to your GitHub account](https://happygitwithr.com/hello-git.html)
1. [Sign into GitHub in Rstudio](https://happygitwithr.com/https-pat.html)

**To reproduce our results without using Git and GitHub, you may use the following steps:** 

1. Download the ZIP file from the [repository](https://github.com/HEDCO-Institute/Depression_Prevention_Demographics)
1. Extract all files to your local machine
1. Open the `Depression_Descriptive_Dashboard.Rproj` R project in R Studio (this will automatically set the working directory and activate the `renv` environment)
1. Open and run the `app.R` script 


## **🔹 Computational Requirements**
### **Software Environment**
- **R Version:** 4.2.2  
- **Operating System:** Windows 10 Enterprise (x86_64-w64-mingw32/x64)  

### **Reproducing the Environment**
Opening the `Depression_Descriptive_Dashboard` R project will automatically install the correct package versions and set up the environment using the `renv` package. To manually load the environment:

1. Install `renv` (if not already installed):
```r
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
```

2. Restore any missing packages:
```r
renv::restore()
```

3. If needed, load the environment:
```r
renv::load()
```

Once the environment is restored, run the script starting with loading the necessary packages:
```r
pacman::p_load(tidyverse, rio, here, DT, shiny, plotly, openxlsx, countrycode, forestplot,
               reactable, htmltools, stringi, shinyWidgets, shinyjs, readr)
```


## **🔹 Licensing**
The code and data in this replication package are licensed under the Creative Commons Attribution 4.0 International License (CC BY 4.0); see the LICENSE file in the main root directory for full terms



## **🔹 Contact Information**
For questions about this replication package, contact:  
✉️ **Shaina Trevino** (strevino@uoregon.edu)  

