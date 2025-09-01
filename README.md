# Survival Analysis Shiny App

This Shiny application provides an interactive interface to perform **basic survival analysis** using Kaplan–Meier estimators.  
It is designed for medical and clinical research data where users can upload their own datasets and explore survival outcomes.

---

## Features

- **Excel file upload** (`.xlsx`) with patient data  
- Dynamic selection of variables:
  - Patient ID  
  - Diagnosis date  
  - Last follow-up date  
  - Vital status (1 = deceased, 0 = alive)  
  - Optional binary grouping variable (0/1) to compare survival curves  
- **Kaplan–Meier estimation** with:
  - Survival curves  
  - Risk tables  
  - Confidence intervals  
- **Time scale selector**: display survival time in days, months, or years  

---

## Requirements

The app uses the following R packages:

```r
shiny
readxl
dplyr
survival
ggsurvfit
ggplot2
lubridate