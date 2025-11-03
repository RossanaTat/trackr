# trackr

## Overview

**trackr** is an R package designed to help users evaluate and project progress on any indicator across countries and time. It provides tools to:

- Calculate **progress scores** using two main methods:  
  - **Speed of progress** (how quickly a country advanced relative to typical trends)  
  - **Percentile ranking** (how a country compares to others in terms of performance)
- **Project future targets** for an indicator based on historical performance
- Output up to **six data frames** depending on the user’s chosen options and analysis settings

## Scoring Methods

- **Speed of Progress**  
  Measures how quickly a country progressed compared to a reference distribution.  
  For example, a score of `2.0` means a country advanced twice as fast as the typical experience.

- **Percentile Score**  
  Reflects the share of historical cases a country has outperformed.  
  A percentile score of `90` indicates the country performed better than 90% of observed experiences.

These methods can be used independently or together, depending on user preferences.

## Installation
remotes::install_github("RossanaTat/trackr")
or
devtools::install_github("RossanaTat/trackr")


## Example Usage
library(trackr)

data <- wbstats::wb_data(indicator = "EG.ELC.ACCS.ZS", lang = "en", country = "countries_only")

result <- track_progress(data = data, 
                         indicator = "EG.ELC.ACCS.ZS", 
                         code_col = "iso3c", 
                         year_col = "date", 
                         startyear_data = 1975, 
                         endyear_data = 2020, 
                         eval_from = 2000, 
                         eval_to = 2024, 
                         speed = TRUE, 
                         percentiles = TRUE, 
                         future = FALSE, 
                         target_year = 2030, 
                         sequence_pctl = seq(20,80,20), 
                         sequence_speed = c(0.25, 0.5, 1, 2, 4), 
                         best = "high", 
                         support = 1, 
                         granularity = 0.1)
