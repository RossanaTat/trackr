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


# Install from GitHub (requires remotes package)
remotes::install_github("RossanaTat/trackr")


## Example Usage
library(trackr)



