####################
### INTRODUCTION ###
####################
# This code reproduces the figure from the blog post on the World Bank Data Blog titled "Setting Realistic and Ambitious Development Targets"
# For questions, please email dmahler@worldbank.org and rtatulli@worldbank.org.

###########################################
### INSTALL AND LOAD NECESSARY PACKAGES ###
###########################################
# List of required non-trackr packages
packages <- c("devtools","dplyr","ggplot2","wbstats")

# Check and install missing packages
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) {
  install.packages(new_packages)
}

# Install trackr
# devtools::install_github("RossanaTat/trackr")

# Load packages
library(trackr)
library(dplyr)
library(ggplot2)

# Set a seed such that the results replicate. 
set.seed(1)

#######################################################
### LOAD DATA FROM THE WORLD DEVELOPMENT INDICATORS ###
#######################################################
# This was run 2026.04.14. Data in WDI might be revised in the future and results might differ if you run this code at a later date.
data <- wbstats::wb_data(indicator = c("SH.DYN.MORT"), lang = "en",country="countries_only")

##################################################
### CALCULATE TARGETS USING THE TRACKR PACKAGE ###
##################################################
# First run trackr to find India's speed of progress
results <- track_progress(
    data=data,
    indicator="SH.DYN.MORT",
    code_col="iso3c",
    year_col="date",
    startyear_data=1960,
    endyear_data=2023,
    eval_from=2013,
    eval_to=2023,
    future=TRUE,
    target_year=2050,
    speed=TRUE,
    percentiles=FALSE,
    sequence_speed = 1,
    best="low"
)

# Store India's speed of progress
speed_india <- results$scores$speed |> filter(code=="IND") |> pull(score)

# Now run trackr with speed of 1, Nepal's speed (automatically calculated), and India's speed of progress
results <- track_progress(
    data=data,
    indicator="SH.DYN.MORT",
    code_col="iso3c",
    year_col="date",
    startyear_data=1960,
    endyear_data=2023,
    eval_from=2013,
    eval_to=2023,
    future=TRUE,
    target_year=2050,
    speed=TRUE,
    percentiles=FALSE,
    sequence_speed = c(1,speed_india),
    best="low"
)

# Future values for Nepal
path_fut_NPL <- results$path_future$speed |> filter(code=="NPL") 

# Historical values for Nepal
path_his_NPL <- data |> filter(iso3c=="NPL" & between(date,2013,2023)) |> select(date,SH.DYN.MORT) |> rename("year"="date","y_his"="SH.DYN.MORT") 

####################
### PLOT RESULTS ###
####################

# 2050 values for labels
labels_2050 <- path_fut_NPL |> 
  filter(year == 2050) |>
  mutate(label = round(y_fut, 1))

# Plot line chart
ggplot() + 
  geom_line(path_fut_NPL,mapping=aes(x=year,y=y_fut,color=as.factor(speed)),linewidth=1.25) + 
  geom_line(data=path_his_NPL,mapping=aes(x=year,y=y_his),color="black",linewidth=1.25) +
  geom_text(data=labels_2050, mapping=aes(x=year, y=y_fut, label=label, color=as.factor(speed)), 
            hjust=-0.2, size=3.5, show.legend=FALSE) +
  labs(x="",y="Under-5 mortality rate\n(per 1,000 live births)",color="Scenario") +
  scale_color_manual(
    values = c("#56B4E9","#E69F00","darkgreen"),
    labels = c("Typical speed","India's past speed (2013-2023)","Past speed (2013-2023)")) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(plot.margin = margin(5.5, 20, 5.5, 5.5)) +
  ggtitle("Target setting for Nepal's under-5 mortality rate")
