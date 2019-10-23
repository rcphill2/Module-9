########################################################################################
# Summary: Predicting maize yields using a simple model
# Date: October 23, 2019
########################################################################################

# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)

# Read & inspect the dataset ----
weather <- read_csv("data/weather.csv")

# Define parameters (constant values) ----
# Baseline temperature for growth [deg. C]
Tb <- 7
# Temperature sum for crop maturity [deg.C/day]
TTM <- 1200
# Temperature sum at the end of leaf area increase [deg.C/day]
TTL <- 700
# Extinction coefficient [--]
K <- 0.7
# Radiation use efficiency [g/MJ]
RUE <- 1.85
# The relative rate of LAI increase for small values of LAI [deg.C/day]
a <- 0.00243
# Maximum LAI [m3/m3]
LAIm <- 7

# Initiatlize output vectors ----

# Thermal time age of the crop on day t [deg.C/day]

# Leaf area index (area of leaves per unit ground area) [m3/m3]

# Biomass of crop per square meter of ground area [g/m2]


# Provide initial values for each output ----


# Run for loop ----
for ( in ) { 
  # Calculate rates of change

  # Update output variables
  
}

# Summarize model data
outputs <- data.frame(day = weather$day, TT, LAI, B)

# Plot predicted maize biomass over time
ggplot(outputs, aes(x = day, y = B)) + 
  geom_line() +
  geom_point(alpha = 0.2) +
  theme_bw()
