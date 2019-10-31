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
Tb <- 10
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
ndays <- nrow(weather)
# Thermal time age of the crop on day t [deg.C/day]
TT <- rep(NA, ndays)
# Leaf area index (area of leaves per unit ground area) [m3/m3]
LAI <- rep(NA, ndays)
# Biomass of crop per square meter of ground area [g/m2]
B <- rep(NA, ndays)

# Provide initial values for each output ----
TT[1] <- 0
LAI[1] <- 0.1
B[1] <- 1

# Run for loop ----


for (day in 1:(ndays - 1)) { # looping through day 1 to the second-to-last day
  # Calculate rates of change
  dTT <- max((weather$Tmin[day] + weather$Tmax[day])/2 - Tb, 0)
  if(TT[day] <= TTM) {dB <- RUE * (1-exp(-K * LAI[day])) * weather$I[day]}
  else {dB <- 0}
  if (TT[day] <= TTL) {dLAI <- a * dTT * LAI[day] * max(LAIm - LAI[day], 0)}
  else {dLAI <- 0}
  
  # Update output variables
  TT[day + 1] <- TT[day] + dTT
  LAI[day + 1] <- LAI[day] + dLAI
  B[day + 1] <- B[day] + dB
}

# Summarize model data
outputs <- data.frame(day = weather$day, TT, LAI, B)

# Plot predicted maize biomass over time
ggplot(outputs, aes(x = day, y = B)) + 
  geom_line() +
  geom_point(alpha = 0.2) +
  theme_bw()
