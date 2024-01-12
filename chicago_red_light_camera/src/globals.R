# File: globals.R
# Author: Sebastiano Quintavalle
# Date: 2024-01-26
# Description: This file contains global variables for project configuration and data.

# File paths (local path referred to the project root)
violation_file <- "./out/violations.csv"

# Frequencies
daily.freq   <- 365.25  # Daily   frequency (considering leap years)
weekly.freq  <- 52      # Weekly  frequency
monthly.freq <- 12      # Monthly frequency

# Names of the three Chicago areas
names.area    <- c("North", "Center", "South")

# Names of the nine Chicago regions divided per area
names.regions <- list(
  North  = c('FarNorth', 'NorthWest', 'North'),
  Center = c('Central', 'West', 'SouthWest', 'South'),
  South  = c('FarSouthWest', 'FarSouthEast')
)

# Area colors
area.colors <- c(
  "#58D68D", # North
  "#2980B9", # Center
  "#EB984E"  # South
)

# Region colors
region.colors <- c(
  "#f7a9a0", # FarNorth
  "#bdbada", # NorthWest
  "#b4d56b", # North
  "#bc80b8", # Center
  "#fbb462", # West
  "#d7eff7", # SouthWest
  "#d9d9c0", # South
  "#d9bc73", # Far SouthWest
  "#facee1"  # Far SouthEast
)    
