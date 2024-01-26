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

names.freq <- c("Daily", "Weekly")
names.ylab <- list(
  Daily="Daily violations", 
  Weekly="Avg daily violations per week"#),
  #monthly="Average daily violations (per month)")
)

# Names of the three Chicago areas
names.area    <- c("North", "Center", "South")

# Names of the nine Chicago regions divided per area
names.regions <- list(
  North  = c('FarNorth', 'NorthWest', 'North'),
  Center = c('Central', 'West', 'SouthWest', 'South'),
  South  = c('FarSouthWest', 'FarSouthEast')
)

# Name of months and weekdays for weekly analysis
names.months <- c(
  "January", "February", "March",     "April",    "May",       "June",
  "July",    "August",   "September", "October",  "November",  "December"
)
names.weekdays <- c(
  "Monday",   "Tuesday", "Wednesday",
  "Thursday", "Friday",  "Saturday",  "Sunday"
)


# Area colors
area.colors <- list(
  North  = "#58D68D",
  Center = "#EB984E",
  South  = "#2980B9" 
)

# Region colors
region.colors <- list(
  FarNorth     = "#f7a9a0",
  NorthWest    = "#bdbada",
  North        = "#b4d56b",
  Central      = "#bc80b8",
  West         = "#fbb462",
  SouthWest    = "#d7eff7",
  South        = "#f9ef6e",
  FarSouthWest = "#d9bc73",
  FarSouthEast = "#facee1"
)

