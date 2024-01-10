# Project file paths
VIOLATIONS_FILE = "./out/violations.csv"

# Frequencies
daily.freq   <- 365.25         # 0.25 to cope with the leap-year
weekly.freq  <- 52             # Mean number of weak per year
monthly.freq <- 12

# Names

names.area   <- c("North", "Center", "South")
names.center <- c('Central'     , 'West'        , 'SouthWest', 'South')
names.north  <- c('FarNorth'    , 'NorthWest'   , 'North'    )
names.south  <- c('FarSouthWest', 'FarSouthEast')
names.regions <- list(names.north, names.center, names.south)

# Colors
region.colors <- c(
  "#bc80b8", # Center; 
  "#f7a9a0", # Far North;
  "#facee1", # Far Southeast;
  "#d9bc73", # Far Southwest;
  "#b4d56b", # North Side;
  "#bdbada", # Northeast;
  "#d9d9c0", # South;
  "#d7eff7", # Southwest;
  "#fbb462"  # West
)    

area.colors <- c(
  "#2980B9", # Center
  "#58D68D", # North
  "#EB984E"  # South
)