# Clear preexisting environment
rm(list=ls())

# Importing files
source("./src/smoothing.R")
source("./src/globals.R")
source("./src/utils.R")

# 1. Reading File
violations.regions.df <- read.csv(file=VIOLATIONS_FILE)

# Casting Regions to a factor 
violations.regions.df$Region = as.factor(violations.regions.df$Region)

# Converting Date column to DateType
violations.regions.df$Date <- as.Date(
  x      = as.character(violations.regions.df$Date),
  format = "%m/%d/%Y"
)

head(violations.regions.df)
summary(violations.regions.df)

# 2. Aggregate Areas

levels(violations.regions.df$Region)

# Assigning each Region to the specific Area
violations.regions.df$Area <- ifelse(violations.regions.df$Region %in% names.north,   'North',
                      ifelse(violations.regions.df$Region %in% names.center, 'Center',
                      ifelse(violations.regions.df$Region %in% names.south,   'South', NA)))
violations.regions.df$Area = as.factor(violations.regions.df$Area)

levels(violations.regions.df$Area)

# Aggregating violations by areas
violations.areas.df = aggregate(
  Violations ~ Area + Date,      # sum Violations number of observations with same Area and Date
  data = violations.regions.df,
  sum
)

# 3. Split for Area and Region
region.df <- split(
  violations.regions.df,
  violations.regions.df$Region
)

area.df <- split(
  violations.areas.df,
  violations.areas.df$Area
)


names(region.df)
names(area.df)

# Set custom order
names(area.df) <- names.area
names(area.df)

# 4. To daily time series

region.ts.daily <- lapply(
  region.df, 
  daily_df_to_daily_ts
)

area.ts.daily <- lapply(
  area.df, 
  daily_df_to_daily_ts
)

names(area.ts.daily) <- names.area

# To monthly

region.ds.monthly <- lapply(
  region.df, 
  daily_df_to_monthly_ts
)


area.ts.monthly <- lapply(
  area.df,
  daily_df_to_monthly_ts
)

names(area.ts.monthly) <- names.area

# 6. Daily Plot

{
par(mfrow = c(3, 3))
for (i in seq_along(names(region.df))) {

  region.name      <- names(region.ts.daily)[i]
  region.ts.daily_ <- region.ts.daily[[region.name]]
  region.color     <- region.colors[i]
  
  plot(
    region.ts.daily_,
    main=region.name,
    col=region.color,
    ylab="Daily violations"
  )
}
par(mfrow = c(1, 1))
}

{
par(mfrow = c(3, 1))
for (i in seq_along(names.area)) {

  area.name      <- names.area[i]
  area.ts.daily_ <- area.ts.daily[[area.name]]
  area.color     <- area.colors[i]
  
  plot(
    area.ts.daily_,
    main = area.name,
    col = area.color,
    ylab="Daily violations"
  )
}
par(mfrow = c(1, 1))
}

# Find indices where values exceed 1500
date.anomaly = get_observation_over_threshold(
  ts=area.ts.daily$Center,
  threshold = 1500
)

date.anomaly

# 1. Filtering to extract trend

p        <- c(         60,           140,          230)
p.colors <- c('lawngreen', 'deepskyblue', 'chocolate1')



{
par(mfrow = c(3, 1))
for (area_name in names(area.ts.daily)) {
  
  area.ts.daily_ <- area.ts.daily[[area_name]]
  
  simple_moving_average_varying_p(
    ts=area.ts.daily_,
    p=p,
    p.color=p.colors,
    main=paste(area_name, "- Simple Moving Average"),
    ylab="Daily violations"
  )
  
}
par(mfrow=c(1,1))}


for (i in seq_along(names(area.ts.daily))) {
  
  area         <- names.area[i]
  regions_area <- names.regions[[i]]
  
  par(mfrow = c(length(regions_area), 1), oma=c(0,0,3,0))
  for (region_name in regions_area) {

    region.ts.daily_ <- region.ts.daily[[region_name]]

    simple_moving_average_varying_p(
      ts=region.ts.daily_,
      p=p,
      p.color=p.colors,
      main=paste(region_name, "- Simple Moving Average"),
      ylab="Daily violations"
    )

  }
  mtext(area, line=0, side=3, outer=TRUE, cex=2)
  par(mfrow=c(1,1))
}

## Spencer

{
par(mfrow = c(3, 1))
for (area_name in names(area.ts.daily)) {
  
  area.ts.daily_ <- area.ts.daily[[area_name]]
  
  plot(
    area.ts.daily_,
    main=paste(area_name, "- Spencer filter"),
    ylab="Daily violations"
  )
  
  spencer.trend = spencer(ts=area.ts.daily_)
  
  lines(
    spencer.trend,
    lty='dashed', lwd=3,
    col='chocolate1'
  )
  
}
par(mfrow=c(1,1))
}

{
  par(mfrow = c(3, 1))
  for (area_name in names(area.ts.monthly)) {
    
    area.ts.monthly_ <- area.ts.monthly[[area_name]]
    
    plot(
      area.ts.monthly_,
      main=paste(area_name, "- Spencer filter"),
      ylab="Monthly violations"
    )
    
    spencer.trend = spencer(ts=area.ts.monthly_)
    
    lines(
      spencer.trend,
      lty='dashed', lwd=3,
      col='chocolate1'
    )
    
  }
  par(mfrow=c(1,1))
}

{
  par(mfrow = c(3, 1))
  for (area_name in names(area.ts.daily)) {
    
    area.ts.daily_ <- area.ts.daily[[area_name]]
    
    plot(
      area.ts.daily_,
      main=paste(area_name, "- Deseasonal filter"),
      ylab="Daily violations"
    )
    
    deseasonal.trend = deseasoning(
      ts=area.ts.daily_, 
      as.integer(daily.freq)
    )
    
    lines(
      deseasonal.trend,
      lty='dashed', lwd=3,
      col='chocolate1'
    )
    
  }
  par(mfrow=c(1,1))
}

{
  par(mfrow = c(3, 1))
  for (area_name in names(area.ts.daily)) {
    
    area.ts.monthly_ <- area.ts.monthly[[area_name]]
    
    plot(
      area.ts.monthly_,
      main=paste(area_name, "- Deseasonal filter"),
      ylab="Monthly violations"
    )
    
    deseasonal.trend <- deseasoning(
      ts=area.ts.monthly_, 
      freq=monthly.freq
    )
    
    lines(
      deseasonal.trend,
      lty='dashed', lwd=3,
      col='chocolate1'
    )
    
  }
  par(mfrow=c(1,1))
}


{
  par(mfrow = c(3, 1))
  for (area_name in names(area.ts.monthly)) {
    
    area.ts.monthly_ <- area.ts.monthly[[area_name]]
    
    monthplot(
      area.ts.monthly_,
      main=paste(area_name, " - Monthplot"),
      ylab="Monthly violations"
    )
    
  }
  par(mfrow=c(1,1))
}


# TSL Decomposition

{
  par(mfrow = c(3, 1))
  for (area_name in names(area.ts.daily)) {
    
    area.ts.daily_ <- area.ts.daily[[area_name]]
    
    plot(
      area.ts.daily_,
      main=paste(area_name, "- Trend from TSL decomposition"),
      ylab="Daily violations"
    )
    
    decomposition.trend = decompose(area.ts.daily_)$trend
    
    lines(
      decomposition.trend,
      lty='dashed', lwd=3,
      col='chocolate1'
    )
    
  }
  par(mfrow=c(1,1))
}

for (area_name in names(area.ts.daily)) {
  
  area.ts.daily_ <- area.ts.daily[[area_name]]
    
  decomposition <- decompose(area.ts.daily_)
  
  plot_decomposition(
    decomposition=decomposition,
    main=area_name
  )
}

for (i in seq_along(names(area.ts.daily))) {
  
  area_name <- names.area[i]
  area_regions <- names.regions[[i]]
  
  for (region_name in area_regions) {
    
    region.ts.daily_ <- region.ts.daily[[region_name]]
    
    decomposition <- decompose(region.ts.daily_)
    
    plot_decomposition(
      decomposition=decomposition,
      main=paste(region_name, "(", area_name, ")")
    )
  }
  
}

# Components comparison

for (i in seq_along(names(area.ts.daily))) {
  
  area_name <- names.area[i]
  area_regions <- names.regions[[i]]
  
  for (region_name in area_regions) {
    
    region.ts.daily_ <- region.ts.daily[[region_name]]
    
    decomposition <- decompose(region.ts.daily_)
    
    plot_decomposition(
      decomposition=decomposition,
      main=paste(region_name, "(", area_name, ")")
    )
  }
}

# Magnitude
plot_component_decompositions(
  ts=area.ts.daily, 
  component.name="x", 
  main=paste("Magnitude comparison"), 
  ylab="Daily violations"
)

# Trend
plot_component_decompositions(
  ts=area.ts.daily, 
  component.name="trend", 
  main=paste("Trend comparison"), 
  ylab="Daily violations"
)

# Seasonality
plot_component_decompositions(
  ts=area.ts.daily, 
  component.name="seasonal", 
  main=paste("Seasonality comparison"), 
  ylab="Daily observations"
)

# Error
plot_component_decompositions(
  ts=area.ts.daily, 
  component.name="random", 
  main=paste("Error comparison"), 
  ylab="Daily observations"
)

# Find worst predictions

worst_predictions <- get_observation_over_threshold(
  ts = decompose(area.ts.daily$Center)$random,
  threshold = -300,
  upper=FALSE
)

worst_predictions
# 2018-09-25 storm https://www.weather.gov/lot/25Sep2018_wind
# 2020-05-13 george floyd
# 2021-12-07 missing element

# 2. Dependency between weekdays

{
  par(mfrow = c(3, 1))
  for (area_name in names(area.ts.daily)) {
    
    area.ts.daily_ <- area.ts.daily[[area_name]]
    
    acf(
      area.ts.daily_,
      main=paste(area_name, " - ACF"),
      lag.max=365
    )
    
  }
  par(mfrow=c(1,1))
}

{ # Lagged 7
  par(mfrow = c(3, 1))
  for (area_name in names(area.ts.daily)) {
    
    area.ts.daily_ <- area.ts.daily[[area_name]]
    area.ts.daily_lagged7 <- diff(area.ts.daily_, 7)
    
    plot(
      area.ts.daily_lagged7,
      main=paste(area_name, " - Lagged 7"),
    )
    
  }
  par(mfrow=c(1,1))
}

{ # Lagged 7
  par(mfrow = c(3, 1))
  for (area_name in names(area.ts.daily)) {
    
    area.ts.daily_ <- area.ts.daily[[area_name]]
    area.ts.daily_lagged7 <- diff(area.ts.daily_, 7)
    
    acf(
      area.ts.daily_lagged7,
      main=paste(area_name, " - Lagged 7 - ACF"),
    )
    
  }
  par(mfrow=c(1,1))
}

{ # Lagged 7 - Regions
  par(mfrow = c(3, 3))
  for (region_name in names(region.ts.daily)) {
    
    region.ts.daily_ <- region.ts.daily[[region_name]]
    region.ts.daily_lagged7 <- diff(region.ts.daily_, 7)
    
    acf(
      region.ts.daily_lagged7,
      main=paste(region_name, " - Lagged 7 - ACF"),
    )
    
  }
  par(mfrow=c(1,1))
}

acf(
  diff(area.ts.daily$Center, 7)
)


