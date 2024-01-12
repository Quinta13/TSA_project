# Clear preexisting environment
Sys.setlocale("LC_TIME", "en_US.UTF-8")
rm(list=ls())

# Importing packages
library(forecast)

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

# 4. To daily time series

region.ts.daily <- lapply(
  region.df, 
  daily_df_to_daily_ts
)

area.ts.daily <- lapply(
  area.df, 
  daily_df_to_daily_ts
)

# To monthly

region.ds.monthly <- lapply(
  region.df, 
  daily_df_to_monthly_ts
)

area.ts.monthly <- lapply(
  area.df,
  daily_df_to_monthly_ts
)

# 6. Daily Plot

{
par(mfrow = c(3, 3))
for (i in 1:length(region.ts.daily)) {

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
par(mfrow = c(length(area.ts.daily), 1))
for (area_name in names.area) {

  area.ts.daily_ <- area.ts.daily[[area_name]]
  
  plot(
    area.ts.daily_,
    main = area_name,
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

plot_multiple_time_series(
  time_series_list=area.ts.daily,
  colors=area.colors,
  legends=names(area.ts.daily),
  main="Violation in Chicago",
  ylab="Daily violations"
)

# 1. Filtering to extract trend
p        <- c(         60,           140,          230)
p.colors <- c('lawngreen', 'deepskyblue', 'chocolate1')

{
par(mfrow = c(length(area.ts.daily), 1))
for (area_name in names.area) {
  
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


for (i in 1:length(area.ts.daily)) {
  
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
par(mfrow = c(length(area.ts.daily), 1))
for (area_name in names.area) {
  
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
    col='orangered'
  )
  
}
par(mfrow=c(1,1))
}

{
  par(mfrow = c(length(area.ts.daily), 1))
  for(area_name in names.area) {
    
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
      col='orangered'
    )
    
  }
  par(mfrow=c(1,1))
}

{
  par(mfrow = c(length(area.ts.daily), 1))
  for(area_name in names.area) {
    
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
      col='orangered'
    )
    
  }
  par(mfrow=c(1,1))
}


{
  par(mfrow = c(length(area.ts.daily), 1))
  for (area_name in names.area) {
    
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
  par(mfrow = c(length(area.ts.daily), 1))
  for(area_name in names.area) {
    
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
      col='orangered'
    )
    
  }
  par(mfrow=c(1,1))
}

for(area_name in names.area) {
  
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
      main=paste(region_name, " (", area_name, ")", sep="")
    )
  }
  
}

# Components comparison

# Magnitude
plot_component_decompositions(
  ts_list = list(area.ts.daily$North, area.ts.daily$Center, area.ts.daily$South), 
  names=names.area,
  component.name="x", 
  main=paste("Magnitude comparison"), 
  ylab="Daily violations"
)

# Trend
plot_component_decompositions(
  ts_list = list(area.ts.daily$North, area.ts.daily$Center, area.ts.daily$South), 
  names=names.area, 
  component.name="trend", 
  main=paste("Trend comparison"), 
  ylab="Daily violations"
)

# Seasonality
plot_component_decompositions(
  ts_list = list(area.ts.daily$North, area.ts.daily$Center, area.ts.daily$South), 
  names=names.area,
  component.name="seasonal", 
  main=paste("Seasonality comparison"), 
  ylab="Daily observations"
)

# Error
plot_component_decompositions(
  ts_list = list(area.ts.daily$North, area.ts.daily$Center, area.ts.daily$South), 
  names=names.area,
  component.name="random", 
  main=paste("Error comparison"), 
  ylab="Daily observations"
)

# Find worst predictions

worst_predictions <- get_observation_over_threshold(
  ts = decompose(area.ts.daily$North)$random,
  threshold = -300,
  upper=FALSE
)

worst_predictions
# 2018-09-25 storm https://www.weather.gov/lot/25Sep2018_wind
# 2020-05-13 george floyd
# 2021-12-07 missing element

# 2. Dependency between weekdays

# Original ARIMA, ACF and PACF

model <- auto.arima(area.ts.daily$Center)
print(paste("Model", arimaorder(model)['p']))

for(area_name in names.area) {
    
    area.ts.daily_ <- area.ts.daily[[area_name]]
    
    # ARIMA
    # order <- arimaorder(auto.arima(area.ts.daily_))
    
    tsdisplay(
      area.ts.daily$Center,
      main= paste(expression(Y[t]) ))
    )
    
    
  }
par(mfrow=c(1,1))




for(area_name in names.area) {
  
  area.ts.daily_ <- area.ts.daily[[area_name]]
  area.ts.daily_lagged7 <- diff(area.ts.daily_, 7)
  
  print(auto.arima(area.ts.daily_lagged7))
  
}

{ # Lagged 7
  par(mfrow = c(length(area.ts.daily), 1))
  for(area_name in names.area) {
    
    area.ts.daily_ <- area.ts.daily[[area_name]]
    area.ts.daily_lagged7 <- diff(area.ts.daily_, 7)
    
    acf(
      as.numeric(area.ts.daily_lagged7),
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


region.ts.weekly <- lapply(
  region.df, 
  daily_df_to_weekly_ts_weekday_weekend
)

area.ts.weekly <- lapply(
  area.df, 
  daily_df_to_weekly_ts_weekday_weekend
)



{ # Plots
  par(mfrow = c(2, length(area.ts.weekly)))
  
  # Weekdays
  for(area_name in names.area) {
    
    area.ts.weekly_ <- area.ts.weekly[[area_name]]
    
    plot(
      area.ts.weekly_$weekday,
      main=paste(area_name, "- Weekday"),
      ylab="Weekly violations"
    )
  }
  
  # Weekends
  for(area_name in names.area) {
    
    area.ts.weekly_ <- area.ts.weekly[[area_name]]
    
    plot(
      area.ts.weekly_$weekend,
      main=paste(area_name, "- Weekend"),
      ylab="Weekly violations"
    )
  }
  
  par(mfrow=c(1,1))
}


{ # Plots
  par(mfrow = c(2, length(region.ts.weekly)))
  
  # Weekdays
  for(region_name in unlist(names.regions)) {
    
    region.ts.weekly_ <- region.ts.weekly[[region_name]]
    
    plot(
      region.ts.weekly_$weekday,
      main=paste(region_name, "- Weekday"),
      ylab="Weekly violations"
    )
  }
  
  # Weekends
  for(region_name in unlist(names.regions)) {
    
    region.ts.weekly_ <- region.ts.weekly[[region_name]]
    
    plot(
      region.ts.weekly_$weekend,
      main=paste(region_name, "- Weekend"),
      ylab="Weekly violations"
    )
  }
  
  par(mfrow=c(1,1))
}

## CCF
{ # Lagged 7
  par(mfrow = c(length(area.ts.weekly), 1))
  for(area_name in names.area) {
    
    area.ts.weekly_ <- area.ts.weekly[[area_name]]
    
    ccf(
      x=as.numeric(area.ts.weekly_$weekday),
      y=as.numeric(area.ts.weekly_$weekday),
      lag.max = 104,
      main=paste(area_name ,"- Cross Correlogram"),
      ylab="CCF"
    )
    
  }
  par(mfrow=c(1,1))
}

# # ARIMA
# { # Lagged 7
#   par(mfrow = c(length(area.ts.weekly), 1))
#   for(area_name in names.area) {
#     
#     area.ts.weekly_ <- area.ts.weekly[[area_name]]
#     
#     print(area_name)
#     print("Weekday: ")
#     print(auto.arima(area.ts.weekly_$weekday)$coef)
#     print("Weekend: ")
#     print(auto.arima(area.ts.weekly_$weekday)$coef)
#     
#   }
#   par(mfrow=c(1,1))
# }

weekday.violations <- as.numeric(area.ts.weekly$Center$weekday)
weekend.violations  <- as.numeric(area.ts.weekly$Center$weekend)

## Relation
plot(
  x=weekday.violations,
  y=weekend.violations,
  pch=16, col="steelblue",
  main="Weekdays VS Weekend violations",
  xlab="Weekly violations on weekdays",
  ylab="Weekly violations on weekdays",
)
grid()

# Linear regression
fit <- lm(weekend.violations ~ weekday.violations)
abline(fit,  col = "orangered", lwd = 3, lty="dashed")
checkresiduals(fit)
AIC(fit)

# Using ARIMA
fit1 <- auto.arima(area.ts.weekly$Center$weekend, xreg=area.ts.weekly$Center$weekday)
summary(fit1)

checkresiduals(fit1)
AIC(fit1)

# DETERMINISTIC  TREND

# First model, use index as predictor
fit_deterministic <- auto.arima(
  area.ts.weekly$Center$weekend,
  xreg=1:length(area.ts.weekly$Center$weekend) # also provide predictor
)
summary(fit_deterministic) # Error Term AR(2)
checkresiduals(fit_deterministic)
AIC(fit_deterministic)

fit_stochastic <- auto.arima(
  area.ts.weekly$Center$weekend
)
checkresiduals(fit_deterministic)
AIC(fit_deterministic)

# Best according to AIC
fit_deterministic$aicc
fit_stochastic$aicc

# We prefer the first case
fc_deterministic <- forecast(
  fit_deterministic, 
  h = 10,
  xreg = (max(xreg)+1:10) # also provide further value for predictors
)
fc_stochastic <- forecast(
  fit_stochastic,
  h = 10
)

# Same prediction, but the deterministic has narrow variance
par(mfrow=c(2,1))
plot(fc_deterministic,fcol=2,main="Forecasts from deterministic trend ",ylim=c(0,10))
plot(fc_stochastic,fcol=4, main="Forecasts from stochastic trend ",ylim=c(0,10))





