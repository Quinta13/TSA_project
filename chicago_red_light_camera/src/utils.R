# --- Converting daily data frame to time series ---

daily_df_to_daily_ts <- function(df) {
  
  # Extracting starting date
  start.date <- df$Date[1]
  start.year <- as.numeric(format(start.date, "%Y"))
  start.day  <- as.numeric(format(start.date, "%j"))
  
  # Creating time series
  daily.ts <- ts(
    df$Violations, 
    start = c(start.year, start.day), 
    frequency = daily.freq
  )
  
  return(daily.ts)

}

daily_df_to_monthly_ts <- function(df) {
  
  # Create month column to aggregate with
  df$Month = format(df$Date, "%Y-%m")
  monthly.df <- aggregate(Violations ~ Month, data=df, sum)
  
  # Extracting starting month
  start.date  <- df$Date[1]
  start.year  <- as.numeric(format(start.date, "%Y"))
  start.month <- as.numeric(format(start.date, "%m"))
  
  monthly.ts <- ts(
    monthly.df$Violations, 
    start = c(start.year, start.month), 
    frequency = monthly.freq
  )
  
  return(monthly.ts)

}

daily_df_to_weekly_ts <- function(df) {
  
  # Create month column to aggregate with
  df$Week <- format(df$Date, "%Y-%U")
  weekly.df <- aggregate(Violations ~ Week, data=df, sum)
  
  # Extracting starting month
  start.date <- df$Date[1]
  start.year <- as.numeric(format(start.date, "%Y"))
  start.week <- as.numeric(format(start.date, "%U"))
  
  weekly.ts <- ts(
    weekly.df$Violations, 
    start = c(start.year, start.week), 
    frequency = weekly.freq
  )
  
  return(weekly.ts)
  
}

float_to_date <- function(date_float) {
  
  # Extract the year (integer part) and the day (decimal part)
  year        <- floor(date_float)
  day_decimal <- date_float - year
  
  # Calculate the day of the year
  day_of_year <- floor(day_decimal * daily.freq) + 1
  
  # Create a Date object
  date_object <- as.Date(paste(year, "-", day_of_year, sep = ""), format="%Y-%j")
  
  return(date_object)
}

plot_decomposition <- function(decomposition, main) {
  
  trends = cbind(
    observed = decomposition$x,
    trend    = decomposition$trend, 
    seasonal = decomposition$seasonal, 
    random   = decomposition$random
  )
  
  plot(
    trends, 
    main=main
  )
}

get_observation_over_threshold <- function(ts, threshold, upper=TRUE) {
  
  if (upper) {
    idx <- which(ts > threshold)
  } else {
    idx <- which(ts < threshold)
  }
  
  times <- time(ts)[idx]
  
  dates <- float_to_date(date_float=times)
  
  return(dates)
  
}
  