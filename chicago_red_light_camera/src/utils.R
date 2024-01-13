# File: utils.R
# Author: Sebastiano Quintavalle
# Date: 2024-01-26
# Description: Utility functions for time series manipulation and visualization.

# --- Daily dataset to time series conversion ---

#' Convert a daily data frame to a time series object.
#'
#' This function takes a data frame with a "Date" column and a "Violations" column
#' and converts it into a time series object. 
#' The time series is created with a specified starting date and frequency.
#'
#' @param df A data frame with "Date" and "Violations" columns.
#' @return A time series object.
#'
daily_df_to_daily_ts <- function(df) {
  
  # Extracting starting date
  start.date <- df$Date[1]
  start.year <- as.numeric(format(start.date, "%Y"))
  start.day  <- as.numeric(format(start.date, "%j"))
  
  # Creating time series object
  daily.ts <- ts(
    df$Violations, 
    start=c(start.year, start.day), 
    frequency=daily.freq
  )
  
  return(daily.ts)
}


#' Convert a daily data frame to a monthly time series object.
#'
#' This function takes a data frame with a "Date" column and a "Violations" column,
#' creates a monthly aggregation, and converts it into a time series object.
#' The time series is created with a specified starting date and frequency.
#'
#' @param df A data frame with "Date" and "Violations" columns.
#' @return A monthly time series object.
#'
daily_df_to_monthly_ts <- function(df) {
  
  # Create month column to aggregate with
  df$Month = format(df$Date, "%Y-%m")
  monthly.df <- aggregate(Violations ~ Month, data=df, sum)
  
  # Extracting starting month
  start.date  <- df$Date[1]
  start.year  <- as.numeric(format(start.date, "%Y"))
  start.month <- as.numeric(format(start.date, "%m"))
  
  # Creating monthly time series
  monthly.ts <- ts(
    monthly.df$Violations, 
    start=c(start.year, start.month), 
    frequency=monthly.freq
  )
  
  return(monthly.ts)
}


#' Convert a daily data frame to separate weekly time series for weekdays and weekends.
#'
#' This function takes a data frame with a "Date" column and a "Violations" column, 
#' generates separate time series for weekdays and weekends,
#' and returns a list  containing both time series objects.
#' The time series are created with a specified starting date and frequency.
#'
#' @param df A data frame with "Date" and "Violations" columns.
#' @return A list containing two time series objects for weekdays and weekends.
#'
daily_df_to_weekly_ts_weekday_weekend <- function(df) {
  
  # Generating Weekday name
  df$WeekdayName = weekdays(df$Date)
  df$WeekdayName = as.factor(df$WeekdayName)
  
  # Generating Year and Week number in the year
  df$Year       <- as.numeric(format(df$Date, "%Y"))
  df$WeekNumber <- as.numeric(format(df$Date, "%U"))
  
  # Generating Weekday or Weekend classification
  names.weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  names.weekend <- c("Saturday", "Sunday")
  
  df$Weekday <- ifelse(df$WeekdayName %in% names.weekday, "Weekday",
                ifelse(df$WeekdayName %in% names.weekend, "Weekend", NA))
  
  # Dropping first and last week that doesn't cover the all keep
  df <- subset(df, !(WeekNumber %in% c(0, 53)))
  
  # Aggregating data by Weekday, WeekNumber, and Year
  weekly.df <- aggregate(Violations ~ Weekday + WeekNumber + Year, data = df, sum)
  
  # Extracting starting week
  start.year <- df$Year[1]
  start.week <- df$WeekNumber[1]
  
  # Splitting weekday and weekend
  weekly.df.weekday <- subset(weekly.df, Weekday == "Weekday")
  weekly.df.weekend <- subset(weekly.df, Weekday == "Weekend")
  
  # Creating time series objects for weekday and weekend
  weekly.ts = list(
    # Weekday
    weekday = ts(
      weekly.df.weekday$Violations, 
      start=c(start.year, start.week), 
      frequency=weekly.freq
    ),
    # Weekend
    weekend = ts(
      weekly.df.weekend$Violations, 
      start=c(start.year, start.week), 
      frequency=weekly.freq
    )
  )
  
  return(weekly.ts)
  
}


# --- Time utils ---

#' Get observation dates exceeding or falling below a threshold in a time series.
#'
#' This function takes a time series object, a threshold value, and an optional 
#' parameter specifying whether to find observations above or below the threshold. 
#' It returns the dates corresponding to the observations exceeding or falling below 
#' the specified threshold.
#'
#' @param ts A time series object.
#' @param threshold The threshold value for observations.
#' @param upper Logical. If TRUE, find observations above the threshold; 
#'              if FALSE, find observations below the threshold.
#' @return A vector of dates corresponding to observations exceeding or falling 
#'         below the threshold.
#'
get_observation_over_threshold <- function(ts, threshold, upper=TRUE) {
  
  # Find indices of observations above or below the threshold
  if (upper) {
    idx <- which(ts > threshold)
  } else {
    idx <- which(ts < threshold)
  }
  
  # Extract times corresponding to the identified indices
  times <- time(ts)[idx]
  
  # Convert float times to date format
  dates <- float_to_date(date_float = times)
  
  return(dates)
}


#' Convert a floating-point representation of a date to a Date object.
#'
#' This function takes a floating-point representation of a date, where the integer
#' part represents the year and the decimal part represents the day of the year, 
#' and converts it into a Date object.
#'
#' @param date_float A floating-point representation of a date.
#' @return A Date object.
#'
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


# --- Plot ---

#' Plot the decomposition of a time series.
#'
#' This function takes a time series object, decomposes it into its observed, trend, 
#' seasonal, and random components, and plots the resulting components.
#'
#' @param ts A time series object.
#' @param main Main title for the plot.
#' @return NULL
#'
plot_ma_decomposition <- function(ts, main) {
  
  # Decompose the time series
  decomposition <- decompose(ts)
  
  # Combine components for plotting
  trends <- cbind(
    observed = decomposition$x,
    trend    = decomposition$trend, 
    seasonal = decomposition$seasonal, 
    random   = decomposition$random
  )
  
  # Plot the decomposition
  plot(
    trends, 
    main = main
  )
  # Return NULL as the result is a plot
  return(NULL)
}

plot_stl_decomposition <- function(ts, main) {
  
  # Decompose the time series
  decomposition <- stl(ts, s.window="periodic")
  decomposition.ts <- decomposition$time.series
  
  # Combine components for plotting
  trends <- cbind(
    trend    = decomposition.ts[, "trend"    ], 
    seasonal = decomposition.ts[, "seasonal" ], 
    random   = decomposition.ts[, "remainder"]
  )
  
  # Plot the decomposition
  plot(
    trends, 
    main=main
  )
  # Return NULL as the result is a plot
  return(NULL)
}


#' Plot multiple time series on the same graph.
#'
#' This function takes a list of time series objects, a list of colors, and a list of legends.
#' It plots the time series on the same graph with specified colors and legends.
#'
#' @param time_series_list A list of time series objects.
#' @param colors A list of colors for each time series.
#' @param legends A list of legend names for each time series.
#' @param main Main title for the plot.
#' @param ylab Label for the y-axis.
#' @return NULL
#'
plot_multiple_time_series <- function(ts_list, colors, legends, main, ylab) {
 
  # Get the number of time series
  num_series <- length(ts_list)
  
  first_name = names(ts_list)[1]
  
  # Plot the first time series
  plot(
    ts_list[[first_name]],
    ylim=range(unlist(ts_list)),
    type="l", col=colors[[first_name]],
    main=main, xlab="Year", ylab=ylab
  )
  
  # Add the remaining time series to the plot
  for (i in 2:num_series) {
    i_name = names(ts_list)[i]
    lines(ts_list[[i_name]], type="l", col=colors[[i_name]])
  }
  
  # Add legend
  legend("topright",
         legend=legends,
         lty=1, col=unlist(colors))
  
  # Return NULL as the result is a plot, and it's typically not assigned to a variable.
  return(NULL)
}
