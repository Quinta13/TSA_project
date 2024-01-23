# File: time.R
# Author: Sebastiano Quintavalle
# Date: 2024-01-26
# Description: # File: time.R
# Author: Sebastiano Quintavalle
# Date: 2024-01-26
# Description: the file contains set of functions facilitating the conversion 
#              of daily violation datasets into various time series representations,
#              including daily, monthly, and weekly aggregations, along with utility
#              functions for handling time-related operations.

# --- Daily dataset to time series conversion ---

#' Convert Daily Dataframe to Daily Time Series
#'
#' This function takes a daily dataframe containing violation data and converts
#' it into a daily time series object.
#'
#' @param df Daily dataframe with columns "Date" and "Violations" representing
#'        the date and corresponding number of violations.
#'
#' @return Returns a daily time series object (\code{ts}) with the violations
#'         data, starting from the first date in the input dataframe.
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

#' Convert Daily Dataframe to Monthly Time Series
#'
#' This function takes a daily dataframe containing violation data and converts
#' it into a monthly time series object by aggregating the violations for each month.
#'
#' @param df Daily dataframe with columns "Date" and "Violations" representing
#'        the date and corresponding violations.
#'
#' @return Returns a monthly time series object (\code{ts}) with the aggregated
#'         violations data, starting from the first date in the input dataframe.
#'
#'
daily_df_to_weekly_ts <- function(df) {
  
  # Create month column to aggregate with
  df$Week = as.numeric(format(df$Date, "%U"))
  df$Year = as.numeric(format(df$Date, "%Y"))
  
  # Replacing the cases of 53 week
  df$Week[df$Week == 53] <- 52
  
  # Taking the mean number of violations in a day
  weekly.df <- aggregate(Violations ~ Week + Year, data=df, mean)
  
  # Extracting starting month
  start.date <- df$Date[1]
  start.year <- as.numeric(format(start.date, "%Y"))
  start.week <- as.numeric(format(start.date, "%U"))
  
  # Creating monthly time series
  weekly.ts <- ts(
    weekly.df$Violations, 
    start=c(start.year, start.week), 
    frequency=weekly.freq
  )
  
  return(weekly.ts)
}


#' Convert Daily Dataframe to Monthly Time Series
#'
#' This function takes a daily dataframe containing violation data and converts
#' it into a monthly time series object by aggregating the violations for each month.
#'
#' @param df Daily dataframe with columns "Date" and "Violations" representing
#'        the date and corresponding violations.
#'
#' @return Returns a monthly time series object (\code{ts}) with the aggregated
#'         violations data, starting from the first date in the input dataframe.
#'
daily_df_to_monthly_ts <- function(df) {
  
  # Create month column to aggregate with
  df$Month = format(df$Date, "%Y-%m")
  monthly.df <- aggregate(Violations ~ Month, data=df, mean)
  
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

#' Convert Daily Dataframe to Weekly Time Series with Weekday/Weekend breakdown
#'
#' This function takes a daily dataframe containing violation data and converts
#' it into two weekly time series objects: one for weekdays and one for weekends.
#' 
#' @param df Daily dataframe with columns "Date" and "Violations" representing
#'        the date and corresponding violations.
#'
#' @return Returns a list with two weekly time series objects (\code{ts}):
#'  \itemize{
#'    \item \code{weekday}: Time series for violations on weekdays.
#'    \item \code{weekend}: Time series for violations on weekends.
#'  }
#
daily_df_to_weekly_ts_weekday_weekend <- function(df) {
  
  # Generating Weekday name
  df$WeekdayName = weekdays(df$Date)
  df$WeekdayName = as.factor(df$WeekdayName)
  
  # Generating Year and Week number in the year
  df$Year       <- as.numeric(format(df$Date, "%Y"))
  df$WeekNumber <- as.numeric(format(df$Date, "%U"))
  
  # Generating Weekday or Weekend class
  names.weekday <- names.weekdays[1:5]
  names.weekend <- names.weekdays[6:7]
  
  df$Weekday <- 
    ifelse(df$WeekdayName %in% names.weekday, "Weekday",
    ifelse(df$WeekdayName %in% names.weekend, "Weekend", NA))
  df$Weekday <- as.factor(df$Weekday)
  
  # Dropping first and last week that doesn't cover the entire year
  df$WeekNumber[df$WeekNumber ==  0] <- 1
  df$WeekNumber[df$WeekNumber == 53] <- 52
  
  # Aggregating data by Weekday, WeekNumber, and Year
  weekly.df <- aggregate(
    Violations ~ Weekday + WeekNumber + Year, 
    data = df, mean
  )
  
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
      frequency=weekly.freq-1
    ),
    # Weekend
    weekend = ts(
      weekly.df.weekend$Violations, 
      start=c(start.year, start.week), 
      frequency=weekly.freq-1
    )
  )
  
  return(weekly.ts)
  
}

# --- Time utils ---

#' Get Dates with Violations Over/Under a Threshold
#'
#' This function takes a time series object and a threshold value and returns
#' the dates where the violations are either over or under the specified threshold.
#'
#' @param ts Time series object (\code{ts}) representing the violations data.
#' @param threshold Numeric threshold value to compare with the violations.
#' @param upper Logical, indicating whether to find observations above (\code{TRUE}) or
#'        below (\code{FALSE}) the threshold. Default is \code{TRUE}.
#'
#' @return Returns a vector of dates corresponding to the observations over or under
#'         the specified threshold.
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


#' Convert Float Times to Date Format
#'
#' This function takes a vector of float times (as used in time series) and converts
#' them into a Date format, assuming the float times represent the day of the year.
#'
#' @param date_float Numeric vector representing float times.
#'
#' @return Returns a vector of Date objects corresponding to the input float times.
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

date_to_float <- function(date) {
  
  year <- as.numeric(format(date, "%Y"))
  day  <- as.numeric(format(date, "%j"))
  
  # Calculate the floating-point representation
  date_float <- year + (day - 1) / daily.freq
  
  return(date_float)
}

outliers_diagnostic <- function(ts, colors, main, ylab) {

  plot(
    ts,
    main=main,
    col=colors$plot,
    ylab=ylab
  )
  grid()
  
  # Getting outliers index and times 
  outliers <- tsoutliers(ts)
  idx      <- outliers$index
  times    <- time(ts)[idx]
  
  # Getting old and new observations
  old  <- ts[idx]
  new_ <- outliers$replacements
  
  points(times, old,  pch=18, col=colors$old, cex=1.5)
  points(times, new_, pch=18, col=colors$new, cex=1.5)
  
  legend(
    "topleft", 
    legend = c("Observed outlier", "Suggested replacement"), 
    pch = c(18, 18), 
    col = c(colors$old, colors$new)
  )
  
  for(i in 1:length(times)) {
    print(paste(
      float_to_date(times[i]), 
      "- old: ", old[i], 
      ", new: ", new_[i])
    )
  }
  
  return(outliers)

}

replace_outliers <- function(df, ts, outliers) {
  
  # Cast to integer violations
  outliers$replacements = round(outliers$replacements)
  
  # Retrieve outliers dates
  outliers$dates = float_to_date(time(ts)[outliers$index])
  
  # Drop outliers from the dataset
  df <- subset(
    df, !(Date %in% outliers$dates)
  )
  
  # Create replacement rows
  replacement <- data.frame(
    Area = as.factor(rep("Center", length(outliers$replacements))),
    Date = outliers$dates,
    Violations = outliers$replacements
  )
  
  # Merge the dataframe with the replacement rows
  df <- rbind(
    df, 
    replacement
  )
  
  # Sort the dataset by the date column
  df <- df[order(df$Date), ]
  
  return(df)
}
