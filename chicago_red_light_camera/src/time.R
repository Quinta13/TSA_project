# File: time.R
# Author: Sebastiano Quintavalle
# Date: 2024-01-26
# Description: # File: time.R
# Author: Sebastiano Quintavalle
# Date: 2024-01-26
# Description:  This script contains various utilities designed to perform tasks
#               such as outlier detection, and data transformation, providing 
#               a comprehensive set of tools for time series manipulation.

# --- Time utils ---


#' Find Times with Observations Above or Below a Threshold
#'
#' This function identifies the times in a time series where observations are either
#' above or below a specified threshold.
#'
#' @param ts Time series object (\code{ts}) representing the data.
#' @param threshold Numeric, the threshold value for identifying observations.
#' @param upper Logical, indicating whether to find observations above (\code{TRUE})
#'              or below (\code{FALSE}) the threshold.
#'
#' @return Returns a vector of dates corresponding to times where the observations
#'         in the time series are either above or below the specified threshold.
#'
above_threshold <- function(ts, threshold, upper=TRUE) {
  
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


#' Convert Floating-Point Representation to Date Format
#'
#' This function converts a floating-point representation of date and time to a date object.
#' The floating-point representation is assumed to consist of an integer part representing
#' the year and a decimal part representing the day of the year.
#'
#' @param date_float Numeric vector representing the floating-point representation of date.
#'
#' @return Returns a Date object representing the converted date.
#'
float_to_date <- function(date_float) {
  
  # Extract the year (integer part) and the day (decimal part)
  year        <- floor(date_float)
  day_decimal <- date_float - year
  
  # Calculate the day of the year
  day_of_year <- floor(day_decimal * freq$daily) + 1
  
  # Create a Date object
  date_object <- as.Date(paste(year, "-", day_of_year, sep = ""), format="%Y-%j")
  
  return(date_object)
}


#' Convert Date to Floating-Point Representation
#'
#' This function converts a Date object to a floating-point representation, where the
#' integer part represents the year, and the decimal part represents the day of the year.
#'
#' @param date Date object to be converted.
#'
#' @return Returns a numeric value representing the floating-point representation of the date.
#'
date_to_float <- function(date) {
  
  # Extract date components
  year <- as.numeric(format(date, "%Y"))
  day  <- as.numeric(format(date, "%j"))
  
  # Calculate the floating-point representation
  date_float <- year + (day - 1) / freq$daily
  
  return(date_float)
  
}

date_to_weeknumber <- function(date) {
  
  # Extract week number
  wn <- as.numeric(format(date, "%U"))
  
  # Restrict to 52 weeks by making
  # first and last collapse in the adjacent one.
  wn <- ifelse(wn ==  0,  1, 
        ifelse(wn == 53, 52, wn))
  
  return(wn)
  
}

# --- DataFrame to TimeSeries conversion ---


#' Convert Daily DataFrame to Daily Time Series
#'
#' This function converts a daily dataframe with violation data to a daily time series
#' object (\code{ts}). It extracts the starting date and creates a time series with
#' the specified frequency.
#'
#' @param df DataFrame containing daily violation data with a "Date" and "Violations" column.
#'
#' @return Returns a daily time series object (\code{ts}) representing the violation data.
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
    frequency=freq$daily
  )
  
  return(daily.ts)
  
}


#' Convert Daily DataFrame to Weekly Time Series
#'
#' This function converts a daily dataframe with violation data to a weekly time series
#' object (\code{ts}). It aggregates the daily data by week and creates a time series
#' with the specified frequency.
#'
#' @param df DataFrame containing daily violation data with a "Date" and "Violations" column.
#'
#' @return Returns a weekly time series object (\code{ts}) representing the aggregated violation data.
#'
daily_df_to_weekly_ts <- function(df) {
  
  # Create month column to aggregate with
  df$Week <- sapply(df$Date, date_to_weeknumber)
  df$Year = as.numeric(format(df$Date, "%Y"))
  
  # Taking the mean number of violations in a day
  weekly.df <- aggregate(Violations ~ Week + Year, data=df, mean)
  
  # Extracting starting month
  start.date <- df$Date[1]
  start.year <- as.numeric(format(start.date, "%Y"))
  start.week <- date_to_weeknumber(date=start.date)
  
  # Creating monthly time series
  weekly.ts <- ts(
    weekly.df$Violations, 
    start=c(start.year, start.week), 
    frequency=freq$weekly
  )
  
  return(weekly.ts)
}


#' Convert Daily DataFrame to Monthly Time Series
#'
#' This function converts a daily dataframe with violation data to a monthly time series
#' object (\code{ts}). It aggregates the daily data by month and creates a time series
#' with the specified frequency.
#'
#' @param df DataFrame containing daily violation data with a "Date" and "Violations" column.
#'
#' @return Returns a monthly time series object (\code{ts}) representing the aggregated violation data.
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
    frequency=freq$monthly
  )
  
  return(monthly.ts)
}


#' Convert Daily DataFrame to Weekly Time Series (Weekday and Weekend Separation)
#'
#' This function converts a daily dataframe with violation data to two weekly time series
#' objects (\code{ts}), one for weekdays and one for weekends. It separates the daily data
#' based on the day of the week and creates time series with the specified frequency.
#'
#' @param df DataFrame containing daily violation data with a "Date" and "Violations" column.
#'
#' @return Returns a list of two weekly time series objects (\code{ts}), one for weekdays
#'         and one for weekends, representing the aggregated violation data.
#'
daily_df_to_weekly_ts_weekday_weekend <- function(df) {
  
  # Generating Weekday name
  df$WeekdayName = weekdays(df$Date)
  
  # Generating Year and Week number in the year
  df$Year       <- as.numeric(format(df$Date, "%Y"))
  df$WeekNumber <- sapply(df$Date, date_to_weeknumber)
  
  # Generating Weekday or Weekend class
  names.weekday <- names.weekdays[1:5]
  names.weekend <- names.weekdays[6:7]
  
  df$Weekday <- 
    ifelse(df$WeekdayName %in% names.weekday, "Weekday",
    ifelse(df$WeekdayName %in% names.weekend, "Weekend", NA))
  df$Weekday <- as.factor(df$Weekday)
  
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
      frequency=freq$weekly-1
    ),
    # Weekend
    weekend = ts(
      weekly.df.weekend$Violations, 
      start=c(start.year, start.week), 
      frequency=freq$weekly-1
    )
  )
  
  return(weekly.ts)
  
}

# --- Outliers ---

#' Outliers Diagnostic Plot for Time Series
#'
#' This function generates a diagnostic plot for outliers in a time series. It plots the
#' original time series along with detected outliers, their suggested replacements, and a legend.
#'
#' @param ts Time series object (\code{ts}) representing the data to be diagnosed.
#' @param colors List of colors specifying the plot, old outliers, and new outliers.
#' @param main Main title for the plot.
#' @param ylab Label for the y-axis.
#'
#' @return Returns a list containing information about the outliers, including their indices,
#'         times, old values, and suggested replacements.
#'
outliers_diagnostic <- function(ts, colors, main, ylab) {

  # Plot the original time series
  plot(
    ts,
    main=main,
    col=colors$plot,
    ylab=ylab
  )
  grid()
  
  # Extract outliers index and times 
  outliers <- tsoutliers(ts)
  idx      <- outliers$index
  times    <- time(ts)[idx]
  
  # Extract old and new observations
  old  <- ts[idx]
  new_ <- outliers$replacements
  
  # Draw old vs suggested points
  points(times, old,  pch=18, col=colors$old, cex=1.5)
  points(times, new_, pch=18, col=colors$new, cex=1.5)
  
  # Add legend for points
  legend(
    "topleft", 
    legend = c("Observed outlier", "Suggested replacement"), 
    pch = c(18, 18), 
    col = c(colors$old, colors$new)
  )
  
  # Print outliers true vs. suggestion info
  for(i in 1:length(times)) {
    print(paste(
      float_to_date(times[i]), 
      "- old: ", old[i], 
      ", new: ", new_[i])
    )
  }
  
  return(outliers)

}


#' Replace Outliers in DataFrame
#'
#' This function replaces outliers in a dataframe with suggested replacement values. It
#' casts the replacement values to integer violations, retrieves the outlier dates, drops
#' outliers from the dataset, creates replacement rows, and merges the dataframe with the
#' replacement rows. The final dataset is sorted by the date column.
#'
#' @param df DataFrame containing violation data with a "Date" and "Violations" column.
#' @param ts Time series object (\code{ts}) representing the data used for outlier detection.
#' @param outliers List of outliers information, typically obtained from \code{outliers_diagnostic}.
#'
#' @return Returns a modified dataframe with outliers replaced and sorted by the date column.
#'
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
  df <- rbind(df, replacement)
  
  # Sort the dataset by the date column
  df <- df[order(df$Date), ]
  
  return(df)
}

# --- Utils ---

#' Calculate Prediction Errors
#'
#' This function calculates various prediction errors, including Mean Absolute Error (MAE),
#' Mean Percentage Error (MPE), Mean Squared Error (MSE), and Root Mean Squared Error (RMSE).
#'
#' @param true Numeric vector representing the true values.
#' @param predicted Numeric vector representing the predicted values.
#'
#' @return Returns a list of prediction errors, including MAE, MPE, MSE, and RMSE.
#'
prediction_errors <- function(true, predicted) {
  
  # Mean Absolute Error (MAE)
  mae <- mean(abs(true- predicted))
  
  # Mean Percentage Error (MPE)
  mpe <- mean(abs(true- predicted) / true) * 100
  
  # Mean Squared Error (MSE)
  mse <- mean((true- predicted)^2)
  
  # Root Mean Squared Error (RMSE)
  rmse <- sqrt(mse)
  
  # List of errors
  errors <- list(
    mae=mae,
    mpe=mpe,
    mse=mse,
    rmse=rmse
  )
  
  return(errors)
  
}



#' Split Time Series into Training and Test Sets
#'
#' This function splits a time series into training and test sets based on a specified
#' date_split. It calculates the start and end dates for the split depending on the
#' frequency of the time series. The resulting split is returned as a list containing
#' two time series objects, one for the training set and one for the test set.
#'
#' @param ts Time series object (\code{ts}) to be split.
#' @param date_split Date representing the split point between training and test sets.
#' @param train_test Logical indicating whether to interpret the split as a train-test split.
#'
#' @return Returns a list containing two time series objects, one for the training set
#'         and one for the test set.
#'
split_ts <- function(ts, date_split, train_test = FALSE) {
  
  # Finding start and end date for split depending on frequency
  
  if(frequency(ts) == freq$daily) {
    
    # Daily
    start <- date_to_float(date_split)
    end   <- date_to_float(date_split-1)
    
  } else if (frequency(ts) == freq$weekly) {
    
    # Weekly
    start.year  <- as.numeric(format(date_split, "%Y"))
    start.week  <- date_to_weeknumber(date=date_split)
    
    # End as preceeding week
    end.year  <- start.year
    end.week  <- start.week - 1
    
    # Turn of the year case
    if(
      end.week <= 0) {
      end.year <- end.year - 1
      end.week <- end.week + as.numeric(freq$weekly)
    }
    
    start <- c(start.year, start.week)
    end   <- c(  end.year,   end.week)
    
  } else {
    stop(paste(
      "Error: invalid ts frequency ", frequency(ts), 
      " not in {", freq$daily, ", ", freq$weekly, "}", sep=""
    ))
  }
  
  # Split based computed start and end
  split <- (list(
    first  = window(ts, end=end),
    second = window(ts, start=start)
  ))
  
  # Interpret as train-test split 
  if(train_test) {
    names(split) <- c("train", "test")
  }
  
  return(split)
  
}