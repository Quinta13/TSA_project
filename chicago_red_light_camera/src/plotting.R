# File: plotting.R
# Author: Sebastiano Quintavalle
# Date: 2024-01-26
# Description: the file contains visualization utils.

# --- Multiple TS ---

#' Plot Multiple Time Series
#'
#' This function plots multiple time series on the same graph with specified colors and legends.
#'
#' @param ts_list List of time series objects (\code{ts}) to be plotted.
#' @param colors List of colors corresponding to each time series.
#' @param legends Vector of legend labels for each time series.
#' @param main Main title for the plot.
#' @param ylab Label for the y-axis.
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
}
  

# --- Weeklyplot ---

#' Plot Weekly Trends
#'
#' This function plots trends for each weekday in a weekly time series data frame.
#'
#' @param df Data frame with Date, Violations, and WeekdayName columns.
#' @param main Main title for the plot.
#'
weeklyplot <- function(df, main) {
  
  # Create weekname
  df$WeekdayName = weekdays(df$Date)
  df$WeekdayName = as.factor(df$WeekdayName)
  
  # Split per weekday
  weekdays.df <- split(
    df,
    df$WeekdayName
  )
  
  # Compute full range
  weekdays.range <- range(do.call(c, lapply(weekdays.df, function(x) x$Violations)))
  
  # Plotting each weekday trend
  
  par(mfrow = c(1, 7), mar = c(4, 4, 2, 1), oma = c(0, 0, 3, 0))
  
  for (name in names.weekdays) {
    plot(
      weekdays.df[[name]]$Date, 
      weekdays.df[[name]]$Violations, 
      type = "l",
      xlab = "Time", 
      ylab = "", 
      main = name, 
      ylim = weekdays.range
    )
    
    abline(h=mean(weekdays.df[[name]]$Violations))
  }
  
  # Add a main title for the entire plot
  mtext(main, line = 0, side = 3, outer = TRUE, cex = 1.5)
  
  # Reset the plotting parameters
  par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
}

