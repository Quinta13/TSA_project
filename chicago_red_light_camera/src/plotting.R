# File: plotting.R
# Author: Sebastiano Quintavalle
# Date: 2024-01-26
# Description: The file contains plotting utilies functions.

# --- Multiple TS ---

#' Plot Time Series in a Grid
#'
#' This function creates a grid of plots, where each plot displays a single time series
#' from a list of time series objects. It allows for customizing the layout, colors,
#' y-axis label, and main title of the entire plot.
#'
#' @param ts_list List of time series objects to be plotted.
#' @param n_row Integer, the number of rows in the grid.
#' @param names Character vector of names corresponding to each time series in \code{ts_list}.
#' @param colors List of colors corresponding to each time series in \code{ts_list}.
#' @param ylab Label for the y-axis.
#'
#' @return Returns \code{NULL} as the result is a plot, and it's typically not
#'         assigned to a variable.
#'
plot_ts_grid <- function(ts_list, n_row=1, names, colors, ylab) {
  
  # Set output parameters
  par(
    mfrow = c(n_row, ceiling(length(ts_list)/n_row))
  )
  
  # Plot each single time series
  for (name in names) {
    
    plot(
      ts_list[[name]],
      main=name,
      col=colors[[name]],
      ylab=ylab
    )
    grid()
    
  }
  
  # Reset the plotting parameters
  par(mfrow = c(1, 1))
  
}

#' Plot Multiple Time Series in a single plot
#'
#' This function plots multiple time series on the same graph with specified colors and legends.
#'
#' @param ts_list List of time series objects (\code{ts}) to be plotted.
#' @param colors List of colors corresponding to each time series.
#' @param legends Vector of legend labels for each time series.
#' @param main Main title for the plot.
#' @param ylab Label for the y-axis.
#'
plot_multiple_ts <- function(ts_list, names, colors, main, ylab, lwd=1, lty=NULL) {
  
  
  # Plot the first time series
  ?plot
  if(is.null(lty)) {lty_ <- 'solid'} else {lty_ <- lty[1]}
  plot(
    ts_list[[names[1]]],
    xlim=range(do.call(c, lapply(ts_list, time)), na.rm = TRUE),
    ylim=range(unlist(ts_list), na.rm=TRUE),
    type="l", col=colors[[names[1]]],
    main=main, xlab="Year", ylab=ylab, lwd=lwd, lty=lty_ 
  )
  grid()
  
  # Add the remaining time series to the plot
  for(i in 2:length(names)) {
    if(is.null(lty)) {lty_ <- 'solid'} else (lty_ <- lty[i])
    i_name = names[i]
    lines(ts_list[[i_name]], type="l", lwd=lwd, lty=lty_, col=colors[[i_name]])
  }
  
  # Add legend
  legend(
    "topleft", legend=names, cex = 0.6,
    lty=lty, lwd=2, col=unlist(colors)
  )
}
  

# --- Weeklyplot ---

#' Plot Weekly Trends
#'
#' This function plots trends for each weekday in a weekly time series data frame.
#'
#' @param df Data frame with Date, Violations, and WeekdayName columns.
#' @param main Main title for the plot.
#'
#'
weeklyplot <- function(df, main) {
  
  # Create weekname
  df$WeekdayName = weekdays(df$Date)
  
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

