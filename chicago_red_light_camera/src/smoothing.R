# File: smoothing.R
# Author: Sebastiano Quintavalle
# Date: 2024-01-26
# Description: This file contains functions for applying smoothing filters and performing time series analysis.


#' Apply a smoothing filter to a time series.
#'
#' This function applies a smoothing filter to a given time series using the specified weights.
#'
#' @param ts A time series object.
#' @param weights The filter weights to be used for smoothing.
#' @return A smoothed time series object.
#'
smoothing_filter <- function(ts, weights) {
  
  # Apply the smoothing filter using the specified weights
  ts_filtered <- filter(
    x = ts,
    sides = 2,
    filter = weights
  )
  
  return(ts_filtered)
  
}


#' Calculate the simple moving average of a time series.
#'
#' This function computes the simple moving average of a given time series
#' using a specified window size.
#'
#' @param ts A time series object.
#' @param p Window size for the moving average. The window size is 2*p+1.
#' @return A time series object representing the simple moving average.
#'
simple_moving_average <- function(ts, p) {
  
  # Weights for the moving average
  ma.weights <- rep(1 / (2 * p + 1), 2 * p + 1)
  
  # Applying the smoothing filter to calculate the moving average
  ma.filter <- smoothing_filter(ts, ma.weights)
  
  return(ma.filter)
  
}


#' Plot simple moving averages with varying window sizes.
#'
#' This function plots a time series and overlays multiple simple moving averages 
#' with varying window sizes.
#'
#' @param ts A time series object.
#' @param p Vector of window sizes for the moving averages.
#' @param p.color Vector of colors corresponding to each window size.
#' @param main Main title for the plot.
#' @param ylab Label for the y-axis.
#' @return NULL
#'
simple_moving_average_varying_p <- function(ts, p, p.color, main, ylab) {
  
  # Plot the original time series
  plot(
    ts,
    main = main,
    ylab = ylab
  )
  
  # Overlay simple moving averages with varying window sizes
  for (i in seq_along(p)) {
    lines(
      simple_moving_average(ts, p[i]),
      lwd = 3, lty = 'dashed',
      col = p.color[i]
    )
  }
  
  # Add legend
  legend(
    "topleft", 
    legend = paste("p =", p),
    lwd = 2, lty = 1, 
    col = p.color
  )
  
  # Return NULL as the result is a plot, and it's typically not assigned to a variable.
  return(NULL)
}


#' Apply the Spencer filter to a time series.
#'
#' This function applies the Spencer filter to a given time series using predefined weights.
#'
#' @param ts A time series object.
#' @return A time series object after applying the Spencer filter.
#'
spencer <- function(ts) {
  
  # Define Spencer filter weights
  spencer.weights <- c(74, 67, 46, 21, 3, -5, -6, -3)
  spencer.weights <- c(rev(spencer.weights[-1]), spencer.weights) 
  spencer.weights <- spencer.weights / sum(spencer.weights) 
  
  # Apply the Spencer filter using the specified weights
  spencer.filter <- smoothing_filter(ts, spencer.weights)
  
  return(spencer.filter)
  
}


#' Apply a deseasoning filter to a time series.
#'
#' This function applies a deseasoning filter to a given time series using predefined weights.
#'
#' @param ts A time series object.
#' @param freq The frequency of the seasonality in the time series.
#' @return A time series object after applying the deseasoning filter.
#'
deseasoning <- function(ts, freq) {
  
  # Define deseasoning filter weights
  deseasoning.weights <- c(.5, rep(1, freq - 1), .5)
  deseasoning.weights <- deseasoning.weights / sum(deseasoning.weights)
  
  # Apply the deseasoning filter using the specified weights
  deseasoning.filter <- smoothing_filter(ts, deseasoning.weights)
  
  return(deseasoning.filter)
  
}


#' Plot decompositions of a specific component for multiple time series.
#'
#' This function plots the decomposition of a specific component (e.g., trend, seasonal, etc.)
#' for each time series in a list.
#'
#' @param ts_list A list of time series objects.
#' @param names Names for each time series, displayed as plot titles.
#' @param component.name Name of the component to be decomposed and plotted (e.g., "trend", "seasonal").
#' @param main Main title for the entire plot.
#' @param ylab Label for the y-axis.
#' @return NULL
#'
plot_component_decompositions <- function(ts_list, names, component.name, main, ylab) {
  
  # Set up multiple plots in a single column
  par(mfrow = c(length(ts_list), 1))
  
  # Loop through each time series and decompose the specified component
  for (i in 1:length(ts_list)) {
    
    # Decompose the specified component
    component <- decompose(ts_list[[i]])[[component.name]]
    
    # Plot the component
    plot(
      component,
      main = names[i],
      ylab = ylab
    )
    
  }
  
  # Add a main title for the entire plot
  mtext(main, line = 0, side = 3, outer = TRUE, cex = 2)
  
  # Reset the plotting layout
  par(mfrow = c(1, 1))
  
  # Return NULL as the result is a plot, and it's typically not assigned to a variable.
  return(NULL)
}

