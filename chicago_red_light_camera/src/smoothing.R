# File: smoothing.R
# Author: Sebastiano Quintavalle
# Date: 2024-01-26
# Description: This file contains functions for applying smoothing and high pass filters and performing time series analysis.


# --- Filtering ---
#' Apply Filtering to Time Series
#'
#' This function applies a specified filter to a time series object.
#'
#' @param ts Time series object (\code{ts}) representing the data to be filtered.
#' @param weights Numeric vector specifying the filter weights.
#'
#' @return Returns a time series object (\code{ts}) representing the result of applying
#'         the specified filter to the input time series.
#'
apply_filtering <- function(ts, weights) {
  
  # Apply the filter using the specified weights
  ts_filtered <- filter(
    x=ts,
    sides=2,
    filter=weights
  )
  
  return(ts_filtered)
  
}


# --- Moving Average ---

#' Calculate Simple Moving Average for Time Series
#'
#' This function calculates the simple moving average for a given time series object
#' using a specified window size.
#'
#' @param ts Time series object (\code{ts}) representing the data to be smoothed.
#' @param p Integer, the window size for the moving average.
#'
#' @return Returns a time series object (\code{ts}) representing the result of the
#'         simple moving average calculation.
#'
simple_ma <- function(ts, p) {
  
  # Weights for the moving average
  ma.weights <- rep(1 / (2 * p + 1), 2 * p + 1)
  
  # Applying the smoothing filter to calculate the moving average
  ma.filter <- apply_filtering(ts=ts, weights=ma.weights)
  
  return(ma.filter)
  
}


#' Plot Simple Moving Averages with Varying Window Sizes
#'
#' This function overlays simple moving averages with varying window sizes on the
#' original time series plot, allowing for visual comparison.
#'
#' @param ts Time series object (\code{ts}) representing the original data.
#' @param p Numeric vector specifying the window sizes for the moving averages.
#' @param p.color Character vector specifying the colors for each moving average.
#' @param main Main title for the plot.
#' @param ylab Label for the y-axis.
#'
plot_simple_ma_varying_p <- function(ts, p, p.color, main, ylab) {
  
  # Plot the original time series
  plot(
    ts,
    main=main,
    ylab=ylab
  )
  
  # Overlay simple moving averages with varying window sizes
  for (i in seq_along(p)) {
    lines(
      simple_ma(ts=ts, p=p[i]),
      lwd=3, lty='dashed',
      col=p.color[i]
    )
  }
  
  # Add legend
  legend(
    "topleft", 
    legend=paste("p =", p),
    lwd=2, lty=1, col=p.color
  )
  
}


#' Plot Moving Average Decomposition Components
#'
#' This function decomposes a time series into its components (observed, trend, seasonal,
#' and random) using the moving average method and plots them for visual inspection.
#'
#' @param ts Time series object (\code{ts}) to be decomposed.
#' @param main Main title for the plot.
#'
#' @return Returns \code{NULL} as the result is a plot, and it's typically not
#'         assigned to a variable.
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
}


# --- Binomial ---

#' Calculate Binomial Smoothing for Time Series
#'
#' This function calculates the binomial smoothing for a given time series object
#' using a specified order.
#'
#' @param ts Time series object (\code{ts}) representing the data to be smoothed.
#' @param p Integer, the order of the binomial smoothing.
#'
#' @return Returns a time series object (\code{ts}) representing the result of the
#'         binomial smoothing calculation.
#'
binomial <- function(ts, p) {
  
  # Generating pascal triangle
  triangle <- matrix(0, nrow = p, ncol = p)
  
  # Fill the triangle
  for (i in 1:p) {
    for (j in 1:i) {
      if (j == 1 || j == i) {
        triangle[i, j] <- 1
      } else {
        triangle[i, j] <- triangle[i - 1, j - 1] + triangle[i - 1, j]
      }
    }
  }
  
  # Retrieve weights from the last triangle row
  binomial.weights = triangle[p, ] / 2^(p-1)
  
  # Applying the smoothing filter to calculate the moving average
  binomial.filter <- apply_filtering(ts=ts, weights=binomial.weights)
  
  return(binomial.filter)
  
}


#' Plot Binomial Smoothings with Varying Orders
#'
#' This function overlays binomial smoothings with varying orders on the original
#' time series plot, allowing for visual comparison.
#'
#' @param ts Time series object (\code{ts}) representing the original data.
#' @param p Numeric vector specifying the orders for the binomial smoothings.
#' @param p.color Character vector specifying the colors for each binomial smoothing.
#' @param main Main title for the plot.
#' @param ylab Label for the y-axis.
#'
plot_binomial_varying_p <- function(ts, p, p.color, main, ylab) {
  
  # Plot the original time series
  plot(
    ts,
    main=main,
    ylab=ylab
  )
  
  # Overlay simple moving averages with varying window sizes
  for (i in seq_along(p)) {
    lines(
      binomial(ts=ts, p=p[i]),
      lwd=3, lty='dashed',
      col=p.color[i]
    )
  }
  
  # Add legend
  legend(
    "topleft", 
    legend=paste("p =", p),
    lwd=2, lty=1, col=p.color
  )
  
}


# --- Spencer ---

#' Apply Spencer Filter to Time Series
#'
#' This function applies the Spencer filter to a given time series object.
#'
#' @param ts Time series object (\code{ts}) representing the data to be filtered.
#'
#' @return Returns a time series object (\code{ts}) representing the result of
#'         applying the Spencer filter to the input time series.
#'
spencer <- function(ts) {
  
  # Define Spencer filter weights
  spencer.weights <- c(74, 67, 46, 21, 3, -5, -6, -3)
  spencer.weights <- c(rev(spencer.weights[-1]), spencer.weights) 
  spencer.weights <- spencer.weights / sum(spencer.weights) 
  
  # Apply the Spencer filter using the specified weights
  spencer.filter <- apply_filtering(ts=ts, weights=spencer.weights)
  
  return(spencer.filter)
  
}


# --- Deseasoning ---

deseasoning <- function(ts, freq) {
  
  # Define deseasoning filter weights
  deseasoning.weights <- c(.5, rep(1, freq - 1), .5)
  deseasoning.weights <- deseasoning.weights / sum(deseasoning.weights)
  
  # Apply the deseasoning filter using the specified weights
  deseasoning.filter <- apply_filtering(ts=ts, weights=deseasoning.weights)
  
  return(deseasoning.filter)
  
}


# -- STL ---

#' Apply Deseasoning Filter to Time Series
#'
#' This function applies a deseasoning filter to a given time series object.
#'
#' @param ts Time series object (\code{ts}) representing the data to be filtered.
#' @param freq Integer, the frequency of the seasonal pattern in the time series.
#'
#' @return Returns a time series object (\code{ts}) representing the result of
#'         applying the deseasoning filter to the input time series.
#'
plot_stl_decomposition <- function(ts, main) {
  
  # Decompose the time series
  decomposition <- stl(ts, s.window=frequency(ts))
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
}

#' Plot STL Decomposition Components for Multiple Time Series
#'
#' This function plots specified components (e.g., trend, seasonal, random) of the STL
#' decomposition for multiple time series objects.
#'
#' @param ts_list List of time series objects (\code{ts}) to be decomposed.
#' @param names Character vector of names corresponding to each time series in \code{ts_list}.
#' @param component_name Character, the name of the component to plot (e.g., "trend", "seasonal").
#' @param freq Integer, the frequency of the seasonal pattern in the time series.
#' @param main Main title for the plot.
#' @param ylab Label for the y-axis.
#'
plot_stl_components <- function(ts_list, names, component_name, freq=NA, main, ylab) {
  
  # Set up multiple plots in a single column
  par(mfrow = c(length(ts_list), 1), oma=c(0,0,2,0))
  
  # Loop through each time series and decompose the specified component
  for (name in names) {
    
    ts <- ts_list[[name]]
    
    # Setting frequency
    if (is.na(freq)) {
      print("Setting")
      freq <- frequency(ts)
      
    } else {
      freq_ <- freq
    }
    
    # Decompose the specified component
    decomposition <- stl(ts, s.window=freq)
    component     <- decomposition$time.series[, component_name]
    
    # Plot the component
    plot(
      component,
      main = name,
      ylab = ylab
    )
    
  }
  
  # Add a main title for the entire plot
  mtext(main, side = 3, line = - 2, padj=-1, outer=TRUE)
  
  # Reset the plotting layout
  par(mfrow = c(1, 1), oma=c(0,0,0,0))
  
}
