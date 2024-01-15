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

library(rlang)

tsdisplay_differencing <- function(ts, order=0, main, ylab) {
  
  if(order==0) {
    # No differencing
    difference <- ts
    main_ <- paste(main, "- ACF and PACF")
    ylab_ <- ylab
    
  } else {
    # Differencing
    difference <- diff(ts, order) 
    main_ <- paste(main, "-", order, "order differencing - ACF and PACF")
    a<-1
    ylab_ <- paste(ylab, " - ", order, " diff", sep="")
  }
  
  tsdisplay(
    difference,
    main=main_,
    ylab=ylab_,
    lag.max=100
  )
}

plot_multipe_differencing_acf_pacf <- function(ts_list, names, main, order=0) {
  
  par(mfrow=c(2, length(ts_list)))
  
  for(name in names) {
    
    ts <- ts_list[[name]]
    
    if(order==0) {
      # No differencing
      difference <- ts
      main_ <- paste(name, "-", main, "- ACF")
      
    } else {
      # Differencing
      difference <- diff(ts, order) 
      main_ <- paste(name, "-", main, "-", order, "order differencing - ACF")
    } 
    
    acf(
      as.numeric(difference),
      main=main_,
      lag.max=100
    )
  }
  
  for(name in names) {
    
    daily <- area.ts.daily[[name]]
    
    if(order==0) {
      # No differencing
      difference <- ts
      main_ <- paste(name, "-", main, "- PACF")
      
    } else {
      # Differencing
      difference <- diff(ts, order) 
      main_ <- paste(name, "-", main, "-", order, "order differencing - PACF")
    } 
    
    pacf(
      as.numeric(difference),
      main=main_,
      lag.max=100
    )
  }
  
  par(mfrow=c(1, 1))
}

#' Plot decompositions of a specific component for multiple time series.
#'
#' This function plots the decomposition of a specific component (e.g., trend, seasonal, etc.)
#' for each time series in a list.
#'
#' @param ts_list A list of time series objects.
#' @param names Names for each time series, displayed as plot titles.
#' @param component_name Name of the component to be decomposed and plotted (e.g., "trend", "seasonal").
#' @param main Main title for the entire plot.
#' @param ylab Label for the y-axis.
#' @return NULL
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
  
  # Return NULL as the result is a plot, and it's typically not assigned to a variable.
  return(NULL)
}

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
