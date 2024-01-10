smoothing_filter <- function(ts, weights) {
  
  ts_filtered <- filter(
    x=ts,
    sides=2,
    filter=weights
  )
  
  return (ts_filtered)
  
}

simple_moving_average <- function(ts, p) {
  
  # Weights
  ma.weights <- rep(1/(2*p+1), 2*p+1)
  
  # Applying filtering
  ma.filter <- smoothing_filter(ts, ma.weights)
  
  return(ma.filter)
  
}

simple_moving_average_varying_p <- function(ts, p, p.color, main, ylab) {
  
  plot(
    ts,
    main=main,
    ylab=ylab
  )
  
  for (i in seq_along(p)) {
    lines(
      simple_moving_average(ts, p[i]),
      lwd=3, lty='dashed',
      col = p.colors[i]
    )
  }
  
  legend(
    "topleft", 
    legend = paste("p = ", p),
    lwd=2, lty=1, 
    col=p.colors
  )
}

spencer <- function(ts) {
  
  # Weights
  spencer.weights <- c(74, 67, 46, 21, 3, -5, -6, -3)
  spencer.weights <- c(rev(spencer.weights[-1]), spencer.weights) 
  spencer.weights <- spencer.weights / sum(spencer.weights) 
  
  # Applying filtering
  spencer.filter <- smoothing_filter(ts, spencer.weights)
  
  return(spencer.filter)
  
}

deseasoning <- function(ts, freq) {
  
  # Weights
  deseasoning.weights <- c(.5, rep(1, freq-1), .5)
  deseasoning.weights <- deseasoning.weights / sum(deseasoning.weights)
  
  # Applying filtering
  deseasoning.filter <- smoothing_filter(ts, deseasoning.weights)
  
  return(deseasoning.filter)
  
}

plot_component_decompositions <- function(ts, component.name, main, ylab) {
  
  par(mfrow=c(length(names(ts)), 1), oma=c(0,0,3,0))
  
  # Loop through each time series and decompose the trend component
  for (i in seq_along(names(ts))) {
    
    name <- names(ts)[i]
    component <- decompose(ts[[name]])[[component.name]]
    
    plot(
      component,
      main=paste(name, "- ", component.name),
      ylab=ylab
    )
    
  }
  
  mtext(main, line=0, side=3, outer=TRUE, cex=2)
  
  par(mfrow=c(1, 1))
  
}



