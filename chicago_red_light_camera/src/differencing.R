tsdisplay_differencing <- function(ts, order=0, main, ylab) {
  
  if(order==0) {
    # No differencing
    difference <- ts
    main_ <- paste(main, "- ACF and PACF")
    ylab_ <- ylab
    
  } else {
    # Differencing
    difference <- diff(ts, order) 
    main_ <- paste(main, "-", order, "order differencing) - ACF and PACF")
    ylab_ <- expression(Y[t] - Y[t-order] ~ ylab)
  }
  
  tsdisplay(
    difference,
    main=main_,
    ylab=ylab_,
    lag.max=100
  )
}