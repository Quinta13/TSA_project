arima_formula <- function (fit) {
  
  # Extract order
  order <- arimaorder(fit)
  
  formula_ <- paste(
    "ARIMA(", 
    order['p'], ",", 
    order['d'], ",", 
    order['q'], ")",
    sep=""
  )
  
  if(! is.na(order['Frequency'])) {
    formula_ <- paste(
      formula_,
      "(",
      order['P'], ",", 
      order['D'], ",",
      order['Q'], ")[", 
      order['Frequency'], "]",
      sep = ""
    )
  }
  
  return(formula_)
  
}

arima_ics <- function(fit) {
  
  npar <- length(fit$coef) + 1
  nstar <- length(fit$residuals) - fit$arma[6] - fit$arma[7] * fit$arma[5]
  
  bic  <- fit$aic + npar * (log(nstar) - 2)
  aicc <- fit$aic + 2 * npar * (nstar/(nstar - npar - 1) - 1)
  
  ics <- list(
    aic = fit$aic,
    aicc = aicc,
    bic = bic
  )
  
  return(ics)
  
}

arima_selection <- function(fit_list, criteria) {
  
  fit_best <- fit_list[[1]]
  
  for(fit in fit_list) {
    
    # Extracting formula and IC
    formula_ <- arima_formula(fit=fit)
    ics      <- arima_ics    (fit=fit)
    
    # Print
    cat(
      formula_, "\n",
      " - AIC:  ", ics$aic, "\n",
      " - AICc: ", ics$aicc, "\n",
      " - BIC:  ", ics$bic,  "\n\n"
    )
    
    # Compare IC criterion
    if(ics[[criteria]] < arima_ics(fit_best)[[criteria]]) {
      fit_best <- fit
    }
    
  }
  
  return(fit_best)
  
}

prediction_erros <- function(true, predicted) {
  
  # Mean Absolute Error (MAE)
  mae <- mean(abs(true- predicted))
  
  # Mean Percentage Error (MPE)
  mpe <- mean((true- predicted) / true) * 100
  
  # Mean Squared Error (MSE)
  mse <- mean((true- predicted)^2)
  
  # Root Mean Squared Error (RMSE)
  rmse <- sqrt(mse)
  
  errors <- list(
    mae=mae,
    mpe=mpe,
    mse=mse,
    rmse=rmse
  )
  
}

train_test_model <- function(ts, date_split, arima_order, freq_type, main, ylab, fourier_=NA) {
  
  # Finding start and end date for split
  if(freq_type == "daily") {
    start <- date_to_float(date_split)
    end   <- date_to_float(date_split-1)
    
  } else if (freq_type == "weekly") {
    
    start.year <- as.numeric(format(date_split, "%Y"))
    start.week  <- as.numeric(format(date_split, "%U"))
    
    # Apply to 50-week format
    if(start.week == 0) {
      start.week <- 1
    } else if(start.week == 52 || start.week == 53) {
      start.week <- 50
    }
    
    end.year <- start.year
    end.week  <- start.week - 1
    
    if(end.week <= 0) {
      end.year <- end.year - 1
      end.week <- end.week + as.numeric(weekly.freq)
    }
    
    start <- c(start.year, start.week)
    end   <- c(  end.year,   end.week)
    
  } else if (freq_type == "monthly") {
    
    start.year   <- as.numeric(format(date_split, "%Y"))
    start.month  <- as.numeric(format(date_split, "%m"))
    
    end.year   <- start.year
    end.month  <- start.month - 1
    
    if(end.month <= 0) {
      end.year <- end.year - 1
      end.month <- end.month + as.numeric(monthly.freq)
    }
    
    start <- c(start.year, start.month)
    end   <- c(  end.year,   end.month)
  
  } else {
    stop("Error: invalid freq_type not in {daily, weekly, monthly}")
  }
  
  # Split
  train <- window(ts, end=end)
  test  <- window(ts, start=start)
  
  # ARIMA fit
  
  # Order
  order <- c(arima_order['p'], arima_order['d'], arima_order['q'])
  
  # Seasonality
  if(!is.na(arima_order['Frequency'])) {
    seasonal = list(
      order = c(arima_order['P'], arima_order['D'], arima_order['Q']),
      period = arima_order['Frequency']
    )
  } else {
    seasonal = c(0, 0, 0)
  }
  
  if(is.na(fourier_)) {
    xreg <- NULL
  } else {
    xreg <- fourier(train, K=4)
  }

  fit <- Arima(
      train,
      order=order,
      seasonal=seasonal,
      xreg=xreg
  )
  
  if(is.na(fourier_)) {
    xreg2 <- NULL
  } else {
    xreg2 <- fourier(train, K=4, h=length(test))
  }
  
  # Make predictions on the test set
  predicted <- forecast(fit, h=length(test), xreg=xreg2)
  
  # Compute the range of all lines involved
  combined_range <- range(c(
    ts, test, 
    predicted$lower[, "95%"],
    predicted$upper[, "95%"],
    predicted$lower[, "80%"], 
    predicted$upper[, "80%"]
  ))
  
  # Plot the original time series and the predicted values for the test set with the calculated range
  plot(ts, main=main, ylab=ylab, ylim=combined_range)
  
  # 95% PI
  polygon(
    c(time(test),   rev(time(test))),
    c(predicted$lower[, "95%"], rev(predicted$upper[, "95%"])),
    col = "#dbdbdf",border = NA
  )
  
  # 80% PI
  polygon(
    c(time(test), rev(time(test))),
    c(predicted$lower[, "80%"], rev(predicted$upper[, "80%"])),
    col = "#b1b5ce", border = NA
  )
  
  # True
  lines(test, col="tomato", lwd=2)
  
  # Predicted
  lines(predicted$mean, col="#2297e6", lwd=2)
  
  
  # Legend
  legend(
    "topleft",
    legend = c("True",      "Predicted", "80% CI",  "95% CI" ),
    col    = c("tomato", "#2297e6",   "#b1b5ce", "#dbdbdf"),
    lty    = c(1,           1,           1,         1        ),  
    lwd    = c(2,           2,           2,         2        ),
    cex = 0.8,
    bg = "white"
  )
  
  errors <- prediction_erros(
    true=as.numeric(test),
    predicted=predicted$mean
  )
  
  return(errors)
  
}
