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

arima_fit_selection <- function(fit_list, criteria) {
  
  valid_criterias <- c("AIC", "AICc", "BIC")
  if (!(criteria %in% valid_criterias)) {
    stop("Invalid criteria. Choose from: ", paste(valid_criterias, collapse = ", "))
  }
  
  models_df <- data.frame(
    ModelName = names(fit_list),
    Formula = sapply(fit_list, arima_formula),
    AIC     = sapply(fit_list, function(x) arima_ics(x)$aic),
    AICc    = sapply(fit_list, function(x) arima_ics(x)$aicc),
    BIC     = sapply(fit_list, function(x) arima_ics(x)$bic)
  )
  
  rownames(models_df) <- NULL
  
  print(models_df)
  
  best <- models_df$ModelName[which.min(models_df[[criteria]])]
  
  print(paste("Best: ", best))
  
  return(fit_list[[best]])
  
}

prediction_erros <- function(true, predicted) {
  
  # Mean Absolute Error (MAE)
  mae <- mean(abs(true- predicted))
  
  # Mean Percentage Error (MPE)
  mpe <- mean(abs(true- predicted) / true) * 100
  
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

split_ts <- function(ts, date_split, train_test = FALSE) {
  
  # Finding start and end date for split
  if(frequency(ts) == daily.freq) {
    
    start <- date_to_float(date_split)
    end   <- date_to_float(date_split-1)
    
  } else if (frequency(ts) == weekly.freq) {
    
    start.year  <- as.numeric(format(date_split, "%Y"))
    start.week  <- week_number_conversion(date=date_split)
    
    # Apply to 52-week format
    if(start.week == 53) {
      start.week <- 52
    }
    
    end.year  <- start.year
    end.week  <- start.week - 1
    
    if(end.week <= 0) {
      end.year <- end.year - 1
      end.week <- end.week + as.numeric(weekly.freq)
    }
    
    start <- c(start.year, start.week)
    end   <- c(  end.year,   end.week)
    
  } else {
    stop(paste(
      "Error: invalid ts frequency ", frequency(ts), 
      " not in {", daily.freq, ", ", weekly.freq, "}", sep=""
    ))
  }
  
  split <- (list(
    first  = window(ts, end=end),
    second = window(ts, start=start)
  ))
  
  if(train_test) {
    names(split) <- c("train", "test")
  }
  
  return(split)
  
}

model_evaluation <- function(ts, test, model, main, ylab) {

  
  # ARIMA fit
  new_model <-  Arima(
      y=ts, model=model
  )
  
  # Make predictions on the test set
  predicted <- forecast(object=new_model, h=length(test))
    
  # Compute the range of all lines involved
  combined_range <- range(c(
    ts, test, 
    predicted$lower[, "95%"],
    predicted$upper[, "95%"],
    predicted$lower[, "80%"], 
    predicted$upper[, "80%"]
  ))
  
  # Plot the original time series and the predicted values for the test set with the calculated range
  plot(
    ts,
    main=main,
    ylab=ylab, 
    xlim=range(do.call(c, lapply(list(ts, test), time)), na.rm = TRUE),
    ylim=combined_range
  )
  grid()
  
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
  
  # Compute errors
  errors <- prediction_erros(
    true=as.numeric(test),
    predicted=predicted$mean
  )
  
  return(errors)
  
}

model_evaluation_multiple <- function(ts, model, split_dates, main, ylab) {
  
  errors_df <- data.frame(
    Date = character(),
    MAE  = numeric(), 
    MPE  = numeric(), 
    MSE  = numeric(), 
    RMSE = numeric()
  )
  
  for(date_split in split_dates) {
    
    print(date_split)
    
    tss <- split_ts(ts=ts, date_split=date_split)
    
    errors <- model_evaluation(
      ts=tss$first,
      test=tss$second,
      model=model,
      main=paste(main, date_split),
      ylab=ylab
    )
    
    errors_df <- rbind(
      errors_df, data.frame(
        Date = date_split,
        MAE  = errors$mae, 
        MPE  = errors$mpe, 
        MSE  = errors$mse, 
        RMSE = errors$rmse
      )
    )
    
  }
  
  return(errors_df)
  
}

model_evaluate_single_prediction <- function(ts, model, date_start, date_end, freq_type, main, ylab) {
  
  if(freq_type=="daily") {
    step <- 1
  } else if(freq_type=="weekly") {
    step <- 7
  } else {
    stop(paste("Invalid freq_type: ", freq_type,". Choose one in {daily, weekly}", sep=""))
  }
  
  prediction_df <- data.frame(
    Date  = numeric(),
    True  = numeric(),
    Pred  = numeric(),
    Low80 = numeric(),
    Up80  = numeric(),
    Low95 = numeric(),
    Up95  = numeric(),
    MAE   = numeric(), 
    MPE   = numeric(), 
    MSE   = numeric(), 
    RMSE  = numeric()
  )
  
  date_iter <- date_start
  
  while(date_iter <= date_end) {
    
    tss <- split_ts(
      ts=ts, 
      date_split=date_iter
    )
    
    ts_fit  <- tss$first
    ts_pred <- window(tss$second, start = start(tss$second), end = start(tss$second))

    fit <- Arima(ts_fit, model=model)
    
    forecast <- forecast(fit, h=1)
    
    errors <- prediction_erros(
      true=as.numeric(ts_pred),
      predicted=as.numeric(forecast$mean)
    )
    
    prediction_df <- rbind(
      prediction_df, data.frame(
        True  = as.numeric(ts_pred),
        Pred  = as.numeric(forecast$mean),
        Low80 = as.numeric(forecast$lower[, "80%"]),
        Up80  = as.numeric(forecast$upper[, "80%"]),
        Low95 = as.numeric(forecast$lower[, "95%"]),
        Up95  = as.numeric(forecast$upper[, "95%"]),
        MAE   = errors$mae, 
        MPE   = errors$mpe, 
        MSE   = errors$mse, 
        RMSE  = errors$rmse
      )
    )
    
    date_iter <- date_iter + step
    
  }
  
  start.year <- as.numeric(format(date_start, "%Y"))
  
  if(freq_type=="daily") {
    start.day <- as.numeric(format(date_start, "%j"))
    ts_out <- ts(prediction_df, start=c(start.year, start.day), freq=daily.freq)
  } else {
    start.week <- week_number_conversion(date=date_start)
    ts_out <- ts(prediction_df, start=c(start.year, start.week), freq=weekly.freq)
  }
  
  # Plot the original time series and the predicted values for the test set with the calculated range
  combined_range <- range(c(
    ts_out[, "True"],
    ts_out[, "Pred"],
    ts_out[, "Low80"],
    ts_out[, "Up80"],
    ts_out[, "Low95"],
    ts_out[, "Up95"]
  ))
  
  plot(
    ts_out[, "True"],
    main=main,
    ylab=ylab,
    col="tomato",
    ylim=combined_range
  )
  grid()
  
  # 95% PI
  polygon(
    c(time(ts_out), rev(time(ts_out))),
    c(ts_out[, "Low95"], rev(ts_out[, "Up95"])),
    col = "#dbdbdf",border = NA
  )
  
  # 80% PI
  polygon(
    c(time(ts_out), rev(time(ts_out))),
    c(ts_out[, "Low80"], rev(ts_out[, "Up80"])),
    col = "#b1b5ce", border = NA
  )
  
  # True
  lines(ts_out[, "True"], col="tomato", lwd=2)
  
  # Predicted
  lines(ts_out[, "Pred"], col="#2297e6", lwd=2)
  
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
  
  return(ts_out)
}

errors_analysis <- function(pred_ts, error_type, k, start_date, freq_type, main, ylab) {
  
  pred_ts_err <- pred_ts[, error_type]
  
  plot(
    pred_ts_err,
    main=main,
    ylab=ylab
  )
  grid()
  
  indices <- rev(tail(order(pred_ts_err), k))
  
  points(
    time(pred_ts_err)[indices],
    pred_ts_err[indices],
    pch=18,
    cex=2,
    col="orangered"
  )
  
  for(i in indices) {
    
    if(freq_type=="daily") {
      print(paste(
        "Date: ", float_to_date(time(pred_ts_err)[i]), " - ", error_type, ": ", pred_ts_err[i], sep="")) 
    } else if(freq_type=="weekly") {
      date_week<- as.Date("2022-01-03") + i * 7
      print(paste("Week from ", date_week, " to ", date_week+6, ": ", single_prediction[, "RMSE"][i], sep="")) 
    } else {
      stop(paste("Invalid freq_type: ", freq_type,". Choose one in {daily, weekly}", sep=""))
    }
    
  }
  
}


compare_models_ic <- function(models, ylab) {
  
  ic_info = data.frame(
    Model = character(),
    AIC   = numeric(), 
    AICc  = numeric(), 
    BIC   = numeric()
  )
  
  par(mfrow=c(length(models), 1))
  for(model_name in names(models)) {
    model <- models[[model_name]]
    plot(forecast(model$fit, xreg=model$xreg, h=model$h), main=model$main, ylab=ylab)
    ics <- arima_ics(model$fit)
    ic_info <- rbind(ic_info, data.frame(
      Model = model_name,
      AIC  = ics$aic,
      AICc = ics$aicc,
      BIC  = ics$bic
    ))
  }
  par(mfrow=c(1, 1))
  
  return(ic_info)
  
}

compare_models_prediction_error <- function(models, train, test, main, ylab, plot_train=TRUE) {
  
  errors_df = data.frame(
    Model = character(),
    MAE   = numeric(), 
    MPE   = numeric(), 
    MSE   = numeric(),
    RMSE  = numeric()
  )
  
  predicted = list()
  
  for(model_name in names(models)) {
    model <- models[[model_name]]
    predicted[[model_name]] <- forecast(model$fit, xreg=model$xreg, h=model$h)$mean
    
    errors <- prediction_erros(
      true=test, 
      predicted = predicted[[model_name]]
    )
    
    errors_df <- rbind(errors_df, data.frame(
      Model = model_name,
      MAE   = errors$mae, 
      MPE   = errors$mpe, 
      MSE   = errors$mse,
      RMSE  = errors$rmse
    ))
  }
  
  cols <- lapply(models, function(x) {x$col})
  cols$True = "steelblue"
  predicted$True <- test
  
  if(plot_train) {
    predicted$Train <- train
    cols$Train <- "black"
  }
  
  plot_multiple_time_series(
    ts_list = predicted,
    names = names(predicted),
    colors=cols,
    main = main,
    ylab=ylab
  )
  
  return(errors_df)
  
}
