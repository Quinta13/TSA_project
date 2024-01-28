# File: time.R
# Author: Sebastiano Quintavalle
# Date: 2024-01-26
# Description: # File: time.R
# Author: Sebastiano Quintavalle
# Date: 2024-01-26
# Description: 


# --- ARIMA object manipulation --- 

#' Extract ARIMA Formula from ARIMA Fit
#'
#' This function extracts the ARIMA formula from a fitted ARIMA model.
#'
#' @param arima_fit Fitted ARIMA model object.
#'
#' @return Returns a character string representing the ARIMA formula of the fitted model.
#'
arima_formula <- function (arima_fit) {
  
  # Extract order
  order <- arimaorder(arima_fit)
  
  # Ordinary part
  formula_ <- paste(
    "ARIMA(", 
    order['p'], ",", 
    order['d'], ",", 
    order['q'], ")",
    sep=""
  )
  
  # Seasonal part (optional)
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


#' Calculate Information Criteria for ARIMA Model
#'
#' This function calculates various information criteria (AIC, AICc, BIC) for an ARIMA model.
#'
#' @param arima_fit Fitted ARIMA model object.
#'
#' @return Returns a list containing the calculated information criteria: AIC, AICc, and BIC.
#'
arima_ic <- function(arima_fit) {
  
  # Extract model parameters
  npar  <- length(arima_fit$coef) + 1
  nstar <- length(arima_fit$residuals) - arima_fit$arma[6] - arima_fit$arma[7] * arima_fit$arma[5]
  
  # Compute BIC and AICc based on model AIC
  bic  <- arima_fit$aic + npar * (log(nstar) - 2)
  aicc <- arima_fit$aic + 2 * npar * (nstar/(nstar - npar - 1) - 1)
  
  # List of IC
  ic <- list(
    AIC = arima_fit$aic,
    AICc = aicc,
    BIC = bic
  )
  
  return(ic)
  
}


#' Select ARIMA Model Based on Information Criteria
#'
#' This function selects the best ARIMA model from a list of fitted ARIMA models
#' based on specified information criteria (AIC, AICc, BIC).
#'
#' @param fit_list List of ARIMA models, where each element is a fitted ARIMA model.
#' @param criteria Character, the information criterion to be used for model selection. 
#'                 Should be one of "AIC", "AICc", or "BIC".
#'
#' @return Returns the best ARIMA model based on the specified information criterion.
#'
arima_fit_selection <- function(fit_list, criteria) {
  
  # Check for correct input
  valid_criterias <- c("AIC", "AICc", "BIC")
  if (!(criteria %in% valid_criterias)) {
    stop("Invalid criteria. Choose from: ", paste(valid_criterias, collapse = ", "))
  }
  
  # Generate dataframe
  models_df <- data.frame(
    ModelName = names(fit_list),
    Formula = sapply(fit_list, arima_formula),
    AIC     = sapply(fit_list, function(x) arima_ic(x)$AIC),
    AICc    = sapply(fit_list, function(x) arima_ic(x)$AICc),
    BIC     = sapply(fit_list, function(x) arima_ic(x)$BIC)
  )
  rownames(models_df) <- NULL
  
  print(models_df)
  
  # Find best model according to criteria
  best <- models_df$ModelName[which.min(models_df[[criteria]])]
  
  print(paste("Best: ", best))
  
  return(fit_list[[best]])
  
}


#' Evaluate ARIMA Model on Test Set
#'
#' This function evaluates the performance of an ARIMA model on a test set by comparing
#' the predicted values with the true values. It provides a visual representation of the
#' model's predictions along with confidence intervals.
#'
#' @param ts Time series object (\code{ts}) representing the entire dataset.
#' @param test Time series object (\code{ts}) representing the test set.
#' @param model Fitted ARIMA model object.
#' @param main Character, the main title of the plot.
#' @param ylab Character, the label for the y-axis of the plot.
#'
#' @return Returns a list of errors, including Mean Absolute Error (MAE), Mean Percentage
#'         Error (MPE), Mean Squared Error (MSE), and Root Mean Squared Error (RMSE).
#'
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
  errors <- prediction_errors(
    true=as.numeric(test),
    predicted=predicted$mean
  )
  
  return(errors)
  
}


#' Evaluate ARIMA Model on Multiple Test Sets
#'
#' This function evaluates the performance of an ARIMA model on multiple test sets
#' by comparing the predicted values with the true values. It provides a visual representation
#' of the model's predictions along with confidence intervals for each test set.
#'
#' @param ts Time series object (\code{ts}) representing the entire dataset.
#' @param model Fitted ARIMA model object.
#' @param split_dates Vector of dates used for splitting the time series into train and test sets.
#' @param main Character, the main title of the plots.
#' @param ylab Character, the label for the y-axis of the plots.
#'
#' @return Returns a data frame containing the evaluation errors for each test set.
#'
model_multiple_evaluation <- function(ts, model, split_dates, main, ylab) {
  
  # Dataframe collecting models errors
  errors_df <- data.frame(
    Date = character(),
    MAE  = numeric(), 
    MPE  = numeric(), 
    MSE  = numeric(), 
    RMSE = numeric()
  )
  
  for(date_split in split_dates) {
    
    # Performing split
    tss <- split_ts(ts=ts, date_split=date_split)
    
    # Evaluate the model
    errors <- model_evaluation(
      ts=tss$first,
      test=tss$second,
      model=model,
      main=paste(main, date_split),
      ylab=ylab
    )
    
    # Update dataframe
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


#' Evaluate ARIMA Model One Step Ahead
#'
#' This function evaluates the performance of an ARIMA model on a time series dataset
#' by making one-step-ahead predictions and comparing them with the true values.
#' It provides visual representation of the original time series, predicted values,
#' and prediction intervals for each step.
#'
#' @param ts Time series object (\code{ts}) representing the entire dataset.
#' @param model Fitted ARIMA model object.
#' @param date_start Date, the starting date for the evaluation period.
#' @param date_end Date, the ending date for the evaluation period.
#' @param freq_type Character, the frequency of the time series ("daily" or "weekly").
#' @param main Character, the main title of the plot.
#' @param ylab Character, the label for the y-axis of the plot.
#'
#' @return Returns a time series object containing the predicted values and intervals.
#'
model_one_step_ahead_evaluation <- function(ts, model, date_start, date_end, freq_type, main, ylab) {
  
  if(freq_type=="daily") {
    step <- 1
  } else if(freq_type=="weekly") {
    step <- 7
  } else {
    stop(paste("Invalid freq_type: ", freq_type,". Choose one in {daily, weekly}", sep=""))
  }
  
  # Dataframe collecting prediction info
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
  
  # Iterating through given time window
  while(date_iter <= date_end) {

    # Performing split
    tss <- split_ts(
      ts=ts, 
      date_split=date_iter
    )
    
    # Using only one observation as test
    ts_fit  <- tss$first
    ts_pred <- window(tss$second, start = start(tss$second), end = start(tss$second))

    # Apply the model to the data
    fit <- Arima(ts_fit, model=model)
    
    # Perform 1-step ahead forecast
    forecast <- forecast(fit, h=1)
    
    # Evaluate prediction errors
    errors <- prediction_errors(
      true=as.numeric(ts_pred),
      predicted=as.numeric(forecast$mean)
    )
    
    # Update prediction dataframe
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
  
  # Create a time series of predictions
  start.year <- as.numeric(format(date_start, "%Y"))
  
  if(freq_type=="daily") {
    start.day <- as.numeric(format(date_start, "%j"))
    ts_out <- ts(prediction_df, start=c(start.year, start.day), freq=freq$daily)
  } else {
    start.week <- date_to_weeknumber(date=date_start)
    ts_out <- ts(prediction_df, start=c(start.year, start.week), freq=freq$weekly)
  } # No more need to check for default case
  
  # Plot the original time series and the predicted values
  combined_range <- range(c(
    ts_out[, "True"],
    ts_out[, "Pred"],
    ts_out[, "Low80"],
    ts_out[, "Up80"],
    ts_out[, "Low95"],
    ts_out[, "Up95"]
  ))
  
  # Plot the True label
  plot(
    ts_out[, "True"],
    main=main,
    ylab=ylab,
    col="orangered",
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
  lines(ts_out[, "True"], col="orangered", lwd=2)
  
  # Predicted
  lines(ts_out[, "Pred"], col="#2297e6", lwd=2)
  
  # Legend
  legend(
    "topleft",
    legend = c("True",      "Predicted", "80% CI",  "95% CI" ),
    col    = c("orangered", "#2297e6",   "#b1b5ce", "#dbdbdf"),
    lty    = c(1,           1,           1,         1        ),  
    lwd    = c(2,           2,           2,         2        ),
    cex = 0.8,
    bg = "white"
  )
  
  return(ts_out)
}


#' Analyze Prediction Errors
#'
#' This function analyzes prediction errors from a time series prediction dataset.
#' It plots the prediction errors, highlights the h-lowest and k-highest errors, and prints
#' information about the dates and errors for better insight.
#'
#' @param pred_ts Time series object (\code{ts}) representing the prediction errors.
#' @param error_type Character, the type of prediction error to analyze (e.g., "MAE", "RMSE").
#' @param k Numeric, the number of highest errors to highlight.
#' @param h Numeric, the number of lowest errors to highlight.
#' @param start_date Date, the starting date for the analysis.
#' @param freq_type Character, the frequency of the time series ("daily" or "weekly").
#' @param main Character, the main title of the plot.
#' @param ylab Character, the label for the y-axis of the plot.
#'
errors_analysis <- function(pred_ts, error_type, k, h, start_date, freq_type, main, ylab) {
  
  # Prediction error ts
  pred_ts_err <- pred_ts[, error_type]
  
  # Plot prediction eorrs
  plot(
    pred_ts_err,
    main=main,
    ylab=ylab
  )
  grid()
  
  # Retrieve indexes of k-highest errors
  indices_worst <- rev(tail(order(pred_ts_err), k))
  indices_best <-  rev(head(order(pred_ts_err), h))
  
  # Highlight k-highest errors 
  points(
    time(pred_ts_err)[indices_worst],
    pred_ts_err[indices_worst],
    pch=18,
    cex=2,
    col="orangered"
  )
  points(
    time(pred_ts_err)[indices_best],
    pred_ts_err[indices_best],
    pch=18,
    cex=2,
    col="green3"
  )
  
  # Print the date referring to the k-highest dates
  print("Worst:")
  for(i in indices_worst) {
    
    if(freq_type=="daily") {
      
      print(
        paste(
          "Date: ", float_to_date(time(pred_ts_err)[i]),
          " - ", error_type, ": ", pred_ts_err[i], sep=""
        )
      ) 
      
    } else if(freq_type=="weekly") {
      
      date_week<- as.Date("2022-01-03") + i * 7
      print(
        paste(
          "Week from ", date_week, " to ", date_week+6,
          ": ", pred_ts_err[i], sep=""
        )
      ) 
      
    } else {
      
      stop(paste("Invalid freq_type: ", freq_type,". Choose one in {daily, weekly}", sep=""))
    
    }
    
  }
  
  print("")
  print("Best:")
  # Print the date referring to the k-highest dates
  for(i in indices_best) {
    
    if(freq_type=="daily") {
      
      print(
        paste(
          "Date: ", float_to_date(time(pred_ts_err)[i]),
          " - ", error_type, ": ", pred_ts_err[i], sep=""
        )
      ) 
      
    } else if(freq_type=="weekly") {
      
      date_week<- as.Date("2022-01-03") + i * 7
      print(
        paste(
          "Week from ", date_week, " to ", date_week+6,
          ": ", pred_ts_err[i], sep=""
        )
      ) 
      
    } else {
      
      stop(paste("Invalid freq_type: ", freq_type,". Choose one in {daily, weekly}", sep=""))
      
    }
    
  }
  
}

# --- Model comparison ---


#' Model Comparison Based on Information Criteria
#'
#' This function compares multiple ARIMA models based on information criteria (AIC, AICc, BIC).
#' It plots the forecast for each model and returns a dataframe with information criteria values.
#'
#' @param models List of ARIMA models. Each element of the list is a named list containing the ARIMA fit (\code{fit}),
#'               exogenous regressors (\code{xreg}), forecast horizon (\code{h}), color for plotting (\code{col}), and main title (\code{main}).
#'               The names of the list elements are used as model names.
#' @param ylab Character, the label for the y-axis of the plots.
#'
#' @return Returns a dataframe with model names and corresponding values of AIC, AICc, and BIC.
#'
model_comparison_ic <- function(models, ylab) {
  
  # Dataframe for IC 
  ic_info = data.frame(
    Model = character(),
    AIC   = numeric(), 
    AICc  = numeric(), 
    BIC   = numeric()
  )
  
  # Set output parameters
  par(mfrow=c(length(models), 1))
  
  for(model_name in names(models)) {
    
    # Extract model
    model <- models[[model_name]]
    
    # Plot forecast
    plot(
      forecast(model$fit, xreg=model$xreg, h=model$h),
      main=model$main, ylab=ylab
    )
    
    # Extract IC
    ic <- arima_ic(model$fit)
    
    # Update dataframe
    ic_info <- rbind(ic_info, data.frame(
      Model = model_name,
      AIC  = ic$AIC,
      AICc = ic$AICc,
      BIC  = ic$BIC
    ))
  }
  
  par(mfrow=c(1, 1))
  
  return(ic_info)
  
}


#' Model Comparison Based on Prediction Accuracy
#'
#' This function compares the predictive accuracy of multiple ARIMA models on a test set.
#' It plots the point forecasts for each model and returns a dataframe with prediction errors.
#'
#' @param models List of ARIMA models. Each element of the list is a named list containing the ARIMA fit (\code{fit}),
#'               exogenous regressors (\code{xreg}), forecast horizon (\code{h}), color for plotting (\code{col}), and main title (\code{main}).
#'               The names of the list elements are used as model names.
#' @param train Time series vector representing the training set.
#' @param test Time series vector representing the test set.
#' @param main Character, the main title for the plot.
#' @param ylab Character, the label for the y-axis of the plots.
#' @param plot_train Logical, indicating whether to plot the training set along with the test set forecasts.
#'
#' @return Returns a dataframe with model names and corresponding values of MAE, MPE, MSE, and RMSE.
#'
model_comparison_prediction <- function(models, train, test, main, ylab, lwd=1, plot_train=TRUE) {
  
  # Dataframe for model prediction errors
  errors_df = data.frame(
    Model = character(),
    MAE   = numeric(), 
    MPE   = numeric(), 
    MSE   = numeric(),
    RMSE  = numeric()
  )
  
  predicted = list()
  
  
  for(model_name in names(models)) {
    
    # Extract model name
    model <- models[[model_name]]
    
    # Compute point forecast
    predicted[[model_name]] <- forecast(model$fit, xreg=model$xreg, h=model$h)$mean
    
    # Compute errors
    errors <- prediction_errors(
      true=test, 
      predicted = predicted[[model_name]]
    )
    
    # Update dataframe
    errors_df <- rbind(errors_df, data.frame(
      Model = model_name,
      MAE   = errors$mae, 
      MPE   = errors$mpe, 
      MSE   = errors$mse,
      RMSE  = errors$rmse
    ))
    
  }
  
  # Extract colors
  cols <- lapply(models, function(x) {x$col})
  
  # Add True entry
  cols$True = "steelblue"
  predicted$True <- test
  
  lty <- c(rep('dashed', length(models)), 'solid')
  
  # Add Train entry according to input flag
  if(plot_train) {
    predicted$Train <- train
    cols$Train <- "black"
    lty <- NULL
  }
  
  # Plot ponit forecasts
  plot_multiple_ts(
    ts_list = predicted,
    names = names(predicted),
    colors=cols,
    main = main,
    ylab=ylab,
    lwd=lwd,
    lty=lty
  )
  
  return(errors_df)
  
}
