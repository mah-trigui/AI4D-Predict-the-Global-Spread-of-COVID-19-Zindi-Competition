# =============================================================================
# 04_time_series_models.R - Time Series Forecasting Models
# COVID-19 Death Forecasting Pipeline
# =============================================================================
# Description: Functions to create time series and fit forecasting models
#              (ETS, ARIMA, TBATS) with different transformations
# =============================================================================

source("00_config.R")

print_section("Step 4: Time Series Model Functions")

# -----------------------------------------------------------------------------
# 1. CREATE TIME SERIES OBJECTS
# -----------------------------------------------------------------------------

#' Create time series objects with different transformations
create_time_series <- function(death_dif) {
  list(
    # Log transformation of daily deaths
    ts_log = ts(log(death_dif + 1), freq = 365.25),
    # Differenced log transformation
    ts_diff_log = ts(diff(log(death_dif + 1)), freq = 365.25)
  )
}

# -----------------------------------------------------------------------------
# 2. ETS MODELS
# -----------------------------------------------------------------------------

#' Fit ETS model on log-transformed data (Model 2)
fit_ets_log <- function(ts_data, h, damped = FALSE, additive_only = FALSE) {
  if (damped) {
    model <- forecast::ets(ts_data, opt.crit = 'mae', damped = TRUE,
                           allow.multiplicative.trend = FALSE, additive.only = TRUE)
  } else {
    model <- forecast::ets(ts_data, opt.crit = 'mae')
  }

  fc <- forecast::forecast(model, h = h)

  # Convert back from log scale
  pred_dif <- round(exp(fc$mean)) - 1

  return(list(
    model = model,
    forecast = fc,
    predicted_diff = as.numeric(pred_dif)
  ))
}

#' Fit ETS model on differenced log data (Model 4)
fit_ets_diff_log <- function(ts_data, last_death_dif, h, additive_only = FALSE) {
  if (additive_only) {
    model <- forecast::ets(ts_data, opt.crit = 'mae',
                           allow.multiplicative.trend = FALSE, additive.only = TRUE)
  } else {
    model <- forecast::ets(ts_data, opt.crit = 'mae')
  }

  fc <- forecast::forecast(model, h = h)

  # Convert back: integrate and exponentiate
  pred <- as.numeric(fc$mean)
  pred[1] <- pred[1] + log(last_death_dif + 1)
  pred_cumsum <- cumsum(pred)
  pred_dif <- round(exp(pred_cumsum)) - 1

  return(list(
    model = model,
    forecast = fc,
    predicted_diff = pred_dif
  ))
}

# -----------------------------------------------------------------------------
# 3. ARIMA MODELS
# -----------------------------------------------------------------------------

#' Fit ARIMA model on log-transformed data (Model 2)
fit_arima_log <- function(ts_data, h, approximate = FALSE) {
  model <- forecast::auto.arima(ts_data,
                                approximation = approximate,
                                seasonal = FALSE,
                                allowdrift = TRUE,
                                stepwise = !approximate,
                                trace = FALSE)

  fc <- forecast::forecast(model, h = h)

  # Convert back from log scale
  pred_dif <- round(exp(fc$mean)) - 1

  return(list(
    model = model,
    forecast = fc,
    predicted_diff = as.numeric(pred_dif)
  ))
}

#' Fit ARIMA model on differenced log data (Model 4)
fit_arima_diff_log <- function(ts_data, last_death_dif, h, approximate = FALSE) {
  model <- forecast::auto.arima(ts_data,
                                approximation = approximate,
                                seasonal = FALSE,
                                allowdrift = TRUE,
                                stepwise = !approximate,
                                trace = FALSE)

  fc <- forecast::forecast(model, h = h)

  # Convert back
  pred <- as.numeric(fc$mean)
  pred[1] <- pred[1] + log(last_death_dif + 1)
  pred_cumsum <- cumsum(pred)
  pred_dif <- round(exp(pred_cumsum)) - 1

  return(list(
    model = model,
    forecast = fc,
    predicted_diff = pred_dif
  ))
}

# -----------------------------------------------------------------------------
# 4. TBATS MODELS
# -----------------------------------------------------------------------------

#' Fit TBATS model on log-transformed data
fit_tbats_log <- function(ts_data, h) {
  model <- forecast::tbats(ts_data,
                           seasonal.periods = NULL,
                           use.trend = NULL,
                           use.box.cox = NULL)

  fc <- forecast::forecast(model, h = h)

  # Convert back from log scale
  pred_dif <- round(exp(fc$mean)) - 1

  return(list(
    model = model,
    forecast = fc,
    predicted_diff = as.numeric(pred_dif)
  ))
}

#' Fit TBATS model on differenced log data
fit_tbats_diff_log <- function(ts_data, last_death_dif, h) {
  model <- forecast::tbats(ts_data,
                           seasonal.periods = NULL,
                           use.trend = FALSE)

  fc <- forecast::forecast(model, h = h)

  # Convert back
  pred <- as.numeric(fc$mean)
  pred[1] <- pred[1] + log(last_death_dif + 1)
  pred_cumsum <- cumsum(pred)
  pred_dif <- round(exp(pred_cumsum)) - 1

  return(list(
    model = model,
    forecast = fc,
    predicted_diff = pred_dif
  ))
}

# -----------------------------------------------------------------------------
# 5. MODEL ENSEMBLE
# -----------------------------------------------------------------------------

#' Fit all models and return predictions
fit_all_models <- function(df, h, damped = FALSE, approximate = FALSE) {
  ts_obj <- create_time_series(df$death_dif)
  last_death_dif <- df$death_dif[nrow(df)]
  last_nb_death <- df$nb_death[nrow(df)]

  results <- list()

  # ETS models
  tryCatch({
    results$ets_log <- fit_ets_log(ts_obj$ts_log, h, damped)
  }, error = function(e) NULL)

  tryCatch({
    results$ets_diff_log <- fit_ets_diff_log(ts_obj$ts_diff_log, last_death_dif, h)
  }, error = function(e) NULL)

  # ARIMA models
  tryCatch({
    results$arima_log <- fit_arima_log(ts_obj$ts_log, h, approximate)
  }, error = function(e) NULL)

  tryCatch({
    results$arima_diff_log <- fit_arima_diff_log(ts_obj$ts_diff_log, last_death_dif, h, approximate)
  }, error = function(e) NULL)

  # TBATS models
  tryCatch({
    results$tbats_log <- fit_tbats_log(ts_obj$ts_log, h)
  }, error = function(e) NULL)

  tryCatch({
    results$tbats_diff_log <- fit_tbats_diff_log(ts_obj$ts_diff_log, last_death_dif, h)
  }, error = function(e) NULL)

  return(results)
}

# -----------------------------------------------------------------------------
# 6. CONVERT PREDICTIONS TO CUMULATIVE
# -----------------------------------------------------------------------------

#' Convert daily difference predictions to cumulative totals
convert_to_cumulative <- function(pred_dif, initial_value) {
  pred_dif <- pmax(pred_dif, 0)  # Ensure non-negative
  pred_dif[1] <- pred_dif[1] + initial_value
  cumsum(pred_dif)
}

# -----------------------------------------------------------------------------
# 7. SELECT BEST MODEL
# -----------------------------------------------------------------------------

#' Select best model based on validation MAE
select_best_model <- function(predictions_list, actual_values) {
  if (length(predictions_list) == 0) return(NULL)

  mae_scores <- sapply(predictions_list, function(pred) {
    if (is.null(pred)) return(Inf)
    MLmetrics::MAE(actual_values, pred$predicted_diff[1:length(actual_values)])
  })

  best_idx <- which.min(mae_scores)

  return(list(
    best_model = names(predictions_list)[best_idx],
    best_prediction = predictions_list[[best_idx]],
    mae_scores = mae_scores
  ))
}

message("Time series model functions loaded!")
