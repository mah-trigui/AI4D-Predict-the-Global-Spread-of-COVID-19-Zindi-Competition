# =============================================================================
# 07_forecast_near_zero.R - Forecast Countries with Near-Zero Deaths
# COVID-19 Death Forecasting Pipeline
# =============================================================================
# Description: Generate forecasts for countries with very few deaths
#              using simpler models
# =============================================================================

source("00_config.R")
source("03_data_preprocessing.R")
source("04_time_series_models.R")

print_section("Step 7: Forecast Near-Zero Countries")

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

print_step("Loading data...")

jh <- readRDS(file.path(OUTPUT_DIR, "jh_with_elasticity.rds"))
countries_down <- readRDS(file.path(OUTPUT_DIR, "countries_down.rds"))
countries_up <- readRDS(file.path(OUTPUT_DIR, "countries_up.rds"))

# Load near-zero from UP processing if available
countries_near_zero_up <- tryCatch(
    {
        readRDS(file.path(OUTPUT_DIR, "countries_near_zero_from_up.rds"))
    },
    error = function(e) character(0)
)

# Identify near-zero countries from remaining
all_countries <- unique(jh$region)
processed_countries <- c(countries_down, countries_up)
remaining_countries <- setdiff(all_countries, processed_countries)

# Find near-zero: countries with <= 7 days of death data
countries_near_zero <- c()
for (n in remaining_countries) {
    df <- jh[jh$region == n & jh$date <= TRAIN_END_DATE]
    df <- clean_negative_deaths(df)
    df <- trim_leading_zeros(df)

    if (nrow(df) <= 7) {
        countries_near_zero <- c(countries_near_zero, n)
    }
}

# Add any from UP processing
countries_near_zero <- unique(c(countries_near_zero, countries_near_zero_up))

message(paste("Near-zero countries to forecast:", length(countries_near_zero)))

# -----------------------------------------------------------------------------
# 2. INITIALIZE RESULT TABLES
# -----------------------------------------------------------------------------

print_step("Initializing result tables...")

forecast_dates <- get_forecast_dates()
h <- length(forecast_dates)

# Tables for cumulative totals and daily differences
final_zero <- data.table(jours = forecast_dates)
final_zero_dif <- data.table(jours = forecast_dates)

# Validation period
validation_dates <- seq(
    from = as.Date("2020-04-14"),
    to = as.Date("2020-04-18"),
    by = "day"
)

# -----------------------------------------------------------------------------
# 3. FORECAST LOOP
# -----------------------------------------------------------------------------

print_step("Starting forecast loop...")

for (n in countries_near_zero) {
    message(paste("Processing:", n))

    # Get country data
    country_data <- jh[jh$region == n]

    # First, validate model selection
    df_val <- country_data %>% filter(date < as.Date("2020-04-14"))

    if (nrow(df_val) < 3) {
        # For countries with minimal data, use flat forecast
        final_zero_dif[[n]] <- rep(0, h)
        final_zero[[n]] <- rep(country_data$nb_death[country_data$date == max(country_data$date[country_data$date <= TRAIN_END_DATE])], h)
        message(paste("  Using flat forecast (minimal data)"))
        next
    }

    # Clean negative values
    df_val <- clean_negative_deaths(df_val)

    # Create time series
    ts_obj <- create_time_series(df_val$death_dif)

    # Fit models for validation
    val_h <- 5
    val_results <- list()

    tryCatch(
        {
            val_results$ets_log <- fit_ets_log(ts_obj$ts_log, val_h)
        },
        error = function(e) NULL
    )

    tryCatch(
        {
            val_results$ets_diff_log <- fit_ets_diff_log(
                ts_obj$ts_diff_log,
                df_val$death_dif[nrow(df_val)], val_h
            )
        },
        error = function(e) NULL
    )

    tryCatch(
        {
            val_results$arima_log <- fit_arima_log(ts_obj$ts_log, val_h, approximate = FALSE)
        },
        error = function(e) NULL
    )

    tryCatch(
        {
            val_results$arima_diff_log <- fit_arima_diff_log(ts_obj$ts_diff_log,
                df_val$death_dif[nrow(df_val)], val_h,
                approximate = FALSE
            )
        },
        error = function(e) NULL
    )

    # Get actual values for validation
    actual_val <- country_data %>%
        filter(date >= as.Date("2020-04-14") & date <= as.Date("2020-04-18"))

    if (nrow(actual_val) == 0) {
        actual_val <- data.table(nb_death = rep(df_val$nb_death[nrow(df_val)], val_h))
    }

    # Select best model based on validation
    valid_val_preds <- val_results[!sapply(val_results, is.null)]

    if (length(valid_val_preds) == 0) {
        # Fallback to flat forecast
        final_zero_dif[[n]] <- rep(0, h)
        df_latest <- country_data %>% filter(date <= TRAIN_END_DATE)
        final_zero[[n]] <- rep(df_latest$nb_death[nrow(df_latest)], h)
        message(paste("  Using flat forecast (no valid models)"))
        next
    }

    # Calculate MAE for each model
    mae_scores <- sapply(valid_val_preds, function(pred) {
        pred_cum <- convert_to_cumulative(pred$predicted_diff, df_val$nb_death[nrow(df_val)])
        calculate_mae(actual_val$nb_death, pred_cum[seq_along(actual_val$nb_death)])
    })

    best_model_idx <- which.min(mae_scores)
    best_model_name <- names(valid_val_preds)[best_model_idx]

    # Now fit on full training data
    df <- country_data %>% filter(date < TRAIN_END_DATE)
    df <- clean_negative_deaths(df)

    ts_obj_full <- create_time_series(df$death_dif)

    # Fit best model type on full data
    best_result <- switch(best_model_name,
        ets_log = tryCatch(fit_ets_log(ts_obj_full$ts_log, h), error = function(e) NULL),
        ets_diff_log = tryCatch(fit_ets_diff_log(
            ts_obj_full$ts_diff_log,
            df$death_dif[nrow(df)], h
        ), error = function(e) NULL),
        arima_log = tryCatch(fit_arima_log(ts_obj_full$ts_log, h, approximate = FALSE), error = function(e) NULL),
        arima_diff_log = tryCatch(fit_arima_diff_log(ts_obj_full$ts_diff_log,
            df$death_dif[nrow(df)], h,
            approximate = FALSE
        ), error = function(e) NULL),
        NULL
    )

    if (is.null(best_result)) {
        # Fallback to flat forecast
        final_zero_dif[[n]] <- rep(0, h)
        final_zero[[n]] <- rep(df$nb_death[nrow(df)], h)
        message(paste("  Using flat forecast (model failed)"))
        next
    }

    # Get predictions
    pred_dif <- pmax(as.numeric(best_result$predicted_diff), 0)
    pred_cum <- convert_to_cumulative(pred_dif, df$nb_death[nrow(df)])

    # Add to result tables
    final_zero_dif[[n]] <- pred_dif
    final_zero[[n]] <- pred_cum

    message(paste("  Best model:", best_model_name, "- Final value:", tail(pred_cum, 1)))
}

# -----------------------------------------------------------------------------
# 4. POST-PROCESSING
# -----------------------------------------------------------------------------

print_step("Post-processing forecasts...")

# Replace any remaining negative values with 0
for (col in names(final_zero_dif)[-1]) {
    final_zero_dif[[col]][final_zero_dif[[col]] < 0] <- 0
}

# -----------------------------------------------------------------------------
# 5. SAVE RESULTS
# -----------------------------------------------------------------------------

print_step("Saving results...")

saveRDS(final_zero, file.path(OUTPUT_DIR, "forecast_zero_cumulative.rds"))
saveRDS(final_zero_dif, file.path(OUTPUT_DIR, "forecast_zero_daily.rds"))
saveRDS(countries_near_zero, file.path(OUTPUT_DIR, "countries_near_zero.rds"))

message(paste("Near-zero countries forecasted:", ncol(final_zero) - 1))
message("Forecast Near-Zero complete!")
