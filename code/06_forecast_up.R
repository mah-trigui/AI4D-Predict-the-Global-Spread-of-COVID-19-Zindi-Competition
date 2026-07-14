# =============================================================================
# 06_forecast_up.R - Forecast Countries with Increasing Deaths
# COVID-19 Death Forecasting Pipeline
# =============================================================================
# Description: Generate forecasts for countries classified as having
#              increasing death rates (UP category)
# =============================================================================

source("00_config.R")
source("03_data_preprocessing.R")
source("04_time_series_models.R")

print_section("Step 6: Forecast UP Countries")

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

print_step("Loading data...")

jh <- readRDS(file.path(OUTPUT_DIR, "jh_with_elasticity.rds"))
countries_up <- readRDS(file.path(OUTPUT_DIR, "countries_up.rds"))
countries_down <- readRDS(file.path(OUTPUT_DIR, "countries_down.rds"))

# Remove any countries already in DOWN category
countries_up <- setdiff(countries_up, countries_down)

message(paste("Countries to forecast:", length(countries_up)))

# -----------------------------------------------------------------------------
# 2. INITIALIZE RESULT TABLES
# -----------------------------------------------------------------------------

print_step("Initializing result tables...")

forecast_dates <- get_forecast_dates()
h <- length(forecast_dates)

# Tables for cumulative totals and daily differences
final_up <- data.table(jours = forecast_dates)
final_up_dif <- data.table(jours = forecast_dates)

# Track countries with near-zero deaths
countries_near_zero <- list()
near_zero_idx <- 1

# -----------------------------------------------------------------------------
# 3. FORECAST LOOP
# -----------------------------------------------------------------------------

print_step("Starting forecast loop...")

for (n in countries_up) {
    message(paste("Processing:", n))

    # Get country data
    country_data <- jh[jh$region == n]

    # Filter to training period (slightly earlier for UP countries)
    df <- country_data %>% filter(date <= as.Date("2020-04-11"))

    # Skip if not enough data
    if (nrow(df) < 7) {
        message(paste("  Skipping - insufficient data"))
        next
    }

    # Clean negative values
    df <- clean_negative_deaths(df)

    # Check for near-zero deaths
    if (df$nb_death[nrow(df)] == 0 || df$nb_death[nrow(df) - 2] == 0) {
        message(paste("  Marking as near-zero"))
        countries_near_zero[[near_zero_idx]] <- n
        near_zero_idx <- near_zero_idx + 1
        next
    }

    # Create time series and fit original models
    ts_obj <- create_time_series(df$death_dif)

    results_orig <- list()

    # ETS models (without damping for upward trend)
    tryCatch(
        {
            results_orig$ets_log <- fit_ets_log(ts_obj$ts_log, h, damped = FALSE)
        },
        error = function(e) message(paste("  ETS log error:", e$message))
    )

    tryCatch(
        {
            results_orig$ets_diff_log <- fit_ets_diff_log(
                ts_obj$ts_diff_log,
                df$death_dif[nrow(df)], h
            )
        },
        error = function(e) message(paste("  ETS diff error:", e$message))
    )

    # ARIMA models (with approximation for speed)
    tryCatch(
        {
            results_orig$arima_log <- fit_arima_log(ts_obj$ts_log, h, approximate = TRUE)
        },
        error = function(e) message(paste("  ARIMA log error:", e$message))
    )

    tryCatch(
        {
            results_orig$arima_diff_log <- fit_arima_diff_log(ts_obj$ts_diff_log,
                df$death_dif[nrow(df)], h,
                approximate = TRUE
            )
        },
        error = function(e) message(paste("  ARIMA diff error:", e$message))
    )

    # Preprocess data with smoothing
    df_trimmed <- trim_leading_zeros(df)
    if (nrow(df_trimmed) <= 3) {
        message(paste("  Skipping - too few data points after trimming"))
        countries_near_zero[[near_zero_idx]] <- n
        near_zero_idx <- near_zero_idx + 1
        next
    }

    df_trimmed <- fix_consecutive_same_values(df_trimmed)

    # Create smoothed versions
    df_mod1 <- smooth_abnormal_spikes(df_trimmed, threshold = 0.5, multiplier = 2)
    df_mod2 <- smooth_abnormal_spikes(df_trimmed, threshold = 0.33, multiplier = 3)

    # Fit models on modified data
    for (df_mod in list(df_mod1, df_mod2)) {
        ts_mod <- create_time_series(df_mod$death_dif)

        tryCatch(
            {
                results_orig[[paste0("ets_log_mod", length(results_orig))]] <-
                    fit_ets_log(ts_mod$ts_log, h, damped = FALSE)
            },
            error = function(e) NULL
        )

        tryCatch(
            {
                results_orig[[paste0("ets_diff_mod", length(results_orig))]] <-
                    fit_ets_diff_log(ts_mod$ts_diff_log, df_mod$death_dif[nrow(df_mod)], h)
            },
            error = function(e) NULL
        )

        tryCatch(
            {
                results_orig[[paste0("tbats_log_mod", length(results_orig))]] <-
                    fit_tbats_log(ts_mod$ts_log, h)
            },
            error = function(e) NULL
        )
    }

    # Collect all predictions
    valid_preds <- results_orig[!sapply(results_orig, is.null)]

    if (length(valid_preds) == 0) {
        message(paste("  No valid predictions"))
        next
    }

    # For UP trend, select based on reasonable growth
    final_values <- sapply(valid_preds, function(x) {
        pred_dif <- x$predicted_diff
        sum(pmax(pred_dif, 0))
    })

    # Use median prediction for robustness
    median_idx <- which.min(abs(final_values - median(final_values)))
    best_pred <- valid_preds[[median_idx]]

    # Get predictions
    pred_dif <- pmax(as.numeric(best_pred$predicted_diff), 0)
    pred_cum <- convert_to_cumulative(pred_dif, df$nb_death[nrow(df)])

    # Add to result tables
    final_up_dif[[n]] <- pred_dif
    final_up[[n]] <- pred_cum

    message(paste("  Final value:", tail(pred_cum, 1)))
}

# -----------------------------------------------------------------------------
# 4. POST-PROCESSING
# -----------------------------------------------------------------------------

print_step("Post-processing forecasts...")

# Replace any remaining negative values with 0
for (col in names(final_up_dif)[-1]) {
    final_up_dif[[col]][final_up_dif[[col]] < 0] <- 0
}

# -----------------------------------------------------------------------------
# 5. SAVE RESULTS
# -----------------------------------------------------------------------------

print_step("Saving results...")

saveRDS(final_up, file.path(OUTPUT_DIR, "forecast_up_cumulative.rds"))
saveRDS(final_up_dif, file.path(OUTPUT_DIR, "forecast_up_daily.rds"))

# Save near-zero countries for separate processing
countries_near_zero <- unlist(countries_near_zero)
saveRDS(countries_near_zero, file.path(OUTPUT_DIR, "countries_near_zero_from_up.rds"))

message(paste("UP countries forecasted:", ncol(final_up) - 1))
message(paste("Near-zero countries identified:", length(countries_near_zero)))
message("Forecast UP complete!")
