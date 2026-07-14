# =============================================================================
# 05_forecast_down.R - Forecast Countries with Decreasing Deaths
# COVID-19 Death Forecasting Pipeline
# =============================================================================
# Description: Generate forecasts for countries classified as having
#              decreasing death rates (DOWN category)
# =============================================================================

source("00_config.R")
source("03_data_preprocessing.R")
source("04_time_series_models.R")

print_section("Step 5: Forecast DOWN Countries")

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

print_step("Loading data...")

jh <- readRDS(file.path(OUTPUT_DIR, "jh_with_elasticity.rds"))
countries_down <- readRDS(file.path(OUTPUT_DIR, "countries_down.rds"))

message(paste("Countries to forecast:", length(countries_down)))

# -----------------------------------------------------------------------------
# 2. INITIALIZE RESULT TABLES
# -----------------------------------------------------------------------------

print_step("Initializing result tables...")

forecast_dates <- get_forecast_dates()
h <- length(forecast_dates)

# Tables for cumulative totals and daily differences
final_down <- data.table(jours = forecast_dates)
final_down_dif <- data.table(jours = forecast_dates)

# -----------------------------------------------------------------------------
# 3. FORECAST LOOP
# -----------------------------------------------------------------------------

print_step("Starting forecast loop...")

for (n in countries_down) {
    message(paste("Processing:", n))

    # Get country data
    country_data <- jh[jh$region == n]

    # Filter to training period
    df <- country_data %>% filter(date < TRAIN_END_DATE)

    # Skip if not enough data
    if (nrow(df) < 7) {
        message(paste("  Skipping - insufficient data"))
        next
    }

    # Clean negative values
    df <- clean_negative_deaths(df)

    # Create time series and fit original models (for comparison)
    ts_obj <- create_time_series(df$death_dif)

    # Original models (ETS and ARIMA with damping for down trend)
    results_orig <- list()

    tryCatch(
        {
            results_orig$ets_log <- fit_ets_log(ts_obj$ts_log, h, damped = TRUE)
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

    tryCatch(
        {
            results_orig$arima_log <- fit_arima_log(ts_obj$ts_log, h, approximate = FALSE)
        },
        error = function(e) message(paste("  ARIMA log error:", e$message))
    )

    tryCatch(
        {
            results_orig$arima_diff_log <- fit_arima_diff_log(ts_obj$ts_diff_log,
                df$death_dif[nrow(df)], h,
                approximate = FALSE
            )
        },
        error = function(e) message(paste("  ARIMA diff error:", e$message))
    )

    # Preprocess data with smoothing
    df_trimmed <- trim_leading_zeros(df)
    if (nrow(df_trimmed) <= 3) {
        message(paste("  Skipping - too few data points after trimming"))
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
                    fit_ets_log(ts_mod$ts_log, h, damped = TRUE)
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

        tryCatch(
            {
                results_orig[[paste0("tbats_diff_mod", length(results_orig))]] <-
                    fit_tbats_diff_log(ts_mod$ts_diff_log, df_mod$death_dif[nrow(df_mod)], h)
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

    # Select best prediction (use the one with minimum end value for DOWN countries)
    final_values <- sapply(valid_preds, function(x) {
        pred_dif <- x$predicted_diff
        # For DOWN trend, prefer conservative (lower) forecasts
        sum(pmax(pred_dif, 0))
    })

    best_idx <- which.min(final_values)
    best_pred <- valid_preds[[best_idx]]

    # Get predictions
    pred_dif <- pmax(as.numeric(best_pred$predicted_diff), 0)
    pred_cum <- convert_to_cumulative(pred_dif, df$nb_death[nrow(df)])

    # Add to result tables
    final_down_dif[[n]] <- pred_dif
    final_down[[n]] <- pred_cum

    message(paste("  Final value:", tail(pred_cum, 1)))
}

# -----------------------------------------------------------------------------
# 4. POST-PROCESSING
# -----------------------------------------------------------------------------

print_step("Post-processing forecasts...")

# Replace any remaining negative values with 0
for (col in names(final_down_dif)[-1]) {
    final_down_dif[[col]][final_down_dif[[col]] < 0] <- 0
}

# Apply Belgium-based decay for specific countries (based on news/policy)
if ("Belgium" %in% names(final_down_dif)) {
    reference_countries <- c("Germany", "France", "Switzerland")

    for (country in reference_countries) {
        if (country %in% names(final_down_dif)) {
            for (i in 2:nrow(final_down_dif)) {
                if (final_down_dif$Belgium[[i - 1]] > 0) {
                    ratio <- final_down_dif$Belgium[[i]] / final_down_dif$Belgium[[i - 1]]
                    final_down_dif[[country]][[i]] <- round(ratio * final_down_dif[[country]][[i - 1]])
                }
            }
        }
    }
}

# Recalculate cumulative totals
for (col in names(final_down)[-1]) {
    if (col %in% names(final_down_dif)) {
        # Get initial value
        country_data <- jh[jh$region == col]
        df <- country_data %>% filter(date < TRAIN_END_DATE)
        initial <- df$nb_death[nrow(df)]

        # Recalculate cumulative
        final_down[[col]] <- convert_to_cumulative(final_down_dif[[col]], initial)
    }
}

# -----------------------------------------------------------------------------
# 5. SAVE RESULTS
# -----------------------------------------------------------------------------

print_step("Saving results...")

saveRDS(final_down, file.path(OUTPUT_DIR, "forecast_down_cumulative.rds"))
saveRDS(final_down_dif, file.path(OUTPUT_DIR, "forecast_down_daily.rds"))

message(paste("DOWN countries forecasted:", ncol(final_down) - 1))
message("Forecast DOWN complete!")
