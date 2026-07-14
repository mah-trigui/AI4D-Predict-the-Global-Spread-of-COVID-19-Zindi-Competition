# =============================================================================
# 08_forecast_rest.R - Forecast Remaining Countries
# COVID-19 Death Forecasting Pipeline
# =============================================================================
# Description: Generate forecasts for countries not in DOWN, UP, or NEAR_ZERO
#              categories using validation-based model selection
# =============================================================================

source("00_config.R")
source("03_data_preprocessing.R")
source("04_time_series_models.R")

print_section("Step 8: Forecast Remaining Countries")

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

print_step("Loading data...")

jh <- readRDS(file.path(OUTPUT_DIR, "jh_with_elasticity.rds"))
countries_down <- readRDS(file.path(OUTPUT_DIR, "countries_down.rds"))
countries_up <- readRDS(file.path(OUTPUT_DIR, "countries_up.rds"))
countries_near_zero <- readRDS(file.path(OUTPUT_DIR, "countries_near_zero.rds"))

# Get remaining countries
all_countries <- unique(jh$region)
processed <- c(countries_down, countries_up, countries_near_zero)
countries_rest <- setdiff(all_countries, processed)

message(paste("Remaining countries to forecast:", length(countries_rest)))

# -----------------------------------------------------------------------------
# 2. INITIALIZE RESULT TABLES
# -----------------------------------------------------------------------------

print_step("Initializing result tables...")

forecast_dates <- get_forecast_dates()
h <- length(forecast_dates)

# Tables for cumulative totals and daily differences
final_rest <- data.table(jours = forecast_dates)
final_rest_dif <- data.table(jours = forecast_dates)

# Validation period
validation_dates <- seq(
    from = as.Date("2020-04-12"),
    to = as.Date("2020-04-18"),
    by = "day"
)
val_h <- length(validation_dates)

# Track countries with <= 3 days of data
list_3 <- c()

# -----------------------------------------------------------------------------
# 3. FORECAST LOOP
# -----------------------------------------------------------------------------

print_step("Starting forecast loop...")

for (n in countries_rest) {
    message(paste("Processing:", n))

    # Get country data
    country_data <- jh[jh$region == n]

    # Split for validation
    df_val <- country_data %>% filter(date < as.Date("2020-04-12"))
    df_actual <- country_data %>% filter(date >= as.Date("2020-04-12") &
        date <= as.Date("2020-04-18"))

    if (nrow(df_val) < 5) {
        message(paste("  Skipping - insufficient validation data"))
        list_3 <- c(list_3, n)
        next
    }

    # Clean data
    df_val <- clean_negative_deaths(df_val)

    # Create time series
    ts_obj <- create_time_series(df_val$death_dif)

    # Fit multiple models for validation
    val_results <- list()

    # Original data models
    tryCatch(
        {
            val_results$ets_log_orig <- fit_ets_log(ts_obj$ts_log, val_h)
        },
        error = function(e) NULL
    )

    tryCatch(
        {
            val_results$ets_diff_orig <- fit_ets_diff_log(
                ts_obj$ts_diff_log,
                df_val$death_dif[nrow(df_val)], val_h
            )
        },
        error = function(e) NULL
    )

    # Preprocess and fit on modified data
    df_trimmed <- trim_leading_zeros(df_val)
    if (nrow(df_trimmed) <= 3) {
        message(paste("  Marking as insufficient data"))
        list_3 <- c(list_3, n)
        next
    }

    df_trimmed <- fix_consecutive_same_values(df_trimmed)
    df_mod <- smooth_abnormal_spikes(df_trimmed, threshold = 0.5, multiplier = 2)

    ts_mod <- create_time_series(df_mod$death_dif)

    tryCatch(
        {
            val_results$ets_log_mod <- fit_ets_log(ts_mod$ts_log, val_h)
        },
        error = function(e) NULL
    )

    tryCatch(
        {
            val_results$ets_diff_mod <- fit_ets_diff_log(
                ts_mod$ts_diff_log,
                df_mod$death_dif[nrow(df_mod)], val_h
            )
        },
        error = function(e) NULL
    )

    tryCatch(
        {
            val_results$tbats_log <- fit_tbats_log(ts_obj$ts_log, val_h)
        },
        error = function(e) NULL
    )

    tryCatch(
        {
            val_results$tbats_log_mod <- fit_tbats_log(ts_mod$ts_log, val_h)
        },
        error = function(e) NULL
    )

    # Filter valid results
    valid_results <- val_results[!sapply(val_results, is.null)]

    if (length(valid_results) == 0) {
        message(paste("  No valid models"))
        list_3 <- c(list_3, n)
        next
    }

    # Calculate validation MAE for each model
    if (nrow(df_actual) > 0) {
        mae_scores <- sapply(valid_results, function(pred) {
            pred_cum <- convert_to_cumulative(pred$predicted_diff, df_val$nb_death[nrow(df_val)])
            pred_cum_subset <- pred_cum[seq_len(min(length(pred_cum), nrow(df_actual)))]
            actual_subset <- df_actual$nb_death[seq_along(pred_cum_subset)]
            calculate_mae(actual_subset, pred_cum_subset)
        })

        best_model_idx <- which.min(mae_scores)
    } else {
        # If no validation data, use first valid model
        best_model_idx <- 1
    }

    best_model_name <- names(valid_results)[best_model_idx]

    # Now fit on full training data
    df <- country_data %>% filter(date < TRAIN_END_DATE)
    df <- clean_negative_deaths(df)
    df_trimmed <- trim_leading_zeros(df)
    df_trimmed <- fix_consecutive_same_values(df_trimmed)
    df_mod <- smooth_abnormal_spikes(df_trimmed, threshold = 0.5, multiplier = 2)

    ts_obj_full <- create_time_series(df$death_dif)
    ts_mod_full <- create_time_series(df_mod$death_dif)

    # Fit best model type on full data
    final_result <- NULL

    if (grepl("mod", best_model_name)) {
        ts_use <- ts_mod_full
        last_dif <- df_mod$death_dif[nrow(df_mod)]
    } else {
        ts_use <- ts_obj_full
        last_dif <- df$death_dif[nrow(df)]
    }

    if (grepl("ets_log", best_model_name)) {
        final_result <- tryCatch(fit_ets_log(ts_use$ts_log, h), error = function(e) NULL)
    } else if (grepl("ets_diff", best_model_name)) {
        final_result <- tryCatch(fit_ets_diff_log(ts_use$ts_diff_log, last_dif, h), error = function(e) NULL)
    } else if (grepl("tbats", best_model_name)) {
        final_result <- tryCatch(fit_tbats_log(ts_use$ts_log, h), error = function(e) NULL)
    }

    if (is.null(final_result)) {
        # Fallback to ETS
        final_result <- tryCatch(fit_ets_log(ts_obj_full$ts_log, h), error = function(e) NULL)
    }

    if (is.null(final_result)) {
        message(paste("  Failed to fit model"))
        list_3 <- c(list_3, n)
        next
    }

    # Get predictions
    pred_dif <- pmax(as.numeric(final_result$predicted_diff), 0)
    pred_cum <- convert_to_cumulative(pred_dif, df$nb_death[nrow(df)])

    # Add to result tables
    final_rest_dif[[n]] <- pred_dif
    final_rest[[n]] <- pred_cum

    message(paste("  Best model:", best_model_name, "- Final value:", tail(pred_cum, 1)))
}

# -----------------------------------------------------------------------------
# 4. POST-PROCESSING
# -----------------------------------------------------------------------------

print_step("Post-processing forecasts...")

# Replace any remaining negative values with 0
for (col in names(final_rest_dif)[-1]) {
    final_rest_dif[[col]][final_rest_dif[[col]] < 0] <- 0
}

# -----------------------------------------------------------------------------
# 5. SAVE RESULTS
# -----------------------------------------------------------------------------

print_step("Saving results...")

saveRDS(final_rest, file.path(OUTPUT_DIR, "forecast_rest_cumulative.rds"))
saveRDS(final_rest_dif, file.path(OUTPUT_DIR, "forecast_rest_daily.rds"))
saveRDS(list_3, file.path(OUTPUT_DIR, "countries_insufficient_data.rds"))

message(paste("REST countries forecasted:", ncol(final_rest) - 1))
message(paste("Countries with insufficient data:", length(list_3)))
message("Forecast REST complete!")
