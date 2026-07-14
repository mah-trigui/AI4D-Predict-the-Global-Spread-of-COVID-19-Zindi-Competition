# =============================================================================
# 09_merge_and_submit.R - Merge All Forecasts and Generate Submission
# COVID-19 Death Forecasting Pipeline
# =============================================================================
# Description: Merge all country forecasts and generate final submission file
# =============================================================================

source("00_config.R")

print_section("Step 9: Merge and Generate Submission")

# -----------------------------------------------------------------------------
# 1. LOAD ALL FORECASTS
# -----------------------------------------------------------------------------

print_step("Loading all forecasts...")

jh <- readRDS(file.path(OUTPUT_DIR, "jh_with_elasticity.rds"))
ref_zindi <- readRDS(file.path(OUTPUT_DIR, "ref_zindi.rds"))

# Load cumulative forecasts
final_down <- readRDS(file.path(OUTPUT_DIR, "forecast_down_cumulative.rds"))
final_up <- readRDS(file.path(OUTPUT_DIR, "forecast_up_cumulative.rds"))
final_zero <- readRDS(file.path(OUTPUT_DIR, "forecast_zero_cumulative.rds"))
final_rest <- readRDS(file.path(OUTPUT_DIR, "forecast_rest_cumulative.rds"))

# Load daily difference forecasts
final_down_dif <- readRDS(file.path(OUTPUT_DIR, "forecast_down_daily.rds"))
final_up_dif <- readRDS(file.path(OUTPUT_DIR, "forecast_up_daily.rds"))
final_zero_dif <- readRDS(file.path(OUTPUT_DIR, "forecast_zero_daily.rds"))
final_rest_dif <- readRDS(file.path(OUTPUT_DIR, "forecast_rest_daily.rds"))

message(paste("DOWN countries:", ncol(final_down) - 1))
message(paste("UP countries:", ncol(final_up) - 1))
message(paste("ZERO countries:", ncol(final_zero) - 1))
message(paste("REST countries:", ncol(final_rest) - 1))

# -----------------------------------------------------------------------------
# 2. POST-PROCESS DAILY DIFFERENCES
# -----------------------------------------------------------------------------

print_step("Post-processing daily differences...")

# Replace negative values with 0
clean_negatives <- function(dt) {
    for (col in names(dt)[-1]) {
        dt[[col]][dt[[col]] < 0] <- 0
    }
    return(dt)
}

final_down_dif <- clean_negatives(final_down_dif)
final_up_dif <- clean_negatives(final_up_dif)
final_zero_dif <- clean_negatives(final_zero_dif)
final_rest_dif <- clean_negatives(final_rest_dif)

# -----------------------------------------------------------------------------
# 3. RECALCULATE CUMULATIVE FROM DAILY
# -----------------------------------------------------------------------------

print_step("Recalculating cumulative totals from daily differences...")

recalculate_cumulative <- function(dif_dt, jh_data, train_end = TRAIN_END_DATE) {
    cum_dt <- data.table(jours = dif_dt$jours)

    for (col in names(dif_dt)[-1]) {
        # Get initial value
        country_data <- jh_data[jh_data$region == col & jh_data$date <= train_end]
        if (nrow(country_data) > 0) {
            initial_value <- country_data$nb_death[nrow(country_data)]
        } else {
            initial_value <- 0
        }

        # Calculate cumulative
        dif_values <- dif_dt[[col]]
        dif_values[1] <- dif_values[1] + initial_value
        cum_dt[[col]] <- cumsum(dif_values)
    }

    return(cum_dt)
}

f_down <- recalculate_cumulative(final_down_dif, jh)
f_up <- recalculate_cumulative(final_up_dif, jh)
f_zero <- recalculate_cumulative(final_zero_dif, jh)
f_rest <- recalculate_cumulative(final_rest_dif, jh)

# -----------------------------------------------------------------------------
# 4. MERGE ALL FORECASTS
# -----------------------------------------------------------------------------

print_step("Merging all forecasts...")

# Create forecast dates column
jour <- data.table(jour = get_forecast_dates())

# Merge all (removing duplicate jours column)
final <- cbind(
    f_down[, -"jours"],
    f_up[, -"jours"],
    f_zero[, -"jours"],
    f_rest[, -"jours"]
)
final <- cbind(jour, final)

message(paste("Total countries in forecast:", ncol(final) - 1))

# -----------------------------------------------------------------------------
# 5. RESHAPE TO SUBMISSION FORMAT
# -----------------------------------------------------------------------------

print_step("Reshaping to submission format...")

# Melt to long format
forecast_long <- melt(final, id = c("jour"))
names(forecast_long) <- c("date", "region", "target")

# Fix special characters in region names
forecast_long$region <- as.character(forecast_long$region)
forecast_long$region[grepl("Ivoire", forecast_long$region)] <- "Côte d'Ivoire"

# Convert date
forecast_long$date <- as.Date(forecast_long$date)

# Order by region and date
forecast_long <- forecast_long[order(forecast_long$region, forecast_long$date), ]

# Filter to submission period
forecast_long <- forecast_long[forecast_long$date < FORECAST_END_DATE, ]

message(paste("Forecast rows:", nrow(forecast_long)))

# -----------------------------------------------------------------------------
# 6. ADD HISTORICAL DATA
# -----------------------------------------------------------------------------

print_step("Adding historical data...")

# Get historical data from JH
historical_start <- as.Date("2020-03-06")
historical_end <- FORECAST_START_DATE

historical <- jh[
    jh$date >= historical_start & jh$date < historical_end,
    c("region", "date", "nb_death")
]
names(historical)[3] <- "target"

# Fix special characters
historical$region[grepl("Ivoire", historical$region)] <- "Côte d'Ivoire"

# Combine historical and forecast
full_data <- rbind(historical, forecast_long)
full_data$region <- as.character(full_data$region)
full_data$date <- as.Date(full_data$date)
full_data <- full_data[order(full_data$region, full_data$date), ]

message(paste("Full data rows:", nrow(full_data)))

# -----------------------------------------------------------------------------
# 7. CREATE SUBMISSION TEMPLATE
# -----------------------------------------------------------------------------

print_step("Creating submission template...")

# Create all date-region combinations
submission_dates <- seq(
    from = as.Date("2020-03-06"),
    to = as.Date("2020-06-07"),
    by = "day"
)

# Fix Zindi reference names
ref_zindi$region[grepl("Ivoire", ref_zindi$region)] <- "Côte d'Ivoire"

# Create cross join of regions and dates
template <- data.table(
    expand.grid(
        region = ref_zindi$region,
        date = submission_dates,
        stringsAsFactors = FALSE
    )
)

message(paste("Template rows:", nrow(template)))
message(paste("Unique regions in template:", length(unique(template$region))))

# -----------------------------------------------------------------------------
# 8. MERGE WITH FORECASTS
# -----------------------------------------------------------------------------

print_step("Merging with forecasts...")

submission <- template %>%
    left_join(full_data, by = c("region", "date"))

# Fill missing targets with 0
submission$target[is.na(submission$target)] <- 0

# -----------------------------------------------------------------------------
# 9. FORMAT SUBMISSION
# -----------------------------------------------------------------------------

print_step("Formatting submission...")

# Create Territory X Date column
submission$month <- month(submission$date)
submission$day <- mday(submission$date)
submission$`Territory X Date` <- paste0(
    submission$region, " X ",
    submission$month, "/",
    submission$day, "/20"
)

# Select final columns
submission_final <- submission[, c("Territory X Date", "target")]

# Ensure target is integer
submission_final$target <- as.integer(round(submission_final$target))

# Replace any remaining NA with 0
submission_final$target[is.na(submission_final$target)] <- 0

# -----------------------------------------------------------------------------
# 10. SAVE SUBMISSION
# -----------------------------------------------------------------------------

print_step("Saving submission...")

# Save to output directory
submission_file <- file.path(OUTPUT_DIR, "submission.csv")
write.csv(submission_final, submission_file, quote = FALSE, row.names = FALSE)

message(paste("Submission saved to:", submission_file))
message(paste("Total rows:", nrow(submission_final)))
message(paste("Total unique territories:", length(unique(submission$region))))

# Summary statistics
message("\nSubmission summary:")
message(paste("Min target:", min(submission_final$target)))
message(paste("Max target:", max(submission_final$target)))
message(paste("Mean target:", round(mean(submission_final$target), 2)))

# Also save to working directory
write.csv(submission_final, "submission_final.csv", quote = FALSE, row.names = FALSE)
message("Also saved to: submission_final.csv")

print_step("Submission generation complete!")
