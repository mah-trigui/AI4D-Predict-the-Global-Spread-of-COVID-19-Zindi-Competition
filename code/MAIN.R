# =============================================================================
# MAIN.R - Main Pipeline Orchestration Script
# COVID-19 Death Forecasting Pipeline
# =============================================================================
# Description: Main entry point to run the complete forecasting pipeline.
#              This script orchestrates all processing steps from data loading
#              to submission generation.
# =============================================================================

cat("
================================================================================
 COVID-19 DEATH FORECASTING PIPELINE
================================================================================
 Competition: Zindi COVID-19 Global Forecasting
 Data Source: Johns Hopkins University CSSE

 This pipeline forecasts cumulative COVID-19 deaths for multiple countries
 using time series models (ETS, ARIMA, TBATS) with ensemble selection.
================================================================================
\n")

# -----------------------------------------------------------------------------
# CONFIGURATION
# -----------------------------------------------------------------------------

# Set this to TRUE to run the full pipeline
RUN_FULL_PIPELINE <- TRUE

# Individual step controls (only used if RUN_FULL_PIPELINE is FALSE)
RUN_STEPS <- list(
    config = TRUE,
    data_loading = TRUE,
    elasticity = TRUE,
    forecast_down = TRUE,
    forecast_up = TRUE,
    forecast_near_zero = TRUE,
    forecast_rest = TRUE,
    merge_submit = TRUE
)

# -----------------------------------------------------------------------------
# UTILITY FUNCTION
# -----------------------------------------------------------------------------

run_script <- function(script_name, description) {
    cat(paste0("\n", strrep("=", 70), "\n"))
    cat(paste0(" Running: ", description, "\n"))
    cat(paste0(" Script: ", script_name, "\n"))
    cat(paste0(strrep("=", 70), "\n\n"))

    start_time <- Sys.time()

    tryCatch(
        {
            source(script_name, local = FALSE)
            end_time <- Sys.time()
            duration <- difftime(end_time, start_time, units = "mins")
            cat(paste0(
                "\n✓ Completed: ", description,
                " (", round(duration, 2), " minutes)\n"
            ))
            return(TRUE)
        },
        error = function(e) {
            cat(paste0("\n✗ Error in ", script_name, ": ", e$message, "\n"))
            return(FALSE)
        }
    )
}

# -----------------------------------------------------------------------------
# PIPELINE EXECUTION
# -----------------------------------------------------------------------------

pipeline_start <- Sys.time()

cat("\nPipeline started at:", format(pipeline_start, "%Y-%m-%d %H:%M:%S"), "\n")

# Step 0: Configuration
if (RUN_FULL_PIPELINE || RUN_STEPS$config) {
    run_script("00_config.R", "Configuration and Setup")
}

# Step 1: Data Loading
if (RUN_FULL_PIPELINE || RUN_STEPS$data_loading) {
    run_script("01_data_loading.R", "Data Loading and Preparation")
}

# Step 2: Elasticity Analysis
if (RUN_FULL_PIPELINE || RUN_STEPS$elasticity) {
    run_script("02_elasticity_analysis.R", "Elasticity and Country Classification")
}

# Step 3: Load preprocessing and model functions
# (These are loaded by subsequent scripts automatically)

# Step 4: Forecast DOWN Countries
if (RUN_FULL_PIPELINE || RUN_STEPS$forecast_down) {
    run_script("05_forecast_down.R", "Forecast Countries with Decreasing Deaths")
}

# Step 5: Forecast UP Countries
if (RUN_FULL_PIPELINE || RUN_STEPS$forecast_up) {
    run_script("06_forecast_up.R", "Forecast Countries with Increasing Deaths")
}

# Step 6: Forecast NEAR_ZERO Countries
if (RUN_FULL_PIPELINE || RUN_STEPS$forecast_near_zero) {
    run_script("07_forecast_near_zero.R", "Forecast Countries with Near-Zero Deaths")
}

# Step 7: Forecast REST Countries
if (RUN_FULL_PIPELINE || RUN_STEPS$forecast_rest) {
    run_script("08_forecast_rest.R", "Forecast Remaining Countries")
}

# Step 8: Merge and Generate Submission
if (RUN_FULL_PIPELINE || RUN_STEPS$merge_submit) {
    run_script("09_merge_and_submit.R", "Merge Forecasts and Generate Submission")
}

# -----------------------------------------------------------------------------
# PIPELINE SUMMARY
# -----------------------------------------------------------------------------

pipeline_end <- Sys.time()
total_duration <- difftime(pipeline_end, pipeline_start, units = "mins")

cat("\n")
cat(strrep("=", 70), "\n")
cat(" PIPELINE COMPLETE\n")
cat(strrep("=", 70), "\n")
cat(paste0("\nTotal execution time: ", round(total_duration, 2), " minutes\n"))
cat(paste0("Pipeline ended at: ", format(pipeline_end, "%Y-%m-%d %H:%M:%S"), "\n"))

# Print output summary
if (dir.exists("output")) {
    cat("\nOutput files:\n")
    output_files <- list.files("output", pattern = "\\.(rds|csv)$")
    for (f in output_files) {
        cat(paste0("  - ", f, "\n"))
    }
}

cat("\n")
cat("================================================================================\n")
cat(" Submission file: output/submission.csv\n")
cat(" Also saved as: submission_final.csv (in working directory)\n")
cat("================================================================================\n")
