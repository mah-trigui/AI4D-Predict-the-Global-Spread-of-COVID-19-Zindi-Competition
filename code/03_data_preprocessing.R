# =============================================================================
# 03_data_preprocessing.R - Data Preprocessing Functions
# COVID-19 Death Forecasting Pipeline
# =============================================================================
# Description: Functions to clean and preprocess country data before forecasting
# =============================================================================

source("00_config.R")

print_section("Step 3: Data Preprocessing Functions")

# -----------------------------------------------------------------------------
# 1. CLEAN NEGATIVE DEATH VALUES
# -----------------------------------------------------------------------------

#' Clean negative daily death values
#' Some countries have correction entries with negative values
clean_negative_deaths <- function(df) {
    df$death_dif <- pmax(df$death_dif, 0)
    return(df)
}

# -----------------------------------------------------------------------------
# 2. TRIM LEADING ZEROS
# -----------------------------------------------------------------------------

#' Remove rows before first death occurred
trim_leading_zeros <- function(df) {
    first_death_idx <- which(df$death_dif != 0)[1]
    if (!is.na(first_death_idx)) {
        return(df[first_death_idx:nrow(df), ])
    }
    return(df)
}

# -----------------------------------------------------------------------------
# 3. FIX CONSECUTIVE SAME VALUES
# -----------------------------------------------------------------------------

#' Fix consecutive days with same death count (likely missed reporting)
#' Interpolates values when confirmed cases show changes but deaths don't
fix_consecutive_same_values <- function(df) {
    if (nrow(df) < 3) {
        return(df)
    }

    for (i in 2:(nrow(df) - 1)) {
        # If confirmed cases changed but deaths stayed same
        if (df$confir_dif[[i]] == 0 && df$nb_death[[i]] == df$nb_death[[i - 1]]) {
            # Interpolate death count
            df$nb_death[[i]] <- round((df$nb_death[[i - 1]] + df$nb_death[[i + 1]]) / 2)
            # Recalculate differences
            df$death_dif[[i]] <- df$nb_death[[i]] - df$nb_death[[i - 1]]
            df$death_dif[[i + 1]] <- df$nb_death[[i + 1]] - df$nb_death[[i]]
        }
    }
    return(df)
}

# -----------------------------------------------------------------------------
# 4. SMOOTH ABNORMAL SPIKES
# -----------------------------------------------------------------------------

#' Smooth abnormal increases/decreases in death counts
#' Uses hypothesis-based thresholds (0.5 for moderate, 0.33 for strict)
smooth_abnormal_spikes <- function(df, threshold = 0.5, multiplier = 2) {
    if (nrow(df) < 4) {
        return(df)
    }

    for (i in 3:(nrow(df) - 1)) {
        prev_val <- df$death_dif[[i - 1]] + 1 # +1 to avoid division by zero
        curr_val <- df$death_dif[[i]]
        next_val <- df$death_dif[[i + 1]]

        # Check if current value is suspiciously low followed by high
        if (curr_val != 0) {
            ratio_prev <- curr_val / prev_val
            ratio_next <- next_val / (curr_val + 1)

            if (ratio_prev < threshold && ratio_next > multiplier) {
                # Redistribute values
                if ((curr_val / df$death_dif[[i - 1]]) < (curr_val / next_val)) {
                    # Merge with previous
                    total <- curr_val + df$death_dif[[i - 1]]
                    df$death_dif[[i - 1]] <- round(total / 2)
                    df$death_dif[[i]] <- total - df$death_dif[[i - 1]]
                } else {
                    # Merge with next
                    total <- curr_val + next_val
                    df$death_dif[[i]] <- round(total / 2)
                    df$death_dif[[i + 1]] <- total - df$death_dif[[i]]
                }
            }
        } else {
            # Handle zero current value
            ratio_prev <- (curr_val + 1) / prev_val
            ratio_next <- next_val / (curr_val + 1)

            if (ratio_prev < threshold && ratio_next > multiplier) {
                if (i > 1 && df$death_dif[[i - 1]] > 0) {
                    if (((curr_val + 1) / df$death_dif[[i - 1]]) < ((curr_val + 1) / next_val)) {
                        total <- curr_val + df$death_dif[[i - 1]]
                        df$death_dif[[i - 1]] <- round(total / 2)
                        df$death_dif[[i]] <- total - df$death_dif[[i - 1]]
                    } else {
                        total <- curr_val + next_val
                        df$death_dif[[i]] <- round(total / 2)
                        df$death_dif[[i + 1]] <- total - df$death_dif[[i]]
                    }
                }
            }
        }
    }
    return(df)
}

# -----------------------------------------------------------------------------
# 5. CREATE MODIFIED DATASETS
# -----------------------------------------------------------------------------

#' Create multiple versions of data with different smoothing levels
create_modified_datasets <- function(df) {
    # Version 1: Moderate smoothing (threshold = 0.5)
    df_mod1 <- smooth_abnormal_spikes(df, threshold = 0.5, multiplier = 2)

    # Version 2: Aggressive smoothing (threshold = 0.33)
    df_mod2 <- smooth_abnormal_spikes(df, threshold = 0.33, multiplier = 3)

    # Average the modified versions with original
    df_avg1 <- average_datasets(df_mod1, df)
    df_avg2 <- average_datasets(df_mod2, df)

    return(list(
        original = df,
        moderate = df_avg1,
        aggressive = df_avg2
    ))
}

#' Average two datasets
average_datasets <- function(df1, df2) {
    combined <- rbindlist(list(df1, df2))
    result <- as.data.frame(sqldf("
    SELECT date, ROUND(AVG(death_dif)) as death_dif, AVG(nb_death) as nb_death
    FROM combined
    GROUP BY date
    ORDER BY date
  "))

    # Clean any negative values
    for (i in seq_len(nrow(result))) {
        if (result$death_dif[[i]] < 0) {
            result$death_dif[[i]] <- 0
        }
    }

    return(result)
}

# -----------------------------------------------------------------------------
# 6. COMPLETE PREPROCESSING PIPELINE
# -----------------------------------------------------------------------------

#' Full preprocessing pipeline for a country
preprocess_country_data <- function(df, apply_smoothing = TRUE) {
    # Step 1: Clean negative values
    df <- clean_negative_deaths(df)

    # Step 2: Trim leading zeros
    df <- trim_leading_zeros(df)

    # Step 3: Fix consecutive same values
    df <- fix_consecutive_same_values(df)

    # Step 4: Apply smoothing if requested
    if (apply_smoothing) {
        return(create_modified_datasets(df))
    }

    return(list(original = df))
}

message("Preprocessing functions loaded!")
