# =============================================================================
# 02_elasticity_analysis.R - Elasticity and Evolution Analysis
# COVID-19 Death Forecasting Pipeline
# =============================================================================
# Description: Calculate elasticity metrics to classify countries by their
#              death trajectory (increasing, decreasing, stable)
# =============================================================================

source("00_config.R")

print_section("Step 2: Elasticity Analysis")

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

print_step("Loading processed data...")

jh <- readRDS(file.path(OUTPUT_DIR, "jh_processed.rds"))

# -----------------------------------------------------------------------------
# 2. CALCULATE ELASTICITY
# -----------------------------------------------------------------------------

print_step("Calculating elasticity metrics...")

# Elasticity: rate of change in daily deaths (vectorized)
jh[, elas := {
    prev <- shift(death_dif, 1L, fill = 0)
    denom <- fifelse(prev == 0, prev + 1, prev)
    (death_dif - prev) / denom
}]

# Cap extreme elasticity values (some countries have irregular reporting)
jh$elas <- pmin(pmax(jh$elas, -40), 40)

# -----------------------------------------------------------------------------
# 3. CALCULATE EVOLUTION SCORES
# -----------------------------------------------------------------------------

print_step("Calculating evolution scores...")

# Evolution: categorical score based on elasticity
jh$evol <- 0

for (i in 2:nrow(jh)) {
    e <- jh$elas[[i]]

    # Positive (increasing) evolution
    if (e >= 3) {
        jh$evol[[i]] <- 10
    } else if (e >= 2 & e < 3) {
        jh$evol[[i]] <- 7
    } else if (e >= 1 & e < 2) {
        jh$evol[[i]] <- 5
    } else if (e >= 0.5 & e < 1) {
        jh$evol[[i]] <- 3
    } else if (e >= 0 & e < 0.5) {
        jh$evol[[i]] <- 1
    }

    # Negative (decreasing) evolution
    if (e <= -3) {
        jh$evol[[i]] <- -10
    } else if (e > -3 & e <= -2) {
        jh$evol[[i]] <- -7
    } else if (e > -2 & e <= -1) {
        jh$evol[[i]] <- -5
    } else if (e > -1 & e < -0.5) {
        jh$evol[[i]] <- -3
    } else if (e >= -0.5 & e < 0) {
        jh$evol[[i]] <- -1
    }
}

# Add row ID within each region
jh[, id := rowid(region)]

# -----------------------------------------------------------------------------
# 4. AGGREGATE ELASTICITY BY COUNTRY
# -----------------------------------------------------------------------------

print_step("Aggregating elasticity by country...")

# Create elasticity summary table
elasticity <- as.data.table(unique(jh$region))
names(elasticity)[1] <- "region"
elasticity <- elasticity[order(region)]

# Overall statistics (excluding first row)
aux_j <- jh[jh$id > 1]
aux <- aggregate(. ~ aux_j$region, aux_j[, c("death_dif", "elas", "evol")], max)
names(aux) <- c("region", "death_dif", "elas", "evol")
elasticity <- elasticity %>% left_join(aux, by = "region")

# 10-day window statistics
aux_j <- jh[jh$id > 1 & jh$date >= (TRAIN_END_DATE - 10)]
aux <- aggregate(. ~ aux_j$region, aux_j[, c("death_dif", "elas", "evol")], mean)
names(aux) <- c("region", "diff_10d", "elas_10d", "evol_10d")
elasticity <- elasticity %>% left_join(aux, by = "region")

# 6-day window statistics (more recent)
aux_j <- jh[jh$id > 1 & jh$date >= (TRAIN_END_DATE - 6)]
aux <- aggregate(. ~ aux_j$region, aux_j[, c("death_dif", "elas", "evol")], mean)
names(aux) <- c("region", "diff_6d", "elas_6d", "evol_6d")
elasticity <- elasticity %>% left_join(aux, by = "region")

# Clean NAs
elasticity[is.na(elasticity)] <- 0

message(paste("Elasticity calculated for", nrow(elasticity), "countries"))

# -----------------------------------------------------------------------------
# 5. CLASSIFY COUNTRIES
# -----------------------------------------------------------------------------

print_step("Classifying countries by trajectory...")

# Countries with decreasing deaths (DOWN)
countries_down <- elasticity[elasticity$evol_6d <= 0 &
    elasticity$diff_6d <= elasticity$diff_10d, ]$region

# Add specific countries based on news/policy (manually curated)
additional_down <- c(
    "Canada", "United States of America (the)", "France",
    "Israel", "Saudi Arabia", "Sweden", "Switzerland"
)
countries_down <- unique(c(countries_down, additional_down))

# Countries with increasing deaths (UP) - significant daily deaths
countries_up <- elasticity[elasticity$diff_6d > elasticity$diff_10d &
    elasticity$death_dif >= 10, ]$region
# Remove countries already in DOWN category
countries_up <- setdiff(countries_up, countries_down)

# Countries with near-zero deaths (NEAR_ZERO)
# Will be identified in the forecasting loop based on data availability

# Remaining countries (REST)
countries_rest <- setdiff(unique(jh$region), c(countries_down, countries_up))

message(paste("DOWN countries:", length(countries_down)))
message(paste("UP countries:", length(countries_up)))
message(paste("Initial REST countries:", length(countries_rest)))

# -----------------------------------------------------------------------------
# 6. SAVE RESULTS
# -----------------------------------------------------------------------------

print_step("Saving elasticity analysis results...")

saveRDS(jh, file.path(OUTPUT_DIR, "jh_with_elasticity.rds"))
saveRDS(elasticity, file.path(OUTPUT_DIR, "elasticity_summary.rds"))
saveRDS(countries_down, file.path(OUTPUT_DIR, "countries_down.rds"))
saveRDS(countries_up, file.path(OUTPUT_DIR, "countries_up.rds"))
saveRDS(countries_rest, file.path(OUTPUT_DIR, "countries_rest.rds"))

# Clean up
rm(aux, aux_j)

message("Elasticity analysis complete!")
