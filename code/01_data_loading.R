# =============================================================================
# 01_data_loading.R - Data Loading and Preparation
# COVID-19 Death Forecasting Pipeline
# =============================================================================
# Description: Load data from Zindi competition and Johns Hopkins University,
#              clean and prepare for analysis
# =============================================================================

source("00_config.R")

print_section("Step 1: Data Loading")

# -----------------------------------------------------------------------------
# 1. LOAD ZINDI COMPETITION DATA
# -----------------------------------------------------------------------------

print_step("Loading Zindi competition data...")

train_zindi <- fread(file.path(DATA_DIR, "train.csv"))
ref_zindi <- as.data.table(train_zindi %>% distinct(Territory))
names(ref_zindi)[1] <- "region"

message(paste("Zindi data loaded:", nrow(train_zindi), "rows"))
message(paste("Unique territories:", nrow(ref_zindi)))

# -----------------------------------------------------------------------------
# 2. LOAD JOHNS HOPKINS DATA
# -----------------------------------------------------------------------------

print_step("Loading Johns Hopkins data...")

# Load confirmed cases, deaths, and recovered
jh_confirmed <- read_csv(file.path(JH_DATA_PATH, "time_series_covid19_confirmed_global.csv"))
jh_deaths <- read_csv(file.path(JH_DATA_PATH, "time_series_covid19_deaths_global.csv"))
jh_recovered <- read_csv(file.path(JH_DATA_PATH, "time_series_covid19_recovered_global.csv"))

# Rename region column
names(jh_confirmed)[2] <- "region"
names(jh_deaths)[2] <- "region"
names(jh_recovered)[2] <- "region"

message("Johns Hopkins data loaded successfully")

# -----------------------------------------------------------------------------
# 3. FIX OVERSEAS TERRITORIES
# -----------------------------------------------------------------------------

print_step("Fixing overseas territories mapping...")

# List of overseas territories that need to be separated
overseas_territories <- c(
    "Aruba", "Bermuda", "Cayman Islands", "Curacao", "Reunion",
    "Saint Barthelemy", "St Martin", "Faroe Islands", "Greenland",
    "French Polynesia", "French Guiana", "New Caledonia"
)

# Fix territories in all datasets
fix_overseas <- function(df) {
    for (territory in overseas_territories) {
        df$region[df$`Province/State` == territory] <- territory
    }
    return(df)
}

jh_confirmed <- fix_overseas(jh_confirmed)
jh_deaths <- fix_overseas(jh_deaths)
jh_recovered <- fix_overseas(jh_recovered)

# -----------------------------------------------------------------------------
# 4. CLEAN AND AGGREGATE DATA
# -----------------------------------------------------------------------------

print_step("Cleaning and aggregating data...")

# Remove unnecessary columns and get unique records
clean_jh_data <- function(df) {
    df$`Province/State` <- NULL
    df$Lat <- NULL
    df$Long <- NULL
    unique(df)
}

jh_confirmed <- clean_jh_data(jh_confirmed)
jh_deaths <- clean_jh_data(jh_deaths)
jh_recovered <- clean_jh_data(jh_recovered)

# -----------------------------------------------------------------------------
# 5. RESHAPE TO LONG FORMAT
# -----------------------------------------------------------------------------

print_step("Reshaping data to long format...")

# Convert deaths to long format
deaths_long <- jh_deaths %>%
    gather(date, nb_death, -region) %>%
    mutate(date = as.Date(date, "%m/%d/%y")) %>%
    arrange(region, date)

deaths_long <- as.data.table(sqldf("
  SELECT region, date, SUM(nb_death) as nb_death
  FROM deaths_long
  GROUP BY region, date
"))

# Convert recovered to long format
recovered_long <- jh_recovered %>%
    gather(date, nb_recov, -region) %>%
    mutate(date = as.Date(date, "%m/%d/%y")) %>%
    arrange(region, date)

recovered_long <- as.data.table(sqldf("
  SELECT region, date, SUM(nb_recov) as nb_recov
  FROM recovered_long
  GROUP BY region, date
"))

# Convert confirmed to long format
confirmed_long <- jh_confirmed %>%
    gather(date, nb_confir, -region) %>%
    mutate(date = as.Date(date, "%m/%d/%y"))

confirmed_long <- as.data.table(sqldf("
  SELECT region, date, SUM(nb_confir) as nb_confir
  FROM confirmed_long
  GROUP BY region, date
"))

# -----------------------------------------------------------------------------
# 6. MERGE ALL DATA
# -----------------------------------------------------------------------------

print_step("Merging confirmed, deaths, and recovered data...")

jh <- as.data.table(sqldf("
  SELECT DISTINCT a.*, b.nb_death, c.nb_recov
  FROM confirmed_long a
  LEFT JOIN deaths_long b ON a.region = b.region AND a.date = b.date
  LEFT JOIN recovered_long c ON a.region = c.region AND a.date = c.date
"))

jh <- jh[order(region, date)]

# Convert to numeric
jh$nb_confir <- as.numeric(jh$nb_confir)
jh$nb_death <- as.numeric(jh$nb_death)
jh$nb_recov <- as.numeric(jh$nb_recov)

# Calculate active cases
jh$nb_actif <- jh$nb_confir - jh$nb_recov

message(paste("Merged data:", nrow(jh), "rows"))
message(paste("Countries:", length(unique(jh$region))))

# -----------------------------------------------------------------------------
# 7. CALCULATE DAILY DIFFERENCES
# -----------------------------------------------------------------------------

print_step("Calculating daily differences...")

jh$confir_dif <- ave(jh$nb_confir, factor(jh$region), FUN = function(x) c(NA, diff(x)))
jh$death_dif <- ave(jh$nb_death, factor(jh$region), FUN = function(x) c(NA, diff(x)))
jh$recov_dif <- ave(jh$nb_recov, factor(jh$region), FUN = function(x) c(NA, diff(x)))
jh$actif_dif <- ave(jh$nb_actif, factor(jh$region), FUN = function(x) c(NA, diff(x)))

# Replace NAs with 0
jh$confir_dif[is.na(jh$confir_dif)] <- 0
jh$death_dif[is.na(jh$death_dif)] <- 0
jh$recov_dif[is.na(jh$recov_dif)] <- 0
jh$actif_dif[is.na(jh$actif_dif)] <- 0

# -----------------------------------------------------------------------------
# 8. MAP COUNTRY NAMES TO ZINDI FORMAT
# -----------------------------------------------------------------------------

print_step("Mapping country names to Zindi format...")

country_mapping <- c(
    "Bahamas" = "Bahamas (the)",
    "Bolivia" = "Bolivia (Plurinational State of)",
    "Brunei" = "Brunei Darussalam",
    "Burma" = "Myanmar",
    "Central African Republic" = "Central African Republic (the)",
    "Congo (Brazzaville)" = "Congo (the)",
    "Congo (Kinshasa)" = "Democratic Republic of the Congo (the)",
    "Cote d'Ivoire" = "Côte d'Ivoire",
    "Korea, South" = "Democratic People's Republic of Korea (the)",
    "Dominican Republic" = "Dominican Republic (the)",
    "Gambia" = "Gambia (the)",
    "Iran" = "Iran (Islamic Republic of)",
    "Laos" = "Lao People's Democratic Republic (the)",
    "Moldova" = "Republic of Moldova (the)",
    "Netherlands" = "Netherlands (the)",
    "Niger" = "Niger (the)",
    "Philippines" = "Philippines (the)",
    "Russia" = "Russian Federation (the)",
    "Sudan" = "Sudan (the)",
    "Syria" = "Syrian Arab Republic (the)",
    "Taiwan*" = "Taiwan",
    "Tanzania" = "United Republic of Tanzania (the)",
    "US" = "United States of America (the)",
    "United Arab Emirates" = "United Arab Emirates (the)",
    "United Kingdom" = "United Kingdom of Great Britain and Northern Ireland (the)",
    "Venezuela" = "Venezuela (Bolivarian Republic of)",
    "Vietnam" = "Viet Nam"
)

for (old_name in names(country_mapping)) {
    jh$region[jh$region == old_name] <- country_mapping[old_name]
}

# -----------------------------------------------------------------------------
# 9. SAVE PROCESSED DATA
# -----------------------------------------------------------------------------

print_step("Saving processed data...")

saveRDS(jh, file.path(OUTPUT_DIR, "jh_processed.rds"))
saveRDS(ref_zindi, file.path(OUTPUT_DIR, "ref_zindi.rds"))

# Clean up
rm(jh_confirmed, jh_deaths, jh_recovered, confirmed_long, deaths_long, recovered_long)

message("Data loading complete!")
message(paste("Processed data saved to:", OUTPUT_DIR))
