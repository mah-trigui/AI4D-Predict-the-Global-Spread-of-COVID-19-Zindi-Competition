# COVID-19 Death Forecasting — Trajectory-Aware Pipeline

This competition is hosted on Zindi, a machine learning platform for data science challenges.  
Here is the link to the competition: [AI4D Predict the Global Spread of COVID-19 🌾 - Win €5,000 EUR](https://zindi.global/competitions/predict-the-global-spread-of-covid-19)

Ranked 4th position (only 47 succeed to submit among 884 competitors)!

---

Forecasting cumulative COVID-19 deaths for ~180 countries across a multi-week horizon.
Zindi COVID-19 Global Forecasting Competition — April/May 2020.

---

## Core Design: Trajectory Classification Before Forecasting

The pipeline does not apply a single forecasting strategy to all countries. It first classifies each country by its current death trajectory, then routes it to a forecasting strategy matched to that trajectory.

**Step 1 — Elasticity scoring:**
For each country, compute rate-of-change in daily deaths across two windows (6-day and 10-day):

```r
# Vectorized elasticity: rate of change in daily deaths
jh[, elas := {
  prev  <- shift(death_dif, 1L, fill = 0)
  denom <- fifelse(prev == 0, prev + 1, prev)
  (death_dif - prev) / denom
}]
```

**Step 2 — Country classification:**
- **DOWN**: recent slope ≤ 0 and 6-day avg ≤ 10-day avg → use damped ETS, pick most conservative model
- **UP**: recent slope > 0 and daily deaths ≥ 10 → use undamped ETS/ARIMA, pick median prediction
- **NEAR_ZERO**: ≤ 7 days of data → validate against held-out days before selecting model
- **REST**: everything else → validation-based model selection across ETS/ARIMA/TBATS variants

**Step 3 — Per-class forecasting with data quality preprocessing:**
Each forecast runs on multiple data versions: original, spike-smoothed (threshold 0.5), and more aggressively smoothed (threshold 0.33). The best version is selected per country.

---

## Key Engineering Decisions

### Spike smoothing before modeling
Irregular reporting (missed days followed by catch-up corrections) would distort time series models. Two thresholds were applied to redistribute abnormal spikes before fitting.

### Validation-based model selection for ambiguous countries
For REST and NEAR_ZERO countries, models were evaluated on a held-out 5-7 day window within training data. The model with lowest RMSE on that validation window was used — not the best CV score across the full series.

### Separate treatment of DOWN countries
For decelerating countries, the pipeline selects the model with the *minimum cumulative prediction* rather than best fit. This encodes the epidemiological prior that a decelerating trend should not suddenly reverse in the forecast.

---

## Project Structure

```
├── 00_config.R              # Libraries, dates, helper functions
├── 01_data_loading.R        # Load Zindi + Johns Hopkins data, map country names
├── 02_elasticity_analysis.R # Compute elasticity, classify countries by trajectory
├── 03_data_preprocessing.R  # Negative value cleaning, spike smoothing, interpolation
├── 04_time_series_models.R  # ETS, ARIMA, TBATS model functions (log + diff-log)
├── 05_forecast_down.R       # Forecast DOWN countries (damped, conservative)
├── 06_forecast_up.R         # Forecast UP countries (undamped, median selection)
├── 07_forecast_near_zero.R  # Forecast NEAR_ZERO countries (validated flat/model)
├── 08_forecast_rest.R       # Forecast REST countries (validation-based selection)
├── 09_merge_and_submit.R    # Merge all forecasts, generate submission
└── MAIN.R                   # Full pipeline orchestration
```

---

## Technical Stack

- **Language**: R
- **Time series**: forecast (ETS, ARIMA, TBATS, auto.arima)
- **Data**: data.table, dplyr, lubridate
- **Data source**: Johns Hopkins CSSE COVID-19 time series

---

## How to Run

```r
source("MAIN.R")
```

Requires Zindi `train.csv` and Johns Hopkins time series CSVs in the working directory.

---

## Scope

Competition data is not included. The repository shares the pipeline architecture and the trajectory-aware forecasting approach.

## Key Features

### Data Preprocessing
- **Negative value cleaning**: Replace negative daily deaths with 0
- **Spike smoothing**: Average out abnormal data entry spikes
- **Interpolation**: Fix consecutive days with same values

### Time Series Transformations
- **Log transformation**: Stabilize variance
- **Differencing**: Remove trend for stationarity
- **Combined**: diff(log(x)) for both effects

### Model Ensemble
Multiple models are fit and the best is selected based on validation MAE:
- ETS (Exponential Smoothing)
- ARIMA (AutoRegressive Integrated Moving Average)
- TBATS (Trigonometric, Box-Cox, ARMA, Trend, Seasonal)

### Validation Strategy
- Hold out recent days for model selection
- Use MAE as primary metric
- Select model with best validation performance

## Output Files

| File | Description |
|------|-------------|
| `output/jh_processed.rds` | Processed Johns Hopkins data |
| `output/elasticity_summary.rds` | Country trend analysis |
| `output/forecast_*_cumulative.rds` | Cumulative forecasts by category |
| `output/forecast_*_daily.rds` | Daily death forecasts by category |
| `output/submission.csv` | Final submission file |

## Configuration Parameters

Edit `00_config.R` to customize:
- `TRAIN_END_DATE`: Last date of training data
- `FORECAST_START_DATE`: First date to forecast
- `FORECAST_END_DATE`: Last date to forecast
- `DATA_DIR`: Input data directory
- `OUTPUT_DIR`: Output directory

## Troubleshooting

### Missing Packages
```r
# Install all required packages
install.packages(c("here", "data.table", "dplyr", "tidyverse", "forecast",
                   "tseries", "caret", "MLmetrics", "sqldf", "opera"))
```

### Memory Issues
- Process countries in smaller batches
- Clear intermediate objects with `rm()`
- Use `gc()` to force garbage collection

### Model Convergence
- Some models may not converge for countries with irregular data
- Pipeline handles errors gracefully and falls back to simpler models

## License

This project is for educational and competition purposes.

## Acknowledgments

- Johns Hopkins University CSSE for COVID-19 data
- Zindi for hosting the competition
