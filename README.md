# UCLA Football Strength & Conditioning Analytics Pipeline

An automated data pipeline and R/Shiny application built for UCLA Football's
strength and conditioning staff, ingesting athlete performance data from multiple
vendor APIs and surfacing it in a daily-updated dashboard used by coaches.

**About this repository**

I was the lead developer who built and maintained this project as the Data Scientist/Engineer for UCLA Football. I have transferred this repository to a UCLA Football organization account, where the coaches continues to host and have new developers run it. This repository is a snapshot of my work through June 2026, preserved for portfolio purposes.
Athlete names have been replaced with placeholders, and no athlete data, credentials, or private data files are included. The pipeline is therefore not runnable as-is. The workflows are included to show the architecture.

## What it does

**Automated data pipeline** (`.github/workflows/daily_pull.yml`)
- Runs daily on a schedule, pulling from the VALD API (ForceDecks, NordBord,
  ForceFrame), the Catapult API, the SmartSpeed API, and a manually maintained
  Google Sheet
- Handles incremental appends with deduplication, so each run fetches only a short
  lookback window instead of re-pulling the full history
- Writes results to a separate private data repository, keeping athlete data out
  of the code repo entirely
- Converts raw CSVs to RDS and commits updates automatically
- Sends email notifications on failure

**Automated Shiny deployment** (`.github/workflows/app_deployment.yml`)
- Redeploys the Shiny app to shinyapps.io whenever the underlying RDS files change,
  so coaches always see current data without anyone touching a deploy button

**The app itself** (`app/`)
- Player cards, position-group comparisons, and return-to-play tracking
- Metric definitions and name normalization handled in a shared layer
  (`app/metrics.r`) so the manual and automated versions stay consistent

## Architecture

```text
VALD / Catapult / SmartSpeed APIs + Google Sheets
                  |
                  v
        GitHub Actions (scheduled daily)
                  |
                  v
        Private data repository (CSV + RDS)
                  |
                  v
        R/Shiny app on shinyapps.io
```

Data and code are deliberately separated: athlete data never lands in the code
repository.

## Tech stack

- **R** — Shiny, dplyr, lubridate, and the `rsconnect` deployment toolchain
- **Python** — SmartSpeed API integration (`scripts/smartspeed_import.py`)
- **GitHub Actions** — scheduled pipeline, cross-repo checkout, conditional
  redeploys, and failure alerting
- **shinyapps.io** — app hosting
- **renv** — dependency pinning for reproducible runs

### Environment reproducibility

- R version is pinned via GitHub Actions
- Exact package versions are locked using `renv`
- Dependency versions are defined in `renv.lock`

To update dependencies: run `renv::update()` locally, test, then commit the
updated `renv.lock`.

### Required secrets (for reference)

The production workflow reads these from GitHub Actions secrets. None are
included here.

`VALD_CLIENT_ID`, `VALD_CLIENT_SECRET`, `VALD_TENANT_ID`, `VALD_REGION`,
`CATAPULT_API_TOKEN`, `DATA_REPO_PAT`, `GOOGLE_SHEETS_CSV`,
`SHINYAPPS_ACCOUNT`, `SHINYAPPS_SECRET`, `SHINYAPPS_TOKEN`, `SHINYAPPS_APPNAME`
