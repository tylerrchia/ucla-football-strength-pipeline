# UCLA-Football-Strength-App

## Automated Data Pipeline
This repository contains a GitHub Actions workflow that:
- Pulls daily data from VALD and Catapult APIs
- Saves raw CSVs and RDS files
- Commits updates automatically
- Data is stored in a separate private repository

### Environment Reproducibility
- R version is pinned via GitHub Actions
- Exact package versions are locked using `renv`
- Dependency versions are defined in `renv.lock`

### Updating Packages
To intentionally update dependencies:
1. Run `renv::update()` locally
2. Test scripts
3. Commit updated `renv.lock`

### Secrets
The workflow requires the following GitHub secrets:
- VALD_CLIENT_ID
- VALD_CLIENT_SECRET
- VALD_TENANT_ID
- VALD_REGION
- CATAPULT_API_TOKEN
