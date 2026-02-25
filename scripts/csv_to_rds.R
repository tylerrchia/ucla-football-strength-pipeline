library(readr)

# -------------------------------------------------------------------------------
# Data output directory (controlled by GitHub Actions)
output_dir <- Sys.getenv("DATA_OUTPUT_DIR", unset = "data")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------------------
forcedecks_csv <- file.path(output_dir, "forcedecks.csv")
forcedecks_rds <- file.path(output_dir, "forcedecks.rds")

if (file.exists(forcedecks_csv)) {
  saveRDS(
    read_csv(forcedecks_csv, show_col_types = FALSE),
    forcedecks_rds
  )
}

# -------------------------------------------------------------------------------
nordbord_csv <- file.path(output_dir, "nordbord.csv")
nordbord_rds <- file.path(output_dir, "nordbord.rds")

if (file.exists(nordbord_csv)) {
  saveRDS(
    read_csv(nordbord_csv, show_col_types = FALSE),
    nordbord_rds
  )
}

# -------------------------------------------------------------------------------
catapult_csv <- file.path(output_dir, "catapult.csv")
catapult_rds <- file.path(output_dir, "catapult.rds")

if (file.exists(catapult_csv)) {
  saveRDS(
    read_csv(catapult_csv, show_col_types = FALSE),
    catapult_rds
  )
}

# -------------------------------------------------------------------------------
manual_csv <- file.path(output_dir, "manual_overrides.csv")
manual_rds <- file.path(output_dir, "manual_overrides.rds")

if (file.exists(manual_csv)) {
  saveRDS(
    read_csv(manual_csv, show_col_types = FALSE),
    manual_rds
  )
}
