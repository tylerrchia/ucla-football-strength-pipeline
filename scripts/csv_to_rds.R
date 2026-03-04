library(readr)

# -------------------------------------------------------------------------------
# data output directory
output_dir <- Sys.getenv("DATA_OUTPUT_DIR", unset = "data")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------------------
# files to convert (without extensions)
files <- c(
  "forcedecks",
  "nordbord",
  "catapult",
  "manual_overrides",
  "smartspeed"
)

# -------------------------------------------------------------------------------
for (file_name in files) {
  
  csv_path <- file.path(output_dir, paste0(file_name, ".csv"))
  rds_path <- file.path(output_dir, paste0(file_name, ".rds"))
  
  if (file.exists(csv_path)) {
    
    # only rewrite RDS if:
    # 1) it doesn't exist
    # 2) CSV is newer than RDS
    if (!file.exists(rds_path) ||
        file.info(csv_path)$mtime > file.info(rds_path)$mtime) {
      
      message("Updating: ", file_name, ".rds")
      
      saveRDS(
        read_csv(csv_path, show_col_types = FALSE),
        rds_path
      )
      
    } else {
      message("No update needed for: ", file_name)
    }
    
  }
}
