library(readr)

url <- Sys.getenv("GOOGLE_SHEETS_CSV")

if (url == "") {
  stop("GOOGLE_SHEETS_CSV is not set")
}

df <- read_csv(url) %>%
  filter(!is.na(Value))

# -------------------------------------------------------------------------------
output_dir <- Sys.getenv("DATA_OUTPUT_DIR", unset = "data")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------------------
write_csv(
  df,
  file.path(output_dir, "manual_overrides.csv")
)
