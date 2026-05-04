library(readr)
library(dplyr)

url <- Sys.getenv("GOOGLE_SHEETS_CSV")
if (url == "") {
  stop("GOOGLE_SHEETS_CSV is not set")
}

# retry logic
max_attempts <- 3
df <- NULL

for (attempt in seq_len(max_attempts)) {
  tryCatch({
    df <- read_csv(url) %>%
      filter(!is.na(Value))
    break  # success — exit loop
  }, error = function(e) {
    message(sprintf("Attempt %d failed: %s", attempt, conditionMessage(e)))
    if (attempt == max_attempts) stop(e)
    Sys.sleep(30 * attempt)  # 30s, then 60s before retrying
  })
}

# -------------------------------------------------------------------------------
output_dir <- Sys.getenv("DATA_OUTPUT_DIR", unset = "data")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
# -------------------------------------------------------------------------------
write_csv(
  df,
  file.path(output_dir, "manual_overrides.csv")
)
