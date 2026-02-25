library(readr)

url <- Sys.getenv("GOOGLE_SHEETS_CSV")

if (url == "") {
  stop("GSHEET_CSV_URL is not set")
}

df <- read_csv(url)
