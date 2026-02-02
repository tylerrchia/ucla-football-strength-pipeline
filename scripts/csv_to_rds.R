library(readr)

if (file.exists("data/forcedecks.csv")) {
  saveRDS(
    read_csv("data/forcedecks.csv", show_col_types = FALSE),
    "data/forcedecks.rds"
  )
}

if (file.exists("data/nordbord.csv")) {
  saveRDS(
    read_csv("data/nordbord.csv", show_col_types = FALSE),
    "data/nordbord.rds"
  )
}

if (file.exists("data/catapult.csv")) {
  saveRDS(
    read_csv("data/catapult.csv", show_col_types = FALSE),
    "data/catapult.rds"
  )
}
