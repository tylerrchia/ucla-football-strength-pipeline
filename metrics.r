# metrics.R
# ---------
# In-memory build for VALD (NordBord + ForceDecks) used by Shiny via source("metrics.R")
#
# UPDATED FOR NEW DATA FORMAT:
#   - forcedecks.csv (new column names)
#   - nordbord.csv (new column names)
#
# Outputs created:
#   - vald_tests_long        : canonical long table (all tests, all dates)
#   - vald_tests_long_ui     : long table + metric_key (for player pages)
#   - players               : unique players
#   - as_of_date            : snapshot cutoff
#   - vald_latest_wide      : latest-per-player wide snapshot (ALL metrics)
#   - roster_view           : wide snapshot (ONLY >=80% filled metrics)
#   - fill_summary          : metric_key -> fill_frac
#   - keep_roster_metrics   : metric keys kept in roster_view
#   - roster_percentiles_wide : roster_view + per-metric percentiles as extra columns (__pctl)
#   - roster_percentiles_long : long percentile table (best for joins / UI)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
})

# ---------------------------
# Config
# ---------------------------
NORD_PATH  <- "data/nordbord.rds"
FORCE_PATH <- "data/forcedecks.rds"
POS_PATH <- "data/profiles_with_groups.rds"

as_of_date <- as.Date("2026-01-23")

filled_threshold <- 0.25

# ---------------------------
# Helpers
# ---------------------------

standardize_name <- function(x) {
  x %>%
    str_squish() %>%
    str_to_upper() %>%
    str_replace_all("[\\.,'']", "") %>%
    str_squish()
}

parse_units <- function(col) {
  u1 <- str_match(col, "\\(([^\\)]+)\\)")[, 2]
  u2 <- str_match(col, "\\[([^\\]]+)\\]")[, 2]
  dplyr::coalesce(u1, u2)
}

clean_metric_name <- function(col) {
  col %>%
    str_squish() %>%
    str_replace_all("\\s*\\([^\\)]+\\)\\s*$", "") %>%   # drop (N)
    str_replace_all("\\s*\\[[^\\]]+\\]\\s*$", "") %>%   # drop [kg]
    str_squish()
}

parse_metric_numeric <- function(x) {
  x %>%
    str_squish() %>%
    dplyr::na_if("") %>%
    dplyr::na_if("—") %>%
    dplyr::na_if("-") %>%
    str_replace_all("%", "") %>%
    str_replace_all("[^0-9\\.\\-]", "") %>%
    suppressWarnings(as.numeric(.))
}

pct_rank_100 <- function(x) {
  # percentile in [0,100], higher=better as default
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  r <- rank(x, na.last = "keep", ties.method = "average")
  n <- sum(!is.na(x))
  if (n <= 1) return(ifelse(is.na(x), NA_real_, 100))
  (r - 1) / (n - 1) * 100
}

ingest_positions <- function(path) {
  # Check file extension
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    pos_raw <- readRDS(path)
  } else {
    pos_raw <- read_csv(path, show_col_types = FALSE)
  }
  
  # Standardize column names - handle both old and new formats
  pos_raw <- pos_raw %>%
    rename_with(~ case_when(
      . == "groupName" ~ "Group",
      . == "positionName" ~ "Position",
      tolower(.) == "group" ~ "Group",
      tolower(.) == "position" ~ "Position",
      tolower(.) == "firstname" ~ "firstName",
      tolower(.) == "lastname" ~ "lastName",
      TRUE ~ .
    ))
  
  pos_raw <- pos_raw %>%
    mutate(
      player_name = str_squish(coalesce(name, paste(firstName, lastName))),
      player_id   = standardize_name(player_name),
      Group       = na_if(str_squish(as.character(Group)), ""),
      Position    = na_if(str_squish(as.character(Position)), ""),
      year        = suppressWarnings(as.integer(year))
    )
  
  # If duplicates exist for the same player_id, keep the "best" row
  # (prefer non-NA Position/Group/year)
  pos_clean <- pos_raw %>%
    group_by(player_id) %>%
    summarise(
      pos_player_name = first(na.omit(player_name)),
      pos_group       = dplyr::coalesce(first(na.omit(Group)), NA_character_),
      pos_position    = dplyr::coalesce(first(na.omit(Position)), NA_character_),
      pos_year        = dplyr::coalesce(first(na.omit(year)), NA_integer_),
      .groups = "drop"
    )
  
  pos_clean
}

# ---------------------------
# Ingestors for NEW DATA FORMAT
# ---------------------------

ingest_nordboard <- function(path) {
  # Check file extension and read accordingly
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    raw <- readRDS(path)
  } else {
    raw <- read_csv(path, show_col_types = FALSE)
  }
  
  # New column names: profileId, testId, testType, firstName, lastName, 
  # groupName, positionName, year, date, leftAvgForce, leftImpulse, leftMaxForce, 
  # rightAvgForce, rightImpulse, rightMaxForce, asymmetry, name
  
  raw <- raw %>%
    mutate(
      # Parse date - try multiple formats
      date = if(inherits(date, "Date")) {
        date
      } else {
        # Try different date parsing functions
        parsed <- suppressWarnings(ymd(date))
        if(all(is.na(parsed))) parsed <- suppressWarnings(mdy(date))
        if(all(is.na(parsed))) parsed <- suppressWarnings(dmy(date))
        parsed
      },
      datetime = as.POSIXct(date),
      player_name = str_squish(coalesce(name, paste(firstName, lastName))),
      player_id = standardize_name(player_name),
      source = "NordBord",
      test_type = coalesce(testType, "Nordic Hamstring")
    )
  
  id_cols <- c("player_id", "player_name", "date", "datetime", "source", "test_type")
  
  # Metrics to extract: all numeric columns that are actual metrics
  metric_cols <- c(
    "leftAvgForce", "leftImpulse", "leftMaxForce",
    "rightAvgForce", "rightImpulse", "rightMaxForce",
    "asymmetry"
  )
  
  # Filter to only columns that exist
  metric_cols <- intersect(metric_cols, names(raw))
  
  if (length(metric_cols) == 0) {
    stop("No metric columns found in NordBord data. Check column names.")
  }
  
  raw %>%
    select(any_of(c(id_cols, metric_cols))) %>%
    pivot_longer(
      cols = all_of(metric_cols),
      names_to = "metric_raw",
      values_to = "metric_value_raw",
      values_transform = list(metric_value_raw = as.character)
    ) %>%
    mutate(
      # Convert camelCase to readable format with units
      metric_name = case_when(
        metric_raw == "leftAvgForce" ~ "L Avg Force",
        metric_raw == "leftImpulse" ~ "L Max Impulse",
        metric_raw == "leftMaxForce" ~ "L Max Force",
        metric_raw == "rightAvgForce" ~ "R Avg Force",
        metric_raw == "rightImpulse" ~ "R Max Impulse",
        metric_raw == "rightMaxForce" ~ "R Max Force",
        metric_raw == "asymmetry" ~ "Max Imbalance",
        TRUE ~ metric_raw
      ),
      units = case_when(
        str_detect(metric_raw, "Force") ~ "N",
        str_detect(metric_raw, "Impulse") ~ "Ns",
        metric_raw == "asymmetry" ~ "%",
        TRUE ~ NA_character_
      ),
      metric_value = parse_metric_numeric(metric_value_raw)
    ) %>%
    filter(!is.na(metric_value)) %>%
    select(-metric_value_raw)
}

ingest_forcedecks <- function(path) {
  # Check file extension and read accordingly
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    raw <- readRDS(path)
  } else {
    raw <- read_csv(path, show_col_types = FALSE)
  }
  
  # New column names: profileId, testId, firstName, lastName, groupName, positionName,
  # testType, date, Athlete.Standing.Weight, Concentric.Impulse, etc.
  
  raw <- raw %>%
    mutate(
      # Parse date - try multiple formats
      date = if(inherits(date, "Date")) {
        date
      } else {
        # Try different date parsing functions
        parsed <- suppressWarnings(ymd(date))
        if(all(is.na(parsed))) parsed <- suppressWarnings(mdy(date))
        if(all(is.na(parsed))) parsed <- suppressWarnings(dmy(date))
        parsed
      },
      datetime = as.POSIXct(date),
      player_name = str_squish(coalesce(name, paste(firstName, lastName))),
      player_id = standardize_name(player_name),
      source = "ForceDecks",
      test_type = coalesce(testType, "Unknown")
    )
  
  id_cols <- c("player_id", "player_name", "date", "datetime", "source", "test_type")
  
  # Identify non-metric columns
  non_metric <- c(
    "profileId", "testId", "firstName", "lastName", "groupName", 
    "positionName", "testType", "date", "name",
    "player_id", "player_name", "datetime", "source", "test_type"
  )
  
  metric_cols <- setdiff(names(raw), non_metric)
  
  if (length(metric_cols) == 0) {
    stop("No metric columns found in ForceDecks data. Check column names.")
  }
  
  # Apply Jump Height filter (if column exists)
  jh_col <- "Jump Height (Imp-Mom)"
  if (jh_col %in% names(raw)) {
    raw <- raw %>%
      mutate(jh_temp = parse_metric_numeric(.data[[jh_col]])) %>%
      filter(is.na(jh_temp) | (jh_temp > 5 & jh_temp < 30)) %>%
      select(-jh_temp)
  }
  
  raw %>%
    select(any_of(c(id_cols, metric_cols))) %>%
    pivot_longer(
      cols = all_of(metric_cols),
      names_to = "metric_raw",
      values_to = "metric_value_raw",
      values_transform = list(metric_value_raw = as.character)
    ) %>%
    mutate(
      # Clean up metric names: replace dots with spaces, remove multiple dots
      metric_name = metric_raw %>%
        str_replace_all("\\.{2,}", " ") %>%
        str_replace_all("\\.", " ") %>%
        str_squish(),
      # Infer units from metric names
      units = case_when(
        str_detect(metric_name, "BM") ~ "W/kg or N/kg",
        str_detect(metric_name, "Weight|Force") ~ "N or kg",
        str_detect(metric_name, "Impulse") ~ "Ns",
        str_detect(metric_name, "Velocity") ~ "m/s",
        str_detect(metric_name, "Power") ~ "W/kg",
        str_detect(metric_name, "Time") ~ "ms",
        str_detect(metric_name, "Depth|Height") ~ "cm or in",
        str_detect(metric_name, "RFD") ~ "N/s/kg",
        str_detect(metric_name, "RSI") ~ "m/s",
        TRUE ~ NA_character_
      ),
      metric_value = parse_metric_numeric(metric_value_raw)
    ) %>%
    filter(!is.na(metric_value)) %>%
    select(-metric_value_raw)
}

ingest_vald_bundle <- function(nord_path, force_path) {
  bind_rows(
    ingest_nordboard(nord_path),
    ingest_forcedecks(force_path)
  ) %>%
    mutate(metric_value = as.numeric(metric_value))
}

# ---------------------------
# Build canonical long tables
# ---------------------------

vald_tests_long <- ingest_vald_bundle(NORD_PATH, FORCE_PATH)

players <- vald_tests_long %>%
  distinct(player_id, player_name) %>%
  arrange(player_name)

positions <- ingest_positions(POS_PATH)

# Join to player directory
players <- players %>%
  left_join(positions, by = "player_id")

# Join to long tables (useful for player pages / filtering)
vald_tests_long <- vald_tests_long %>%
  left_join(positions, by = "player_id")

vald_tests_long_ui <- vald_tests_long %>%
  mutate(metric_key = paste(source, test_type, metric_name, sep="|"))

# ---------------------------
# Build wide "as-of" snapshot (ALL metrics)
# ---------------------------

vald_latest_wide <- vald_tests_long_ui %>%
  filter(date <= as_of_date) %>%
  group_by(player_id, player_name, metric_key) %>%
  slice_max(date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, player_name, pos_group, pos_position, metric_key, metric_value) %>%
  pivot_wider(names_from = metric_key, values_from = metric_value) %>%
  mutate(as_of_date = as_of_date)

pos_cols <- c("pos_group", "pos_position")
id_cols <- c("player_id", "player_name", pos_cols, "as_of_date")
metric_cols <- setdiff(names(vald_latest_wide), id_cols)

# ---------------------------
# Keep roster metrics (>=threshold filled) — computed WITHIN SOURCE
# ---------------------------

# helper to extract source from metric_key
metric_source <- function(mk) strsplit(mk, "\\|")[[1]][1]

sources <- sort(unique(vald_tests_long_ui$source))

fill_summary <- lapply(sources, function(s) {
  
  players_s <- vald_tests_long_ui %>%
    filter(source == s, date <= as_of_date) %>%
    distinct(player_id)
  
  metric_cols_s <- metric_cols[startsWith(metric_cols, paste0(s, "|"))]
  if (length(metric_cols_s) == 0) return(tibble::tibble())
  
  wide_s <- vald_latest_wide %>%
    filter(player_id %in% players_s$player_id) %>%
    select(any_of(c("player_id", "player_name", "as_of_date", metric_cols_s)))
  
  summ <- wide_s %>%
    summarise(
      across(
        all_of(metric_cols_s),
        list(
          fill_frac = ~ mean(!is.na(.)),
          zero_frac = ~ {
            x <- .[!is.na(.)]
            if (length(x) == 0) NA_real_ else mean(x == 0)
          },
          n_unique = ~ {
            x <- .[!is.na(.)]
            length(unique(x))
          }
        ),
        .names = "{.col}__{.fn}"
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("metric_key", "stat"),
      names_sep = "__",
      values_to = "value"
    ) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    mutate(source = s) %>%
    arrange(fill_frac)
  
  summ
}) %>%
  bind_rows()

keep_roster_metrics <- fill_summary %>%
  filter(
    fill_frac >= filled_threshold,
    n_unique > 1,
    is.na(zero_frac) | zero_frac <= 0.90
  ) %>%
  pull(metric_key)

roster_view <- vald_latest_wide %>%
  select(all_of(c(id_cols, keep_roster_metrics)))

# ---------------------------
# Roster-wide percentiles per metric (stored two ways)
# ---------------------------

# (A) Wide: add __pctl columns next to metrics
roster_percentiles_wide <- roster_view %>%
  mutate(across(all_of(keep_roster_metrics), pct_rank_100, .names = "{.col}__pctl"))

# Join positions to wide percentiles
roster_percentiles_wide <- roster_percentiles_wide %>%
  left_join(positions, by = "player_id")

# (B) Long: one row per player per metric (best for joins/plots)
roster_percentiles_long <- roster_view %>%
  pivot_longer(
    cols = all_of(keep_roster_metrics),
    names_to = "metric_key",
    values_to = "metric_value"
  ) %>%
  group_by(metric_key) %>%
  mutate(percentile = pct_rank_100(metric_value)) %>%
  ungroup()

# Join positions to long percentiles
roster_percentiles_long <- roster_percentiles_long %>%
  left_join(positions, by = "player_id")

message("Built objects:")
message("  vald_tests_long:         ", nrow(vald_tests_long), " rows")
message("  players:                ", nrow(players), " players")
message("  vald_latest_wide:       ", nrow(vald_latest_wide), " rows, ", length(metric_cols), " metrics")
message("  roster metrics kept:    ", length(keep_roster_metrics), " (>= ", filled_threshold*100, "% filled)")
message("  roster_view:            ", nrow(roster_view), " rows")
message("  roster_percentiles_*:   computed for kept roster metrics")
