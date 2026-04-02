# metrics.R
# ---------
# In-memory build for VALD (NordBord + ForceDecks) + Catapult used by Shiny via source("metrics.R")
#
# INTEGRATED VERSION:
#   - Handles new data format (camelCase columns from API via RDS files)
#   - Includes position-based percentiles
#   - Includes Athleticism Score composite
#   - Includes Catapult data support
#   - Includes measurements (height, weight, etc.)
#   - Uses RDS files from new data folder
#
# Outputs created:
#   - vald_tests_long        : canonical long table (all tests, all dates)
#   - vald_tests_long_ui     : long table + metric_key (for player pages)
#   - catapult_tests_long    : catapult data in long format
#   - catapult_tests_long_ui : catapult data with metric_key
#   - players               : unique players
#   - as_of_date            : snapshot cutoff
#   - vald_latest_wide      : latest-per-player wide snapshot (ALL metrics)
#   - vald_best_wide        : best-per-player wide snapshot (ALL metrics)
#   - roster_view           : wide snapshot (ONLY >=25% filled metrics)
#   - roster_best_view      : best-only wide snapshot (kept metrics only)
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
# Resolve data directory: env var > local dev path > shinyapps.io bundled path
DATA_DIR <- Sys.getenv("DATA_OUTPUT_DIR", unset = "")
if (!nzchar(DATA_DIR) || !dir.exists(DATA_DIR)) {
  if (dir.exists("data-repo/data")) {
    DATA_DIR <- "data-repo/data"
  } else if (dir.exists("data")) {
    DATA_DIR <- "data"
  } else {
    DATA_DIR <- "data-repo/data"  # fallback (will warn on missing files)
  }
}

NORD_PATH  <- file.path(DATA_DIR, "nordbord.rds")
FORCE_PATH <- file.path(DATA_DIR, "forcedecks.rds")
POS_PATH   <- file.path(DATA_DIR, "profiles_with_groups.rds")
CAT_PATH   <- file.path(DATA_DIR, "catapult.rds")
MEAS_PATH  <- file.path(DATA_DIR, "measurements.rds")
SMART_PATH     <- file.path(DATA_DIR, "smartspeed.rds")
OVERRIDES_PATH <- file.path(DATA_DIR, "manual_overrides.rds")

OVERRIDE_TESTS <- c("Vertical Jump", "Squat", "Bench", "Clean")

as_of_date <- Sys.Date()

filled_threshold <- 0.25

# ---------------------------
# Helpers
# ---------------------------

name_fixes <- c(
  "Player A"  = "Player A",
  "Player D" = "Player D",
  "Player B"   = "Player B",
  "Player C"    = "Player C",
  "Player E"  = "Player E",
  "Player E" = "Player E",
  "Player F" = "Player F"
)

fix_player_name <- function(x) {
  raw <- stringr::str_squish(as.character(x))
  
  key <- raw %>%
    stringr::str_to_upper() %>%
    stringr::str_replace_all("[\\.,''`]", "") %>%
    stringr::str_squish()
  
  fixed <- dplyr::if_else(
    key %in% names(name_fixes),
    unname(name_fixes[key]),
    raw,
    missing = raw
  )
  
  stringr::str_squish(fixed)
}

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
    str_replace_all("\\s*\\([^\\)]+\\)\\s*$", "") %>%
    str_replace_all("\\s*\\[[^\\]]+\\]\\s*$", "") %>%
    str_squish()
}

parse_metric_numeric <- function(x) {
  suppressWarnings({
    x %>%
      as.character() %>%
      str_squish() %>%
      dplyr::na_if("") %>%
      dplyr::na_if("\u2014") %>%
      dplyr::na_if("-") %>%
      dplyr::na_if("NA") %>%
      str_replace_all("%", "") %>%
      str_replace_all("[^0-9\\.\\-]", "") %>%
      as.numeric()
  })
}

pct_rank_100 <- function(x) {
  # Returns percentile in [0, 100]; higher = better by default.
  # Single non-NA value returns 50 (mid-point) rather than an
  # arbitrary 100, which is less misleading in the UI.
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  n <- sum(!is.na(x))
  if (n <= 1) return(ifelse(is.na(x), NA_real_, 50))
  r <- rank(x, na.last = "keep", ties.method = "average")
  (r - 1) / (n - 1) * 100
}

is_flip_metric <- function(metric_key) {
  grepl("Imbalance|Deceleration|Best Split Seconds", metric_key, ignore.case = TRUE)
}

latest_only_metric_name <- function(metric_key) {
  nm <- sub("^.*\\|", "", metric_key) %>% str_squish()
  nm <- str_replace_all(nm, "\\s*\\([^\\)]+\\)\\s*$", "")
  nm <- str_replace_all(nm, "\\s*\\[[^\\]]+\\]\\s*$", "")
  nm %in% c("Total Player Load", "Total Distance", "Athlete Standing Weight")
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

# ---------------------------
# Position ingestion
# ---------------------------

ingest_positions <- function(path) {
  if (!file.exists(path)) {
    message("Warning: Positions file not found at ", path)
    return(tibble::tibble(
      player_id       = character(),
      pos_player_name = character(),
      pos_group       = character(),
      pos_position    = character(),
      pos_year        = integer()
    ))
  }
  
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    pos_raw <- readRDS(path)
  } else {
    pos_raw <- read_csv(path, show_col_types = FALSE)
  }

  # Normalize to a single 'name' column regardless of source format
  if (!"name" %in% names(pos_raw)) {
    if (all(c("firstName", "lastName") %in% names(pos_raw))) {
      pos_raw$name <- paste(pos_raw$firstName, pos_raw$lastName)
    } else if ("firstName" %in% names(pos_raw)) {
      pos_raw$name <- pos_raw$firstName
    } else if ("lastName" %in% names(pos_raw)) {
      pos_raw$name <- pos_raw$lastName
    } else {
      stop("profiles_with_groups.rds: no name, firstName, or lastName column found")
    }
  }

  pos_raw <- pos_raw %>%
    rename_with(~ case_when(
      . == "groupName"          ~ "Group",
      . == "positionName"       ~ "Position",
      tolower(.) == "group"     ~ "Group",
      tolower(.) == "position"  ~ "Position",
      tolower(.) == "firstname" ~ "firstName",
      tolower(.) == "lastname"  ~ "lastName",
      TRUE ~ .
    ))

  pos_raw <- pos_raw %>%
    mutate(
      player_name = str_squish(name),
      player_id   = standardize_name(player_name),
      Group       = na_if(str_squish(as.character(Group)), ""),
      Position    = na_if(str_squish(as.character(Position)), ""),
      year        = suppressWarnings(as.integer(year))
    )
  
  pos_raw %>%
    group_by(player_id) %>%
    summarise(
      pos_player_name = first(na.omit(player_name)),
      pos_group       = dplyr::coalesce(first(na.omit(Group)),    NA_character_),
      pos_position    = dplyr::coalesce(first(na.omit(Position)), NA_character_),
      pos_year        = dplyr::coalesce(first(na.omit(year)),     NA_integer_),
      .groups = "drop"
    )
}

# ---------------------------
# Measurements ingestion
# ---------------------------

ingest_measurements <- function(path) {
  if (!file.exists(path)) {
    message("Warning: Measurements file not found at ", path)
    return(tibble::tibble(
      player_id        = character(),
      meas_player_name = character(),
      pos_group        = character(),
      pos_position     = character(),
      class_year       = character(),
      class_year_base  = character(),
      is_redshirt      = character(),
      height_display   = character(),
      weight_display   = character(),
      wingspan_display = character(),
      hand_display     = character(),
      arm_display      = character()
    ))
  }
  
  meas_raw <- readRDS(path)

  # Normalize to a single 'name' column regardless of source format
  if (!"name" %in% names(meas_raw)) {
    if (all(c("firstName", "lastName") %in% names(meas_raw))) {
      meas_raw$name <- paste(meas_raw$firstName, meas_raw$lastName)
    } else if ("firstName" %in% names(meas_raw)) {
      meas_raw$name <- meas_raw$firstName
    } else if ("lastName" %in% names(meas_raw)) {
      meas_raw$name <- meas_raw$lastName
    } else {
      stop("measurements.rds: no name, firstName, or lastName column found")
    }
  }

  meas_raw <- meas_raw %>%
    mutate(
      player_name = str_squish(name),
      player_id   = standardize_name(player_name),
      
      Group    = na_if(str_squish(as.character(Group)),    ""),
      Position = na_if(str_squish(as.character(Position)), ""),
      Year     = na_if(str_squish(as.character(Year)),     ""),
      Height   = na_if(str_squish(as.character(Height)),   ""),
      Weight   = na_if(str_squish(as.character(Weight)),   ""),
      Wingspan = na_if(str_squish(as.character(Wingspan)), ""),
      Hand     = na_if(str_squish(as.character(Hand)),     ""),
      Arm      = na_if(str_squish(as.character(Arm)),      "")
    ) %>%
    mutate(
      class_year      = Year %>% str_replace_all("\\*", "") %>% str_squish(),
      is_redshirt     = if_else(!is.na(class_year) & str_detect(class_year, "^RS-"), "Yes", "No"),
      class_year_base = class_year %>%
        str_replace("^RS-", "") %>%
        str_to_upper() %>%
        na_if("")
    )
  
  meas_raw %>%
    group_by(player_id) %>%
    summarise(
      meas_player_name = first(na.omit(player_name)),
      pos_group        = dplyr::coalesce(first(na.omit(Group)),          NA_character_),
      pos_position     = dplyr::coalesce(first(na.omit(Position)),       NA_character_),
      class_year       = dplyr::coalesce(first(na.omit(class_year)),     NA_character_),
      class_year_base  = dplyr::coalesce(first(na.omit(class_year_base)),NA_character_),
      is_redshirt      = dplyr::coalesce(first(na.omit(is_redshirt)),    NA_character_),
      height_display   = dplyr::coalesce(first(na.omit(Height)),         NA_character_),
      weight_display   = dplyr::coalesce(first(na.omit(Weight)),         NA_character_),
      wingspan_display = dplyr::coalesce(first(na.omit(Wingspan)),       NA_character_),
      hand_display     = dplyr::coalesce(first(na.omit(Hand)),           NA_character_),
      arm_display      = dplyr::coalesce(first(na.omit(Arm)),            NA_character_),
      .groups = "drop"
    )
}

# ---------------------------
# NordBord ingestion
# ---------------------------

ingest_nordboard <- function(path) {
  if (!file.exists(path)) {
    message("Warning: NordBord file not found at ", path)
    return(tibble::tibble())
  }
  
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    raw <- readRDS(path)
  } else {
    raw <- read_csv(path, show_col_types = FALSE)
  }
  
  raw <- raw %>%
    mutate(
      date = if (inherits(date, "Date")) date else {
        parsed <- suppressWarnings(lubridate::ymd(date))
        if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::mdy(date))
        if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::dmy(date))
        parsed
      },
      datetime    = as.POSIXct(date),
      player_name = fix_player_name(str_squish(coalesce(name, paste(firstName, lastName)))),
      player_id   = standardize_name(player_name),
      source      = "NordBord",
      test_type   = coalesce(testType, "Nordic Hamstring")
    )
  
  id_cols <- c("player_id", "player_name", "date", "datetime", "source", "test_type")
  
  metric_cols <- intersect(
    c("leftAvgForce", "leftImpulse", "leftMaxForce",
      "rightAvgForce", "rightImpulse", "rightMaxForce",
      "asymmetry", "avg_max_force"),
    names(raw)
  )
  
  if (length(metric_cols) == 0) {
    message("Warning: No metric columns found in NordBord data")
    return(tibble::tibble())
  }
  
  raw %>%
    select(any_of(c(id_cols, metric_cols))) %>%
    pivot_longer(
      cols            = all_of(metric_cols),
      names_to        = "metric_raw",
      values_to       = "metric_value_raw",
      values_transform = list(metric_value_raw = as.character)
    ) %>%
    mutate(
      metric_name = case_when(
        metric_raw == "leftAvgForce"  ~ "L Avg Force",
        metric_raw == "leftImpulse"   ~ "L Max Impulse",
        metric_raw == "leftMaxForce"  ~ "L Max Force",
        metric_raw == "rightAvgForce" ~ "R Avg Force",
        metric_raw == "rightImpulse"  ~ "R Max Impulse",
        metric_raw == "rightMaxForce" ~ "R Max Force",
        metric_raw == "asymmetry"     ~ "Max Imbalance",
        metric_raw == "avg_max_force" ~ "Avg Max Force",
        TRUE ~ metric_raw
      ),
      units = case_when(
        str_detect(metric_raw, "Force")   ~ "N",
        str_detect(metric_raw, "Impulse") ~ "Ns",
        metric_raw == "asymmetry"         ~ "%",
        TRUE ~ NA_character_
      ),
      metric_value = parse_metric_numeric(metric_value_raw)
    ) %>%
    filter(!is.na(metric_value)) %>%
    select(-metric_value_raw)
}

# ---------------------------
# ForceDecks ingestion
# ---------------------------

ingest_forcedecks <- function(path) {
  if (!file.exists(path)) {
    message("Warning: ForceDecks file not found at ", path)
    return(tibble::tibble())
  }
  
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    raw <- readRDS(path)
  } else {
    raw <- read_csv(path, show_col_types = FALSE)
  }
  
  raw <- raw %>%
    mutate(
      date = if (inherits(date, "Date")) date else {
        parsed <- suppressWarnings(lubridate::ymd(date))
        if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::mdy(date))
        if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::dmy(date))
        parsed
      },
      datetime    = as.POSIXct(date),
      player_name = fix_player_name(str_squish(coalesce(name, paste(firstName, lastName)))),
      player_id   = standardize_name(player_name),
      source      = "ForceDecks",
      test_type   = coalesce(testType, "CMJ")
    )
  
  id_cols <- c("player_id", "player_name", "date", "datetime", "source", "test_type")
  
  non_metric <- c(
    "profileId", "testId", "firstName", "lastName", "name",
    "groupName", "positionName", "testType", "date", "year",
    "player_id", "player_name", "datetime", "source", "test_type"
  )
  
  metric_cols <- setdiff(names(raw), non_metric)
  metric_cols <- metric_cols[sapply(raw[metric_cols], function(x) is.numeric(x) || is.character(x))]
  
  if (length(metric_cols) == 0) {
    message("Warning: No metric columns found in ForceDecks data")
    return(tibble::tibble())
  }
  
  raw %>%
    select(any_of(c(id_cols, metric_cols))) %>%
    pivot_longer(
      cols            = all_of(metric_cols),
      names_to        = "metric_raw",
      values_to       = "metric_value_raw",
      values_transform = list(metric_value_raw = as.character)
    ) %>%
    mutate(
      metric_name  = metric_raw %>% str_replace_all("\\.", " ") %>% str_squish(),
      units        = parse_units(metric_raw),
      metric_value = parse_metric_numeric(metric_value_raw)
    ) %>%
    filter(!is.na(metric_value)) %>%
    select(-metric_value_raw)
}

# ---------------------------
# Catapult ingestion
# ---------------------------

ingest_catapult <- function(path) {
  if (!file.exists(path)) {
    message("Warning: Catapult file not found at ", path)
    return(tibble::tibble())
  }
  
  raw <- readRDS(path) %>%
    rename_with(~ str_squish(.))
  
  raw <- raw %>%
    mutate(
      date = if (inherits(date, "Date")) date else {
        parsed <- suppressWarnings(lubridate::ymd(date))
        if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::mdy(date))
        if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::dmy(date))
        parsed
      },
      datetime    = as.POSIXct(date),
      player_name = fix_player_name(str_squish(athlete_name)),
      player_id   = standardize_name(player_name),
      source      = "Catapult",
      test_type   = "Catapult"
    )
  
  id_cols <- c("player_id", "player_name", "date", "datetime", "source", "test_type")
  
  non_metric <- c(
    "athlete_name", "activity_name", "period_id", "period_name",
    "date", "datetime", "player_name", "player_id", "source", "test_type"
  )
  
  metric_cols <- setdiff(names(raw), non_metric)
  
  if (length(metric_cols) == 0) {
    message("Warning: No metric columns found in Catapult data")
    return(tibble::tibble())
  }
  
  long <- raw %>%
    select(any_of(c(id_cols, metric_cols))) %>%
    pivot_longer(
      cols            = all_of(metric_cols),
      names_to        = "metric_raw",
      values_to       = "metric_value_raw",
      values_transform = list(metric_value_raw = as.character)
    ) %>%
    mutate(
      units        = NA_character_,
      metric_name  = metric_raw %>%
        str_replace_all("_", " ") %>%
        str_squish() %>%
        str_to_title(),
      metric_value = parse_metric_numeric(metric_value_raw)
    ) %>%
    mutate(
      metric_value = case_when(
        metric_name %in% c("Explosive Efforts", "Total Explosive Efforts") & metric_value > 40  ~ NA_real_,
        metric_name %in% c("Max V", "Max Vel", "Max Velocity")             & metric_value > 24  ~ NA_real_,
        TRUE ~ metric_value
      )
    ) %>%
    filter(!is.na(metric_value)) %>%
    select(-metric_value_raw)
  
  # Normalise metric names and keep only the ones we care about
  long <- long %>%
    mutate(
      metric_keep = case_when(
        metric_name %in% c("Total Player Load", "Player Load", "Total Load")       ~ "Total Player Load",
        metric_name %in% c("Explosive Efforts", "Total Explosive Efforts")         ~ "Explosive Efforts",
        str_detect(metric_name, "^High Speed Distance") &
          str_detect(metric_name, "12mph")                                         ~ "High Speed Distance (12 mph)",
        str_detect(metric_name, "^Sprint Distance") &
          str_detect(metric_name, "16mph")                                         ~ "Sprint Distance (16 mph)",
        metric_name %in% c("Max Vel", "Max V", "Max Velocity")                     ~ "Max Vel",
        metric_name %in% c("Max Effort Acceleration", "Max Effort Accel")          ~ "Max Effort Acceleration",
        metric_name %in% c("Max Effort Deceleration", "Max Effort Decel")          ~ "Max Effort Deceleration",
        metric_name %in% c("Total Duration", "Duration", "Total Time")             ~ "Total Duration",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(metric_keep)) %>%
    mutate(metric_name = metric_keep) %>%
    select(-metric_keep)
  
  # Aggregate across multiple sessions per day
  sum_metrics      <- c("Total Player Load", "Explosive Efforts",
                        "High Speed Distance (12 mph)", "Sprint Distance (16 mph)",
                        "Total Duration")
  peak_max_metrics <- c("Max Vel", "Max Effort Acceleration")
  peak_min_metrics <- c("Max Effort Deceleration")
  
  summed <- long %>%
    filter(metric_name %in% sum_metrics) %>%
    group_by(player_id, player_name, date, source, test_type, metric_name) %>%
    summarise(metric_value = sum(metric_value, na.rm = TRUE), .groups = "drop") %>%
    mutate(datetime = as.POSIXct(date), units = NA_character_)
  
  maxed <- long %>%
    filter(metric_name %in% peak_max_metrics) %>%
    group_by(player_id, player_name, date, source, test_type, metric_name) %>%
    summarise(metric_value = max(metric_value, na.rm = TRUE), .groups = "drop") %>%
    mutate(datetime = as.POSIXct(date), units = NA_character_)
  
  minned <- long %>%
    filter(metric_name %in% peak_min_metrics) %>%
    group_by(player_id, player_name, date, source, test_type, metric_name) %>%
    summarise(metric_value = min(metric_value, na.rm = TRUE), .groups = "drop") %>%
    mutate(datetime = as.POSIXct(date), units = NA_character_)
  
  # Daily Player Load Per Minute
  daily_plmin <- summed %>%
    filter(metric_name %in% c("Total Player Load", "Total Duration")) %>%
    select(player_id, player_name, date, source, test_type, metric_name, metric_value) %>%
    pivot_wider(names_from = metric_name, values_from = metric_value) %>%
    mutate(
      duration_min = case_when(
        is.na(`Total Duration`)      ~ NA_real_,
        `Total Duration` > 1000      ~ `Total Duration` / 60,
        TRUE                         ~ `Total Duration`
      ),
      metric_name  = "Player Load Per Minute",
      metric_value = `Total Player Load` / duration_min
    ) %>%
    filter(!is.na(metric_value) & is.finite(metric_value)) %>%
    mutate(datetime = as.POSIXct(date), units = NA_character_) %>%
    select(player_id, player_name, date, datetime, source, test_type,
           metric_name, metric_value, units)
  
  bind_rows(
    summed %>% filter(metric_name != "Total Duration"),
    maxed,
    minned,
    daily_plmin
  ) %>%
    mutate(metric_value = round(metric_value, 2)) %>%
    select(all_of(c("player_id", "player_name", "date", "datetime",
                    "source", "test_type", "metric_name", "metric_value", "units")))
}


# ---------------------------
# SmartSpeed ingestion
# ---------------------------

ingest_smartspeed <- function(path) {
  if (!file.exists(path)) {
    message("Warning: SmartSpeed file not found at ", path)
    return(tibble::tibble())
  }
  
  # support both .rds and .csv
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    raw <- readRDS(path)
  } else {
    raw <- readr::read_csv(path, show_col_types = FALSE)
  }
  
  raw %>%
    mutate(
      date = if ("testDate" %in% names(.)) {
        if (inherits(testDate, "Date")) testDate else {
          parsed <- suppressWarnings(lubridate::ymd(testDate))
          if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::mdy(testDate))
          if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::dmy(testDate))
          parsed
        }
      } else if ("testDateUtc" %in% names(.)) {
        as.Date(testDateUtc)
      } else {
        as.Date(NA)
      },
      datetime    = as.POSIXct(date),
      player_name = fix_player_name(str_squish(name)),
      player_id   = standardize_name(player_name),
      source      = "SmartSpeed",
      test_type   = coalesce(testName, testTypeName, "SmartSpeed"),
      metric_name = "Best Split Seconds",
      metric_value = suppressWarnings(as.numeric(bestSplitSeconds)),
      units       = "s"
    ) %>%
    filter(!is.na(metric_value)) %>%
    select(
      player_id, player_name, date, datetime,
      source, test_type, metric_name, metric_value, units
    )
}

# ---------------------------
# Manual Overrides ingestion (Vertical Jump, Squat, Bench, Clean)
# ---------------------------

ingest_manual_overrides <- function(path) {
  if (!file.exists(path)) {
    message("Warning: manual_overrides.rds not found at ", path)
    return(tibble::tibble())
  }
  
  raw <- readRDS(path)
  # Normalize column names to lower-case for robustness
  names(raw) <- tolower(names(raw))
  
  if (!all(c("name", "date", "test", "value") %in% names(raw))) {
    message("Warning: manual_overrides.rds missing expected columns (Name, Date, Test, Value)")
    return(tibble::tibble())
  }
  
  raw %>%
    mutate(
      player_name  = fix_player_name(str_squish(as.character(name))),
      player_id    = standardize_name(player_name),
      date         = {
        d <- as.character(date)
        parsed <- suppressWarnings(lubridate::ymd(d))
        if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::mdy(d))
        if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::dmy(d))
        parsed
      },
      datetime     = as.POSIXct(date),
      source       = "Lifts",
      test_type    = "Strength",
      metric_name  = str_squish(as.character(test)),
      metric_value = suppressWarnings(as.numeric(as.character(value))),
      units        = case_when(
        str_detect(metric_name, "Vertical Jump") ~ "in",
        TRUE                                     ~ "lbs"
      )
    ) %>%
    filter(!is.na(metric_value), metric_name %in% OVERRIDE_TESTS) %>%
    select(player_id, player_name, date, datetime,
           source, test_type, metric_name, metric_value, units)
}

# ============================================================
# LOAD ALL DATA SOURCES
# ============================================================

nord       <- ingest_nordboard(NORD_PATH)
force      <- ingest_forcedecks(FORCE_PATH)
catapult   <- ingest_catapult(CAT_PATH)
smartspeed <- ingest_smartspeed(SMART_PATH)
overrides  <- ingest_manual_overrides(OVERRIDES_PATH)

vald_tests_long <- bind_rows(nord, force, catapult, smartspeed, overrides) %>%
  mutate(
    metric_value = as.numeric(metric_value),
    metric_value = round(metric_value, 3)
  )

# Separate catapult reference kept for compatibility
catapult_tests_long <- catapult %>%
  mutate(
    metric_value = as.numeric(metric_value),
    metric_value = round(metric_value, 2)
  )

catapult_tests_long_ui <- catapult_tests_long %>%
  mutate(metric_key = paste(source, test_type, metric_name, sep = "|"))

# ============================================================
# PLAYER METADATA
# ============================================================

positions    <- ingest_positions(POS_PATH)
measurements <- ingest_measurements(MEAS_PATH)

# Prefer measurements for pos_group / pos_position
player_metadata <- positions %>%
  full_join(measurements, by = "player_id", suffix = c("_pos", "_meas")) %>%
  mutate(
    pos_player_name = coalesce(pos_player_name, meas_player_name),
    pos_group       = coalesce(pos_group_meas,    pos_group_pos),
    pos_position    = coalesce(pos_position_meas, pos_position_pos)
  ) %>%
  select(
    player_id, pos_player_name, pos_group, pos_position, pos_year,
    class_year, class_year_base, is_redshirt,
    height_display, weight_display, wingspan_display, hand_display, arm_display
  )

# Unique player directory
players <- vald_tests_long %>%
  distinct(player_id, player_name) %>%
  arrange(player_name) %>%
  left_join(player_metadata, by = "player_id")

# Enrich long table with metadata and add metric_key
vald_tests_long <- vald_tests_long %>%
  left_join(player_metadata, by = "player_id")

vald_tests_long_ui <- vald_tests_long %>%
  mutate(metric_key = paste(source, test_type, metric_name, sep = "|"))

# ============================================================
# BEST and LATEST per player × metric_key (as-of)
# ============================================================

base <- vald_tests_long_ui %>%
  filter(date <= as_of_date) %>%
  mutate(val_num = suppressWarnings(as.numeric(metric_value)))

latest_long <- base %>%
  group_by(player_id, player_name, metric_key) %>%
  slice_max(date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(player_id, player_name, metric_key, latest_value = val_num)

best_long <- base %>%
  group_by(player_id, player_name, metric_key) %>%
  summarise(
    best_value = {
      x  <- val_num
      mk <- dplyr::first(metric_key)
      if (all(is.na(x))) NA_real_
      else if (is_flip_metric(mk)) min(x, na.rm = TRUE)
      else                         max(x, na.rm = TRUE)
    },
    .groups = "drop"
  )

# ============================================================
# HYBRID ROSTER VALUES
# (BEST for most metrics; LATEST for load/weight metrics)
# ============================================================

roster_values_long <- best_long %>%
  left_join(latest_long, by = c("player_id", "player_name", "metric_key")) %>%
  mutate(
    metric_value_roster = if_else(
      latest_only_metric_name(metric_key),
      latest_value,
      best_value
    )
  ) %>%
  select(player_id, player_name, metric_key, metric_value_roster)

# ============================================================
# STEP 1: Build FULL roster_view (pivot + attach metadata)
# ============================================================

# Define metadata column names used throughout
pos_cols <- c(
  "pos_group", "pos_position",
  "class_year", "class_year_base", "is_redshirt",
  "height_display", "weight_display",
  "wingspan_display", "hand_display", "arm_display"
)
id_cols <- c("player_id", "player_name", pos_cols, "as_of_date")

roster_view <- roster_values_long %>%
  tidyr::pivot_wider(names_from = metric_key, values_from = metric_value_roster) %>%
  left_join(
    player_metadata %>% select(player_id, all_of(pos_cols)),
    by = "player_id"
  ) %>%
  mutate(as_of_date = as_of_date)

# Derive metric column names from the full (unfiltered) roster_view
metric_cols <- setdiff(names(roster_view), id_cols)

# ============================================================
# STEP 2: Compute fill_summary and keep_roster_metrics
# (uses full roster_view and correct metric_cols)
# ============================================================

sources <- sort(unique(vald_tests_long_ui$source))

fill_summary <- lapply(sources, function(s) {
  
  players_s   <- vald_tests_long_ui %>%
    filter(source == s, date <= as_of_date) %>%
    distinct(player_id)
  
  metric_cols_s <- metric_cols[startsWith(metric_cols, paste0(s, "|"))]
  if (length(metric_cols_s) == 0) return(tibble::tibble())
  
  wide_s <- roster_view %>%
    filter(player_id %in% players_s$player_id) %>%
    select(any_of(c("player_id", "player_name", "as_of_date", metric_cols_s)))
  
  wide_s %>%
    summarise(
      across(
        all_of(metric_cols_s),
        list(
          fill_frac = ~ mean(!is.na(.)),
          zero_frac = ~ { x <- .[!is.na(.)]; if (length(x) == 0) NA_real_ else mean(x == 0) },
          n_unique  = ~ { x <- .[!is.na(.)]; length(unique(x)) }
        ),
        .names = "{.col}__{.fn}"
      )
    ) %>%
    pivot_longer(
      cols      = everything(),
      names_to  = c("metric_key", "stat"),
      names_sep = "__",
      values_to = "value"
    ) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    mutate(source = s) %>%
    arrange(fill_frac)
  
}) %>% bind_rows()

keep_roster_metrics <- fill_summary %>%
  filter(
    fill_frac >= filled_threshold,
    n_unique  >  1,
    is.na(zero_frac) | zero_frac <= 0.90
  ) %>%
  pull(metric_key)

force_include_keys <- c(
  "Catapult|Catapult|Explosive Efforts",
  "Catapult|Catapult|High Speed Distance (12 mph)",
  "Catapult|Catapult|Sprint Distance (16 mph)",
  "SmartSpeed|Flying 10s|Best Split Seconds",
  paste0("Lifts|Strength|", OVERRIDE_TESTS)
)

keep_roster_metrics <- unique(c(keep_roster_metrics, force_include_keys))

# ============================================================
# STEP 3: Build vald_best_wide
# (must exist before roster_best_view)
# ============================================================

vald_best_wide <- vald_tests_long_ui %>%
  filter(date <= as_of_date) %>%
  group_by(player_id, player_name, metric_key) %>%
  summarise(
    metric_value = {
      x  <- suppressWarnings(as.numeric(metric_value))
      mk <- dplyr::first(metric_key)
      if (all(is.na(x))) NA_real_
      else if (is_flip_metric(mk)) min(x, na.rm = TRUE)
      else                         max(x, na.rm = TRUE)
    },
    .groups = "drop"
  ) %>%
  left_join(
    player_metadata %>%
      select(player_id, pos_group, pos_position,
             class_year, class_year_base, is_redshirt,
             height_display, weight_display,
             wingspan_display, hand_display, arm_display),
    by = "player_id"
  ) %>%
  pivot_wider(names_from = metric_key, values_from = metric_value) %>%
  mutate(as_of_date = as_of_date)


# ============================================================
# STEP 4: Filter roster_view to kept metrics + round
# ============================================================

roster_view <- roster_view %>%
  select(any_of(c(id_cols, keep_roster_metrics))) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# ============================================================
# STEP 5: Build roster_best_view (vald_best_wide now exists)
# ============================================================

roster_best_view <- vald_best_wide %>%
  select(any_of(c(id_cols, keep_roster_metrics))) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# ============================================================
# STEP 6: Build vald_latest_wide (all metrics, latest value)
# ============================================================

vald_latest_wide <- vald_tests_long_ui %>%
  filter(date <= as_of_date) %>%
  group_by(player_id, player_name, metric_key) %>%
  slice_max(date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(
    player_id, player_name,
    pos_group, pos_position,
    class_year, class_year_base, is_redshirt,
    height_display, weight_display,
    wingspan_display, hand_display, arm_display,
    metric_key, metric_value
  ) %>%
  pivot_wider(names_from = metric_key, values_from = metric_value) %>%
  mutate(as_of_date = as_of_date)

# ============================================================
# STEP 7: Position-based percentiles
# - Best-all-time for ForceDecks, NordBord, and peak Catapult metrics
# - Most-recent for Catapult load/distance/volume metrics
# ============================================================

# Keys where most-recent is more appropriate than best
# latest_preferred_keys <- keep_roster_metrics[
#   grepl("Total Player Load|Player Load Per Minute|High Speed Distance|Sprint Distance|Explosive Efforts",
#         keep_roster_metrics, ignore.case = TRUE) &
#     startsWith(keep_roster_metrics, "Catapult|")
# ]

latest_preferred_keys <- keep_roster_metrics[
  grepl(
    "Athlete Standing Weight|Total Player Load|Player Load Per Minute|High Speed Distance|Sprint Distance|Explosive Efforts|Total Distance",
    keep_roster_metrics,
    ignore.case = TRUE
  )
]

best_only_keys <- setdiff(
  intersect(keep_roster_metrics, names(vald_best_wide)),
  latest_preferred_keys
)

pos_pct_col <- if ("pos_position" %in% names(vald_best_wide)) "pos_position" else NULL

roster_percentiles_pos_long  <- NULL
roster_percentiles_pos_wide  <- NULL
roster_best_percentiles_pos_long <- NULL
roster_best_percentiles_pos_wide <- NULL

if (!is.null(pos_pct_col)) {
  
  # --- Best-all-time percentiles (ForceDecks, NordBord, Max Vel, Acc, Dec) ---
  best_pcts <- vald_best_wide %>%
    select(player_id, player_name, !!pos_pct_col, any_of(best_only_keys)) %>%
    pivot_longer(
      cols      = any_of(best_only_keys),
      names_to  = "metric_key",
      values_to = "metric_value"
    ) %>%
    group_by(metric_key, .data[[pos_pct_col]]) %>%
    mutate(
      percentile = if_else(
        is_flip_metric(metric_key),
        pct_rank_100(-metric_value),
        pct_rank_100(metric_value)
      )
    ) %>%
    ungroup()
  
  # --- Most-recent percentiles (Catapult load/distance/volume metrics) ---
  latest_raw <- vald_tests_long_ui %>%
    filter(
      date <= as_of_date,
      metric_key %in% latest_preferred_keys
    ) %>%
    mutate(val_num = suppressWarnings(as.numeric(metric_value))) %>%
    group_by(player_id, player_name, metric_key) %>%
    slice_max(date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    filter(!is.na(val_num), !is.na(pos_position))
  
  latest_pcts <- latest_raw %>%
    select(player_id, player_name, pos_position, metric_key, val_num) %>%
    rename(metric_value = val_num) %>%
    group_by(metric_key, pos_position) %>%
    mutate(
      percentile = if_else(
        is_flip_metric(metric_key),
        pct_rank_100(-metric_value),
        pct_rank_100(metric_value)
      )
    ) %>%
    ungroup() %>%
    rename(!!pos_pct_col := pos_position)
  
  # --- Combine ---
  roster_best_percentiles_pos_long <- bind_rows(best_pcts, latest_pcts)
  roster_percentiles_pos_long      <- roster_best_percentiles_pos_long
  
  roster_best_percentiles_pos_wide <- roster_best_percentiles_pos_long %>%
    select(player_id, metric_key, percentile) %>%
    pivot_wider(names_from = metric_key, values_from = percentile)
  
  roster_percentiles_pos_wide <- roster_best_percentiles_pos_wide
}

roster_percentiles_wide <- roster_view %>%
  select(player_id) %>%
  left_join(roster_percentiles_pos_wide, by = "player_id")

roster_percentiles_long <- roster_best_percentiles_pos_long %>%
  transmute(player_id, player_name, metric_key, percentile)

# ============================================================
# STEP 9: Athleticism Score (weighted position percentiles)
# ============================================================

athleticism_key <- "Composite|Score|Athleticism Score"

find_key <- function(source_name, test_type = NULL, metric_name) {
  cand <- keep_roster_metrics[startsWith(keep_roster_metrics, paste0(source_name, "|"))]
  if (length(cand) == 0) return(NA_character_)
  
  parts <- strsplit(cand, "\\|")
  cand_source <- vapply(parts, function(x) x[1], character(1))
  cand_test   <- vapply(parts, function(x) if (length(x) >= 2) x[2] else NA_character_, character(1))
  cand_metric <- vapply(parts, function(x) if (length(x) >= 3) x[3] else NA_character_, character(1))
  
  norm <- function(s) {
    s %>%
      stringr::str_squish() %>%
      stringr::str_to_lower() %>%
      stringr::str_replace_all("[\\(\\)\\[\\]]", "") %>%
      stringr::str_replace_all("[^a-z0-9\\s\\-]", "") %>%
      stringr::str_squish()
  }
  
  keep <- norm(cand_source) == norm(source_name) &
    norm(cand_metric) == norm(metric_name)
  
  if (!is.null(test_type)) {
    keep <- keep & norm(cand_test) == norm(test_type)
  }
  
  hits <- cand[keep]
  if (length(hits) >= 1) hits[1] else NA_character_
}

find_first_existing <- function(source_name, metric_names, test_type = NULL) {
  for (nm in metric_names) {
    k <- find_key(source_name, test_type = test_type, metric_name = nm)
    if (!is.na(k)) return(k)
  }
  NA_character_
}

# ForceDecks keys
k_jump <- find_first_existing("ForceDecks", c(
  "Jump Height (Imp-Mom) in Inches", "Jump Height (Imp-Mom)",
  "Jump Height Imp-Mom in Inches",   "Jump Height Imp-Mom", "Jump Height"
))
k_rsi <- find_first_existing("ForceDecks", c(
  "RSI-modified (Imp-Mom)", "RSI-modified", "RSI Modified",
  "RSI Modified (Imp-Mom)", "RSI-modified (Imp-Mom) in Inches"
))
k_ebi <- find_first_existing("ForceDecks", c("Eccentric Braking Impulse"))
k_pp  <- find_first_existing("ForceDecks", c("Force at Peak Power"))

# NordBord keys
k_lmf <- find_first_existing("NordBord", c("L Max Force"))
k_rmf <- find_first_existing("NordBord", c("R Max Force"))
k_imb <- find_first_existing("NordBord", c("Max Imbalance"))

# Catapult keys
k_maxv <- find_first_existing("Catapult", c("Max Vel", "Max Velocity", "Max V"))
k_acc  <- find_first_existing("Catapult", c("Max Effort Acceleration", "Max Effort Accel"))
k_dec  <- find_first_existing("Catapult", c("Max Effort Deceleration", "Max Effort Decel"))

k_fly10 <- find_first_existing(
  "SmartSpeed",
  metric_names = c("Best Split Seconds"),
  test_type = "Flying 10s"
)


keys_needed <- c(k_jump, k_rsi, k_ebi, k_pp, k_lmf, k_rmf, k_imb, k_maxv, k_acc, k_dec, k_fly10)
keys_needed <- keys_needed[!is.na(keys_needed)]

pcts_wide <- roster_best_percentiles_pos_long %>%
  filter(metric_key %in% keys_needed) %>%
  select(player_id, player_name, metric_key, percentile) %>%
  tidyr::pivot_wider(names_from = metric_key, values_from = percentile)

w <- c(
  jump = 0.125, rsi = 0.10, ebi = 0.10, pp = 0.125,
  mf   = 0.15,  imb = 0.05, maxv = 0.10, fly10 = 0.10, acc = 0.075, dec = 0.075
)

safe_col <- function(df, key) {
  if (is.na(key) || !key %in% names(df)) return(rep(NA_real_, nrow(df)))
  suppressWarnings(as.numeric(df[[key]]))
}

ath <- pcts_wide %>%
  mutate(
    jump = safe_col(., k_jump),
    rsi  = safe_col(., k_rsi),
    ebi  = safe_col(., k_ebi),
    pp   = safe_col(., k_pp),
    maxv = safe_col(., k_maxv),
    acc  = safe_col(., k_acc),
    dec  = safe_col(., k_dec),
    fly10 = safe_col(., k_fly10),
    lmf  = safe_col(., k_lmf),
    rmf  = safe_col(., k_rmf),
    mf   = ifelse(
      !is.na(lmf) | !is.na(rmf),
      rowMeans(cbind(lmf, rmf), na.rm = TRUE),
      NA_real_
    ),
    imb = safe_col(., k_imb),
    
    AthleticismScore = {
      vs <- cbind(jump, rsi, ebi, pp, mf, imb, maxv, fly10, acc, dec)
      colnames(vs) <- names(w)
      apply(vs, 1, function(v) {
        ok <- !is.na(v)
        if (!any(ok)) return(NA_real_)
        sum(w[ok] * v[ok]) / sum(w[ok])
      })
    }
  ) %>%
  left_join(player_metadata %>% select(player_id, pos_position), by = "player_id") %>%
  mutate(
    AthleticismScore = ifelse(
      grepl("^SP", pos_position %||% ""),
      NA_real_,
      AthleticismScore
    )
  ) %>%
  transmute(
    player_id,
    player_name,
    AthleticismScore = round(AthleticismScore, 1)
  )

# Attach Athleticism Score to roster_view
roster_view <- roster_view %>%
  left_join(ath, by = c("player_id", "player_name")) %>%
  mutate(!!athleticism_key := AthleticismScore) %>%
  select(-AthleticismScore)

roster_best_view <- roster_best_view %>%
  left_join(ath, by = c("player_id","player_name")) %>%
  mutate(!!athleticism_key := AthleticismScore) %>%
  select(-AthleticismScore)

keep_roster_metrics <- unique(c(keep_roster_metrics, athleticism_key))

# Add Athleticism Score to percentiles long table
roster_percentiles_long <- roster_percentiles_long %>%
  bind_rows(
    ath %>% transmute(
      player_id, player_name,
      metric_key = athleticism_key,
      percentile = AthleticismScore
    )
  )

# ============================================================
# Summary messages
# ============================================================

message("Built objects:")
message("  vald_tests_long:       ", nrow(vald_tests_long),  " rows (NordBord + ForceDecks + Catapult)")
message("  players:               ", nrow(players),           " players")
message("  vald_latest_wide:      ", nrow(vald_latest_wide),  " rows, ", length(metric_cols), " metrics")
message("  vald_best_wide:        ", nrow(vald_best_wide),    " rows")
message("  roster metrics kept:   ", length(keep_roster_metrics),
        " (>= ", filled_threshold * 100, "% filled)")
message("  roster_view:           ", nrow(roster_view),       " rows")
message("  roster_best_view:      ", nrow(roster_best_view),  " rows")
message("  roster_percentiles_*:  computed for kept roster metrics")
