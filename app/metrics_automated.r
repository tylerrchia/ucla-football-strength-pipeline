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
#   - acwr_per_player       : acute:chronic workload ratio per player

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
DATA_DIR <- Sys.getenv("DATA_OUTPUT_DIR", unset = "")
if (!nzchar(DATA_DIR) || !dir.exists(DATA_DIR)) {
  if (dir.exists("data-repo/data")) {
    DATA_DIR <- "data-repo/data"
  } else if (dir.exists("data")) {
    DATA_DIR <- "data"
  } else {
    DATA_DIR <- "data-repo/data"
  }
}

NORD_PATH      <- file.path(DATA_DIR, "nordbord.rds")
FORCE_PATH     <- file.path(DATA_DIR, "forcedecks.rds")
POS_PATH       <- file.path(DATA_DIR, "profiles_with_groups.rds")
CAT_PATH       <- file.path(DATA_DIR, "catapult.rds")
MEAS_PATH      <- file.path(DATA_DIR, "measurements.rds")
SMART_PATH     <- file.path(DATA_DIR, "smartspeed.rds")
OVERRIDES_PATH <- file.path(DATA_DIR, "manual_overrides.rds")
FORCEFRAME_PATH <- file.path(DATA_DIR, "forceframe.rds")

OVERRIDE_TESTS <- c("Vertical Jump", "Squat", "Bench", "Clean")

force_include_keys <- character(0)

NAME_FIXES <- c(
  "Player A"    = "Player A",
  "Player A"   = "Player A",
  "Player B"     = "Player B",
  "Player C"      = "Player C",
  "Player E"    = "Player E",
  "Player E"   = "Player E",
  "Player F"   = "Player F"
)

# ---------------------------
# Helpers
# ---------------------------

fix_player_name <- function(nm) {
  upper <- toupper(nm)
  idx   <- match(upper, names(NAME_FIXES))
  ifelse(!is.na(idx), NAME_FIXES[idx], nm)
}

standardize_name <- function(nm) {
  tolower(stringr::str_replace_all(nm, "[^a-zA-Z0-9]", "_"))
}

compute_percentile <- function(x, lower_is_better = FALSE) {
  n <- length(x)
  if (n == 0) return(numeric(0))
  non_na <- sum(!is.na(x))
  if (non_na == 0) return(rep(NA_real_, n))
  if (non_na == 1) return(ifelse(is.na(x), NA_real_, 50))
  r <- rank(x, ties.method = "average", na.last = "keep")
  pct <- (r - 0.5) / non_na * 100
  if (lower_is_better) pct <- 100 - pct
  pct
}

is_lower_better <- function(metric_key) {
  grepl("Imbalance|Deceleration|Best Split Seconds|Asymmetry", metric_key, ignore.case = TRUE)
}

is_most_recent <- function(nm) {
  nm %in% c("Total Player Load", "Total Distance", "Athlete Standing Weight",
            "Nordic Asymmetry", "ISO Asymmetry", "Abduction Asymmetry", "Adduction Asymmetry")
}

# ---------------------------
# Player metadata
# ---------------------------

if (file.exists(POS_PATH)) {
  pos_raw <- readRDS(POS_PATH)
} else {
  message("Warning: Position file not found at ", POS_PATH)
  pos_raw <- tibble::tibble(
    player_id = character(), pos_player_name = character(),
    pos_group = character(), pos_position = character(), pos_year = integer()
  )
}

if (file.exists(MEAS_PATH)) {
  meas_raw <- readRDS(MEAS_PATH)
} else {
  message("Warning: Measurements file not found at ", MEAS_PATH)
  meas_raw <- tibble::tibble()
}

# ---------------------------
# Build player_metadata
# ---------------------------

if (nrow(pos_raw) > 0) {
  nm_col <- intersect(c("displayName","fullName","name","player_name","profileName"), names(pos_raw))[1]
  if (!is.na(nm_col)) {
    player_metadata <- pos_raw %>%
      mutate(
        player_name = fix_player_name(str_squish(.data[[nm_col]])),
        player_id   = standardize_name(player_name)
      )
  } else {
    player_metadata <- pos_raw %>% mutate(player_id = character(nrow(.)))
  }
} else {
  player_metadata <- tibble::tibble(player_id = character(), player_name = character())
}

# Normalise column names in player_metadata
if ("groupName" %in% names(player_metadata) && !("pos_group" %in% names(player_metadata))) {
  player_metadata <- player_metadata %>% rename(pos_group = groupName)
}
if ("position" %in% names(player_metadata) && !("pos_position" %in% names(player_metadata))) {
  player_metadata <- player_metadata %>% rename(pos_position = position)
}
if ("classYear" %in% names(player_metadata) && !("class_year" %in% names(player_metadata))) {
  player_metadata <- player_metadata %>% rename(class_year = classYear)
}
if ("classYearBase" %in% names(player_metadata) && !("class_year_base" %in% names(player_metadata))) {
  player_metadata <- player_metadata %>% rename(class_year_base = classYearBase)
}
if ("isRedshirt" %in% names(player_metadata) && !("is_redshirt" %in% names(player_metadata))) {
  player_metadata <- player_metadata %>% rename(is_redshirt = isRedshirt)
}

if ("heightDisplay" %in% names(player_metadata) && !("height_display" %in% names(player_metadata))) {
  player_metadata <- player_metadata %>% rename(height_display = heightDisplay)
}
if ("weightDisplay" %in% names(player_metadata) && !("weight_display" %in% names(player_metadata))) {
  player_metadata <- player_metadata %>% rename(weight_display = weightDisplay)
}
if ("wingspanDisplay" %in% names(player_metadata) && !("wingspan_display" %in% names(player_metadata))) {
  player_metadata <- player_metadata %>% rename(wingspan_display = wingspanDisplay)
}
if ("handDisplay" %in% names(player_metadata) && !("hand_display" %in% names(player_metadata))) {
  player_metadata <- player_metadata %>% rename(hand_display = handDisplay)
}
if ("armDisplay" %in% names(player_metadata) && !("arm_display" %in% names(player_metadata))) {
  player_metadata <- player_metadata %>% rename(arm_display = armDisplay)
}

for (col in c("pos_group","pos_position","class_year","class_year_base","is_redshirt",
              "height_display","weight_display","wingspan_display","hand_display","arm_display")) {
  if (!(col %in% names(player_metadata))) player_metadata[[col]] <- NA
}

# ---------------------------
# Measurements
# ---------------------------

if (nrow(meas_raw) > 0) {
  nm_col_m <- intersect(c("displayName","fullName","name","player_name","profileName"), names(meas_raw))[1]
  if (!is.na(nm_col_m)) {
    meas_clean <- meas_raw %>%
      mutate(
        player_name = fix_player_name(str_squish(.data[[nm_col_m]])),
        player_id   = standardize_name(player_name)
      )
    height_col <- intersect(c("height","heightDisplay","height_display"), names(meas_clean))[1]
    weight_col <- intersect(c("weight","weightDisplay","weight_display"), names(meas_clean))[1]
    if (!is.na(height_col)) player_metadata <- player_metadata %>%
      left_join(meas_clean %>% select(player_id, height_meas = all_of(height_col)) %>% distinct(player_id, .keep_all=TRUE), by="player_id") %>%
      mutate(height_display = coalesce(height_display, as.character(height_meas))) %>%
      select(-any_of("height_meas"))
    if (!is.na(weight_col)) player_metadata <- player_metadata %>%
      left_join(meas_clean %>% select(player_id, weight_meas = all_of(weight_col)) %>% distinct(player_id, .keep_all=TRUE), by="player_id") %>%
      mutate(weight_display = coalesce(weight_display, as.character(weight_meas))) %>%
      select(-any_of("weight_meas"))
  }
}

# ---------------------------
# NordBord ingestion
# ---------------------------

ingest_nordbord <- function(path) {
  if (!file.exists(path)) {
    message("Warning: NordBord file not found at ", path)
    return(tibble::tibble())
  }
  raw <- readRDS(path)

  id_cols    <- c("player_id", "player_name", "date", "datetime", "source", "test_type")
  metric_map <- c(
    leftMaxForce    = "Left Max Force",
    rightMaxForce   = "Right Max Force",
    imbalance       = "Imbalance",
    isoAsymmetry    = "ISO Asymmetry",
    nordicAsymmetry = "Nordic Asymmetry"
  )
  if (!any(names(metric_map) %in% names(raw))) return(tibble::tibble())

  raw %>%
    mutate(
      date = {
        col <- if ("testDate" %in% names(.)) .data[["testDate"]] else if ("testDateUtc" %in% names(.)) .data[["testDateUtc"]] else NA_character_
        parsed <- suppressWarnings(lubridate::ymd_hms(col))
        if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::ymd(col))
        as.Date(parsed)
      },
      datetime    = as.POSIXct(date),
      player_name = fix_player_name(str_squish(coalesce(
        if ("displayName" %in% names(.)) .data[["displayName"]] else NULL,
        if ("name"        %in% names(.)) .data[["name"]]        else NULL,
        if ("profileName" %in% names(.)) .data[["profileName"]] else NULL
      ))),
      player_id   = standardize_name(player_name),
      source      = "NordBord",
      test_type   = coalesce(
        if ("testTypeName" %in% names(.)) .data[["testTypeName"]] else NULL,
        "NordBord"
      )
    ) %>%
    select(any_of(c(id_cols, names(metric_map)))) %>%
    tidyr::pivot_longer(
      cols      = any_of(names(metric_map)),
      names_to  = "metric_raw",
      values_to = "metric_value_raw"
    ) %>%
    filter(!is.na(metric_value_raw)) %>%
    mutate(
      metric_name  = metric_map[metric_raw],
      metric_value = suppressWarnings(as.numeric(metric_value_raw)),
      units        = ""
    ) %>%
    filter(!is.na(metric_value)) %>%
    select(player_id, player_name, date, datetime,
           source, test_type, metric_name, metric_value, units)
}

# ---------------------------
# ForceDecks ingestion
# ---------------------------

ingest_forcedecks <- function(path) {
  if (!file.exists(path)) {
    message("Warning: ForceDecks file not found at ", path)
    return(tibble::tibble())
  }
  raw <- readRDS(path)

  id_cols <- c("player_id", "player_name", "date", "datetime", "source", "test_type")
  metric_cols <- intersect(
    c("jumpHeight","peakPower","peakForce","rsi","reactivestrenindex",
      "eccentricBrakeImpulse","concentricImpulse","peakLandingForce",
      "leftMeanForce","rightMeanForce","limbImbalance","contactTime",
      "flightTime","takeoffVelocity","landingVelocity",
      "leftPeakForce","rightPeakForce",
      "peakPowerPerBodyMass","peakForcePerBodyMass"),
    names(raw)
  )
  if (length(metric_cols) == 0) return(tibble::tibble())

  raw %>%
    mutate(
      date = {
        col <- if ("testDate" %in% names(.)) .data[["testDate"]] else if ("testDateUtc" %in% names(.)) .data[["testDateUtc"]] else NA_character_
        parsed <- suppressWarnings(lubridate::ymd_hms(col))
        if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::ymd(col))
        as.Date(parsed)
      },
      datetime    = as.POSIXct(date),
      player_name = fix_player_name(str_squish(coalesce(
        if ("displayName" %in% names(.)) .data[["displayName"]] else NULL,
        if ("name"        %in% names(.)) .data[["name"]]        else NULL,
        if ("profileName" %in% names(.)) .data[["profileName"]] else NULL
      ))),
      player_id   = standardize_name(player_name),
      source      = "ForceDecks",
      test_type   = coalesce(
        if ("testTypeName" %in% names(.)) .data[["testTypeName"]] else NULL,
        "ForceDecks"
      )
    ) %>%
    select(any_of(c(id_cols, metric_cols))) %>%
    tidyr::pivot_longer(
      cols      = any_of(metric_cols),
      names_to  = "metric_raw",
      values_to = "metric_value_raw"
    ) %>%
    filter(!is.na(metric_value_raw)) %>%
    mutate(
      metric_name  = metric_raw,
      metric_value = suppressWarnings(as.numeric(metric_value_raw)),
      units        = ""
    ) %>%
    filter(!is.na(metric_value)) %>%
    select(player_id, player_name, date, datetime,
           source, test_type, metric_name, metric_value, units)
}

# ---------------------------
# Catapult ingestion
# ---------------------------

ingest_catapult <- function(path) {
  if (!file.exists(path)) {
    message("Warning: Catapult file not found at ", path)
    return(tibble::tibble())
  }
  raw <- readRDS(path)

  id_cols <- c("player_id", "player_name", "date", "datetime", "source", "test_type")
  metric_cols <- intersect(
    c("playerLoad","totalDistance","highSpeedDistance","sprintDistance",
      "explosiveEfforts","playerLoadPerMinute","playerLoadSlow",
      "maxVelocity","maxAcceleration","maxDeceleration",
      "standingWeight"),
    names(raw)
  )
  if (length(metric_cols) == 0) return(tibble::tibble())

  nm_col <- intersect(c("displayName","fullName","name","player_name","athleteName","profileName"), names(raw))[1]
  date_col <- intersect(c("date","sessionDate","testDate","testDateUtc"), names(raw))[1]

  if (is.na(nm_col) || is.na(date_col)) {
    message("Warning: Catapult file missing name or date column")
    return(tibble::tibble())
  }

  raw %>%
    mutate(
      date = {
        col <- .data[[date_col]]
        parsed <- suppressWarnings(lubridate::ymd_hms(col))
        if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::ymd(col))
        if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::mdy(col))
        as.Date(parsed)
      },
      datetime    = as.POSIXct(date),
      player_name = fix_player_name(str_squish(.data[[nm_col]])),
      player_id   = standardize_name(player_name),
      source      = "Catapult",
      test_type   = "Catapult"
    ) %>%
    select(any_of(c(id_cols, metric_cols))) %>%
    tidyr::pivot_longer(
      cols      = any_of(metric_cols),
      names_to  = "metric_raw",
      values_to = "metric_value_raw"
    ) %>%
    filter(!is.na(metric_value_raw)) %>%
    mutate(
      metric_name  = metric_raw,
      metric_value = suppressWarnings(as.numeric(metric_value_raw)),
      units        = ""
    ) %>%
    filter(!is.na(metric_value)) %>%
    select(player_id, player_name, date, datetime,
           source, test_type, metric_name, metric_value, units)
}

# ---------------------------
# SmartSpeed ingestion
# ---------------------------

ingest_smartspeed <- function(path) {
  if (!file.exists(path)) {
    message("Warning: SmartSpeed file not found at ", path)
    return(tibble::tibble())
  }
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
        parsed <- suppressWarnings(lubridate::ymd_hms(testDateUtc))
        if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::ymd(testDateUtc))
        as.Date(parsed)
      } else {
        as.Date(NA)
      },
      datetime     = as.POSIXct(date),
      player_name  = fix_player_name(str_squish(name)),
      player_id    = standardize_name(player_name),
      source       = "SmartSpeed",
      test_type    = ifelse(coalesce(testName, testTypeName, "SmartSpeed") == "Flying 10s",
                           "Fly 10-15",
                           coalesce(testName, testTypeName, "SmartSpeed")),
      metric_name  = "Best Split Seconds",
      metric_value = suppressWarnings(as.numeric(bestSplitSeconds)),
      units        = "s"
    ) %>%
    filter(!is.na(metric_value)) %>%
    select(player_id, player_name, date, datetime,
           source, test_type, metric_name, metric_value, units)
}

# ---------------------------
# ForceFrame ingestion
# ---------------------------
ingest_forceframe <- function(path) {
  if (!file.exists(path)) {
    message("Warning: ForceFrame file not found at ", path)
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
      source      = "ForceFrame",
      test_type   = coalesce(testTypeName, "ForceFrame")
    )
  
  id_cols <- c("player_id", "player_name", "date", "datetime", "source", "test_type")
  metric_cols <- intersect(
    c("maxInnerForce", "maxOuterForce", "AB_AD_ratio",
      "outerLeftMaxForce", "outerRightMaxForce",
      "innerLeftMaxForce", "innerRightMaxForce",
      "abduction_asymmetry", "adduction_asymmetry"),
    names(raw)
  )
  if (length(metric_cols) == 0) {
    message("Warning: No ForceFrame metric columns found")
    return(tibble::tibble())
  }
  
  raw %>%
    select(any_of(c(id_cols, metric_cols))) %>%
    pivot_longer(
      cols             = all_of(metric_cols),
      names_to         = "metric_raw",
      values_to        = "metric_value_raw",
      values_transform = list(metric_value_raw = as.character)
    ) %>%
    filter(!is.na(metric_value_raw)) %>%
    mutate(
      metric_name  = metric_raw,
      metric_value = suppressWarnings(as.numeric(metric_value_raw)),
      units        = ""
    ) %>%
    filter(!is.na(metric_value)) %>%
    select(player_id, player_name, date, datetime,
           source, test_type, metric_name, metric_value, units)
}

# ---------------------------
# Manual overrides ingestion
# ---------------------------

ingest_overrides <- function(path) {
  if (!file.exists(path)) return(tibble::tibble())
  raw <- readRDS(path)
  if (nrow(raw) == 0) return(tibble::tibble())
  
  required <- c("player_name", "date", "source", "test_type", "metric_name", "metric_value")
  if (!all(required %in% names(raw))) {
    message("Warning: manual_overrides.rds missing required columns")
    return(tibble::tibble())
  }
  
  raw %>%
    mutate(
      player_name = fix_player_name(str_squish(player_name)),
      player_id   = standardize_name(player_name),
      date        = as.Date(date),
      datetime    = as.POSIXct(date),
      metric_value = suppressWarnings(as.numeric(metric_value)),
      units       = if ("units" %in% names(.)) units else ""
    ) %>%
    filter(!is.na(metric_value)) %>%
    select(player_id, player_name, date, datetime,
           source, test_type, metric_name, metric_value, units)
}

# ---------------------------
# Load raw data
# ---------------------------

nord      <- ingest_nordbord(NORD_PATH)
force     <- ingest_forcedecks(FORCE_PATH)
catapult  <- ingest_catapult(CAT_PATH)
smartspeed <- ingest_smartspeed(SMART_PATH)
overrides  <- ingest_overrides(OVERRIDES_PATH)
forceframe <- ingest_forceframe(FORCEFRAME_PATH)

vald_tests_long <- bind_rows(nord, force, catapult, smartspeed, overrides, forceframe) %>%
  filter(!is.na(date)) %>%
  arrange(player_id, date)

message("  vald_tests_long: ", nrow(vald_tests_long), " rows")

# ---------------------------
# Build metric_key
# ---------------------------

metric_name_map <- c(
  jumpHeight           = "Jump Height",
  peakPower            = "Peak Power",
  peakForce            = "Peak Force",
  rsi                  = "RSI",
  reactivestrenindex   = "RSI",
  eccentricBrakeImpulse = "Eccentric Brake Impulse",
  concentricImpulse    = "Concentric Impulse",
  peakLandingForce     = "Peak Landing Force",
  leftMeanForce        = "Left Mean Force",
  rightMeanForce       = "Right Mean Force",
  limbImbalance        = "Limb Imbalance",
  contactTime          = "Contact Time",
  flightTime           = "Flight Time",
  takeoffVelocity      = "Takeoff Velocity",
  landingVelocity      = "Landing Velocity",
  leftPeakForce        = "Left Peak Force",
  rightPeakForce       = "Right Peak Force",
  peakPowerPerBodyMass = "Peak Power Per Body Mass",
  peakForcePerBodyMass = "Peak Force Per Body Mass",
  playerLoad           = "Total Player Load",
  totalDistance        = "Total Distance",
  highSpeedDistance    = "High Speed Distance (12 mph)",
  sprintDistance       = "Sprint Distance (16 mph)",
  explosiveEfforts     = "Explosive Efforts",
  playerLoadPerMinute  = "Player Load Per Minute",
  playerLoadSlow       = "Player Load Slow",
  maxVelocity          = "Max Velocity",
  maxAcceleration      = "Max Effort Acceleration",
  maxDeceleration      = "Max Effort Deceleration",
  standingWeight       = "Athlete Standing Weight",
  leftMaxForce         = "Left Max Force",
  rightMaxForce        = "Right Max Force",
  imbalance            = "Imbalance",
  isoAsymmetry         = "ISO Asymmetry",
  nordicAsymmetry      = "Nordic Asymmetry",
  maxInnerForce        = "Max Inner Force",
  maxOuterForce        = "Max Outer Force",
  AB_AD_ratio          = "AB/AD Ratio",
  outerLeftMaxForce    = "Outer Left Max Force",
  outerRightMaxForce   = "Outer Right Max Force",
  innerLeftMaxForce    = "Inner Left Max Force",
  innerRightMaxForce   = "Inner Right Max Force",
  abduction_asymmetry  = "Abduction Asymmetry",
  adduction_asymmetry  = "Adduction Asymmetry"
)

vald_tests_long_ui <- vald_tests_long %>%
  mutate(
    display_name = ifelse(
      metric_name %in% metric_name_map,
      metric_name_map[metric_name],
      metric_name
    ),
    metric_key = paste(source, test_type, display_name, sep = "|")
  )

# ---------------------------
# as_of_date
# ---------------------------

as_of_date <- Sys.Date()

# ---------------------------
# players table
# ---------------------------

players <- vald_tests_long_ui %>%
  distinct(player_id, player_name) %>%
  left_join(
    player_metadata %>% select(player_id,
      any_of(c("pos_group","pos_position","class_year","class_year_base","is_redshirt",
               "height_display","weight_display","wingspan_display","hand_display","arm_display"))),
    by = "player_id"
  )

message("  players: ", nrow(players))

# ---------------------------
# vald_latest_wide
# ---------------------------

vald_latest_wide <- vald_tests_long_ui %>%
  filter(date <= as_of_date) %>%
  group_by(player_id, player_name, metric_key) %>%
  slice_max(date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, player_name, metric_key, latest_value = metric_value) %>%
  tidyr::pivot_wider(names_from = metric_key, values_from = latest_value)

# ---------------------------
# Lifts (manual overrides)
# ---------------------------

if (nrow(overrides) > 0) {
  lifts_long <- vald_tests_long_ui %>%
    filter(source == "Lifts") %>%
    filter(date <= as_of_date)
  
  if (nrow(lifts_long) > 0) {
    lifts_best <- lifts_long %>%
      group_by(player_id, player_name, metric_key) %>%
      slice_max(metric_value, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    lifts_latest <- lifts_long %>%
      group_by(player_id, player_name, metric_key) %>%
      slice_max(date, n = 1, with_ties = FALSE) %>%
      ungroup()
  } else {
    lifts_best   <- tibble::tibble()
    lifts_latest <- tibble::tibble()
  }
} else {
  lifts_best   <- tibble::tibble()
  lifts_latest <- tibble::tibble()
}

# ---------------------------
# Roster metric selection logic
# ---------------------------

if (file.exists(OVERRIDES_PATH)) {
  overrides_data <- readRDS(OVERRIDES_PATH)
  if (nrow(overrides_data) > 0 && "source" %in% names(overrides_data)) {
    lift_keys <- vald_tests_long_ui %>%
      filter(source == "Lifts") %>%
      distinct(metric_key) %>%
      pull(metric_key)
    force_include_keys <- c(
      force_include_keys,
      lift_keys
    )
  }
}

# ============================================================
# STEP 1: Build FULL roster_view
# ============================================================

pos_cols <- c(
  "pos_group", "pos_position",
  "class_year", "class_year_base", "is_redshirt",
  "height_display", "weight_display",
  "wingspan_display", "hand_display", "arm_display"
)
id_cols <- c("player_id", "player_name", pos_cols, "as_of_date")

roster_values_long <- vald_tests_long_ui %>%
  filter(date <= as_of_date) %>%
  group_by(player_id, player_name, metric_key) %>%
  summarise(
    latest_value = {
      idx <- which.max(date)
      metric_value[idx]
    },
    best_value = {
      lb <- is_lower_better(metric_key[1])
      mr <- is_most_recent(metric_key[1])
      if (mr) {
        idx <- which.max(date)
        metric_value[idx]
      } else if (lb) {
        min(metric_value, na.rm = TRUE)
      } else {
        max(metric_value, na.rm = TRUE)
      }
    },
    .groups = "drop"
  ) %>%
  mutate(
    metric_value_roster = ifelse(
      is_most_recent(metric_key),
      latest_value,
      best_value
    )
  ) %>%
  select(player_id, player_name, metric_key, metric_value_roster)

roster_view <- roster_values_long %>%
  tidyr::pivot_wider(names_from = metric_key, values_from = metric_value_roster) %>%
  left_join(
    player_metadata %>% select(player_id, all_of(pos_cols)),
    by = "player_id"
  ) %>%
  mutate(as_of_date = as_of_date)

metric_cols <- setdiff(names(roster_view), id_cols)

# ============================================================
# STEP 2: Compute fill_summary and keep_roster_metrics
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
    pivot_wider(names_from = stat, values_from = value)
}) %>%
  bind_rows()

keep_roster_metrics <- fill_summary %>%
  filter(
    fill_frac >= 0.25,
    n_unique  >  1,
    is.na(zero_frac) | zero_frac <= 0.90
  ) %>%
  pull(metric_key)

force_include_keys <- c(
  "Catapult|Catapult|Explosive Efforts",
  "Catapult|Catapult|High Speed Distance (12 mph)",
  "Catapult|Catapult|Sprint Distance (16 mph)",
  "SmartSpeed|Fly 10-15|Best Split Seconds",
  paste0("Lifts|Strength|", OVERRIDE_TESTS)
)

keep_roster_metrics <- unique(c(keep_roster_metrics, force_include_keys))

# ============================================================
# STEP 3: Build vald_best_wide
# ============================================================

vald_best_wide <- vald_tests_long_ui %>%
  filter(date <= as_of_date) %>%
  group_by(player_id, player_name, metric_key) %>%
  summarise(
    best_value = {
      lb <- is_lower_better(metric_key[1])
      if (lb) min(metric_value, na.rm = TRUE) else max(metric_value, na.rm = TRUE)
    },
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = metric_key, values_from = best_value)

# ============================================================
# STEP 4: Filter roster_view to kept metrics + round
# ============================================================

roster_view <- roster_view %>%
  select(any_of(c(id_cols, keep_roster_metrics))) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

roster_best_view <- vald_best_wide %>%
  select(any_of(c("player_id", "player_name", keep_roster_metrics))) %>%
  left_join(
    player_metadata %>% select(player_id, all_of(pos_cols)),
    by = "player_id"
  ) %>%
  mutate(
    as_of_date = as_of_date,
    across(where(is.numeric), ~ round(., 2))
  )

message("  roster_view: ", nrow(roster_view), " rows, ", ncol(roster_view) - length(id_cols), " roster metrics")

# ============================================================
# STEP 5: Position-based percentiles
# ============================================================

roster_percentiles_long <- roster_view %>%
  select(player_id, player_name, pos_position, all_of(keep_roster_metrics)) %>%
  tidyr::pivot_longer(
    cols      = all_of(keep_roster_metrics),
    names_to  = "metric_key",
    values_to = "metric_value"
  ) %>%
  group_by(pos_position, metric_key) %>%
  mutate(
    percentile = compute_percentile(metric_value, lower_is_better = is_lower_better(metric_key[1]))
  ) %>%
  ungroup()

roster_percentiles_wide <- roster_view %>%
  left_join(
    roster_percentiles_long %>%
      select(player_id, metric_key, percentile) %>%
      tidyr::pivot_wider(
        names_from  = metric_key,
        values_from = percentile,
        names_glue  = "{metric_key}__pctl"
      ),
    by = "player_id"
  )

# ============================================================
# Best-value percentiles (for athleticism score)
# ============================================================

roster_best_percentiles_pos_long <- roster_best_view %>%
  select(player_id, player_name, pos_position, all_of(intersect(keep_roster_metrics, names(roster_best_view)))) %>%
  tidyr::pivot_longer(
    cols      = all_of(intersect(keep_roster_metrics, names(roster_best_view))),
    names_to  = "metric_key",
    values_to = "metric_value"
  ) %>%
  group_by(pos_position, metric_key) %>%
  mutate(
    percentile = compute_percentile(metric_value, lower_is_better = is_lower_better(metric_key[1]))
  ) %>%
  ungroup()

# ============================================================
# Athleticism Score
# ============================================================

find_first_existing <- function(source_prefix, metric_names, test_type = NULL) {
  for (mn in metric_names) {
    if (!is.null(test_type)) {
      k <- paste(source_prefix, test_type, mn, sep = "|")
    } else {
      k <- paste(source_prefix, mn, sep = "|")
    }
    if (k %in% names(roster_best_view)) return(k)
  }
  NA_character_
}

k_jump <- find_first_existing("ForceDecks", c("jumpHeight","Jump Height","peakPower","Peak Power"))
k_rsi  <- find_first_existing("ForceDecks", c("rsi","RSI","reactivestrenindex"))
k_ebi  <- find_first_existing("ForceDecks", c("eccentricBrakeImpulse","Eccentric Brake Impulse"))
k_pp   <- find_first_existing("ForceDecks", c("peakPower","Peak Power","peakPowerPerBodyMass","Peak Power Per Body Mass"))
k_lmf  <- find_first_existing("NordBord",   c("leftMaxForce","Left Max Force"))
k_rmf  <- find_first_existing("NordBord",   c("rightMaxForce","Right Max Force"))
k_imb  <- find_first_existing("NordBord",   c("imbalance","Imbalance"))
k_maxv <- find_first_existing("Catapult",   c("maxVelocity","Max Velocity"))
k_acc  <- find_first_existing("Catapult",   c("Max Effort Acceleration", "Max Effort Accel"))
k_dec  <- find_first_existing("Catapult",   c("Max Effort Deceleration", "Max Effort Decel"))
k_fly10 <- find_first_existing("SmartSpeed", metric_names = c("Best Split Seconds"), test_type = "Flying 10s")
k_fly10_15 <- find_first_existing("SmartSpeed", metric_names = c("Best Split Seconds"), test_type = "Fly 10-15")

keys_needed <- c(k_jump, k_rsi, k_ebi, k_pp, k_lmf, k_rmf, k_imb, k_maxv, k_acc, k_dec, k_fly10, k_fly10_15)
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
    jump  = safe_col(., k_jump),
    rsi   = safe_col(., k_rsi),
    ebi   = safe_col(., k_ebi),
    pp    = safe_col(., k_pp),
    maxv  = safe_col(., k_maxv),
    acc   = safe_col(., k_acc),
    dec   = safe_col(., k_dec),
    fly10 = {
      f10 <- safe_col(., k_fly10)
      f15 <- safe_col(., k_fly10_15)
      ifelse(!is.na(f10), f10, f15)
    },
    lmf   = safe_col(., k_lmf),
    rmf   = safe_col(., k_rmf),
    mf    = ifelse(
      !is.na(lmf) | !is.na(rmf),
      rowMeans(cbind(lmf, rmf), na.rm = TRUE),
      NA_real_
    ),
    imb   = safe_col(., k_imb),
    AthleticismScore = {
      vs <- cbind(jump, rsi, ebi, pp, mf, imb, maxv, fly10, acc, dec)
      colnames(vs) <- c("jump", "rsi", "ebi", "pp", "mf", "imb", "maxv", "fly10", "acc", "dec")
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

athletics_key <- "Composite|Score|Athleticism Score"
athleticism_key <- "Composite|Score|Athleticism Score"

roster_view <- roster_view %>%
  left_join(ath %>% select(player_id, AthleticismScore), by = "player_id") %>%
  mutate(!!athleticism_key := AthleticismScore) %>%
  select(-AthleticismScore)

roster_best_view <- roster_best_view %>%
  left_join(ath %>% select(player_id, AthleticismScore), by = "player_id") %>%
  mutate(!!athleticism_key := AthleticismScore) %>%
  select(-AthleticismScore)

keep_roster_metrics <- unique(c(keep_roster_metrics, athleticism_key))

roster_percentiles_long <- bind_rows(
  roster_percentiles_long,
  ath %>%
    filter(!is.na(AthleticismScore)) %>%
    transmute(
      player_id,
      player_name,
      pos_position = NA_character_,
      metric_key = athleticism_key,
      metric_value = AthleticismScore,
      percentile = AthleticismScore
    )
)

# ============================================================
# Roster percentiles wide (final, with athleticism)
# ============================================================

roster_percentiles_wide <- roster_view %>%
  left_join(
    roster_percentiles_long %>%
      select(player_id, metric_key, percentile) %>%
      tidyr::pivot_wider(
        names_from  = metric_key,
        values_from = percentile,
        names_glue  = "{metric_key}__pctl"
      ),
    by = "player_id"
  )

message("  roster_view: ", nrow(roster_view), " rows (final w/ athleticism)")

# ============================================================
# ACWR
# ============================================================

acute_days  <- 7L
chronic_days <- 28L

acwr_per_player <- catapult %>%
  filter(!is.na(date)) %>%
  group_by(player_id, player_name) %>%
  summarise(
    acute  = {
      cutoff <- as_of_date - acute_days
      mean(metric_value[date > cutoff & metric_name == "playerLoad"], na.rm = TRUE)
    },
    chronic = {
      cutoff <- as_of_date - chronic_days
      mean(metric_value[date > cutoff & metric_name == "playerLoad"], na.rm = TRUE)
    },
    .groups = "drop"
  ) %>%
  mutate(
    acwr = ifelse(chronic > 0, round(acute / chronic, 2), NA_real_)
  ) %>%
  filter(!is.na(acwr))

message("  ACWR for ", nrow(acwr_per_player), " players")
message("metrics_automated.r done")
