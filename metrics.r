# metrics.R
# ---------
# In-memory build for VALD (NordBord + ForceDecks) used by Shiny via source("metrics.R")
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
#
# No scoring/composites.

suppressPackageStartupMessages({
  library(readr)
  library(readxl)   # NEW
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
})

# ---------------------------
# Config
# ---------------------------
NORD_PATH  <- "nordbord_data2.csv"
FORCE_PATH <- "forcedecks_data1.csv"
POS_PATH <- "positions.csv"
CAT_PATH <- "catapult.csv"
MEAS_PATH <- "MEASUREMENTS.xlsx"



as_of_date <- as.Date("2026-02-04")

filled_threshold <- 0.25

# ---------------------------
# Helpers
# ---------------------------

standardize_name <- function(x) {
  x %>%
    str_squish() %>%
    str_to_upper() %>%
    str_replace_all("[\\.,'’]", "") %>%
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

is_flip_metric <- function(metric_key) {
  # flip metrics where LOWER is better
  grepl("Imbalance", metric_key, ignore.case = TRUE)
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b


ingest_positions <- function(path) {
  pos_raw <- read_csv(path, show_col_types = FALSE) %>%
    mutate(
      player_name = str_squish(paste(firstName, lastName)),
      player_id   = standardize_name(player_name),
      Group       = na_if(str_squish(as.character(Group)), ""),
      Position    = na_if(str_squish(as.character(Position)), ""),
      year        = suppressWarnings(as.integer(year))
    )
  
  # If duplicates exist for the same player_id, keep the “best” row
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


ingest_measurements <- function(path) {
  meas_raw <- readxl::read_excel(path) %>%
    mutate(
      player_name = str_squish(paste(firstName, lastName)),
      player_id   = standardize_name(player_name),
      
      Group    = na_if(str_squish(as.character(Group)), ""),
      Position = na_if(str_squish(as.character(Position)), ""),
      
      # keep original strings exactly (trim only; no numeric parsing)
      Year     = na_if(str_squish(as.character(Year)), ""),
      Height   = na_if(str_squish(as.character(Height)), ""),
      Weight   = na_if(str_squish(as.character(Weight)), ""),
      Wingspan = na_if(str_squish(as.character(Wingspan)), ""),
      Hand     = na_if(str_squish(as.character(Hand)), ""),
      Arm      = na_if(str_squish(as.character(Arm)), "")
    ) %>%
    mutate(
      # filter-friendly versions (still strings)
      class_year = Year %>% str_replace_all("\\*", "") %>% str_squish(),
      
      is_redshirt = if_else(!is.na(class_year) & str_detect(class_year, "^RS-"), "Yes", "No"),
      
      class_year_base = class_year %>%
        str_replace("^RS-", "") %>%
        str_to_upper() %>%
        na_if("")
    )
  
  # de-dupe per player_id (prefer rows with non-missing Position/Group/Year)
  meas_clean <- meas_raw %>%
    group_by(player_id) %>%
    summarise(
      meas_player_name = first(na.omit(player_name)),
      
      pos_group    = dplyr::coalesce(first(na.omit(Group)), NA_character_),
      pos_position = dplyr::coalesce(first(na.omit(Position)), NA_character_),
      
      class_year      = dplyr::coalesce(first(na.omit(class_year)), NA_character_),
      class_year_base = dplyr::coalesce(first(na.omit(class_year_base)), NA_character_),
      is_redshirt     = dplyr::coalesce(first(na.omit(is_redshirt)), NA_character_),
      
      height_display   = dplyr::coalesce(first(na.omit(Height)), NA_character_),
      weight_display   = dplyr::coalesce(first(na.omit(Weight)), NA_character_),
      wingspan_display = dplyr::coalesce(first(na.omit(Wingspan)), NA_character_),
      hand_display     = dplyr::coalesce(first(na.omit(Hand)), NA_character_),
      arm_display      = dplyr::coalesce(first(na.omit(Arm)), NA_character_),
      
      .groups = "drop"
    )
  
  meas_clean
}


# ---------------------------
# Ingestors
# ---------------------------

ingest_nordboard <- function(path) {
  raw <- read_csv(path, show_col_types = FALSE)
  
  raw <- raw %>%
    rename(date_chr = `Date UTC`, time_chr = `Time UTC`, test_type = Test) %>%
    mutate(
      date     = mdy(date_chr),
      datetime = suppressWarnings(mdy_hms(paste(date_chr, time_chr))),
      datetime = if_else(is.na(datetime), as.POSIXct(date), datetime),
      player_name = str_squish(Name),
      player_id   = standardize_name(player_name),
      source      = "NordBord"
    )
  
  id_cols <- c("player_id", "player_name", "date", "datetime", "source", "test_type")
  
  non_metric <- c("Name", "ExternalId", "Device", "date_chr", "time_chr",
                  "date", "datetime", "source", "test_type")
  metric_cols <- setdiff(names(raw), c(non_metric, id_cols))
  
  raw %>%
    select(any_of(id_cols), any_of(metric_cols)) %>%
    pivot_longer(
      cols = all_of(metric_cols),
      names_to = "metric_raw",
      values_to = "metric_value_raw",
      values_transform = list(metric_value_raw = as.character)
    ) %>%
    mutate(
      units        = parse_units(metric_raw),
      metric_name  = clean_metric_name(metric_raw),
      metric_value = parse_metric_numeric(metric_value_raw)
    ) %>%
    filter(!is.na(metric_value)) %>%
    select(-metric_value_raw)
}

ingest_forcedecks <- function(path) {
  raw <- read_csv(path, show_col_types = FALSE)
  names(raw) <- str_squish(names(raw))
  
  # Column name after str_squish()
  
  jh_col <- "Jump Height (Imp-Mom) in Inches [in]"
  
  if (jh_col %in% names(raw)) {
    raw <- raw %>%
      mutate(jh_in = parse_metric_numeric(.data[[jh_col]])) %>%
      filter(jh_in <= 5 | jh_in >= 30) %>%
      select(-jh_in)
  }
  
  raw <- raw %>%
    rename(test_type = `Test Type`) %>%
    mutate(
      date     = mdy(as.character(Date)),
      datetime = as.POSIXct(date),   # ← SAFE fallback
      player_name = str_squish(Name),
      player_id   = standardize_name(player_name),
      source      = "ForceDecks"
    )
  
  id_cols <- c("player_id", "player_name", "date", "datetime", "source", "test_type")
  
  non_metric <- c("Name", "ExternalId", "Date", "Time", "Tags",
                  "date", "datetime", "source", "test_type")
  metric_cols <- setdiff(names(raw), c(non_metric, id_cols))
  
  raw %>%
    select(any_of(id_cols), any_of(metric_cols)) %>%
    pivot_longer(
      cols = all_of(metric_cols),
      names_to = "metric_raw",
      values_to = "metric_value_raw",
      values_transform = list(metric_value_raw = as.character)
    ) %>%
    mutate(
      units        = parse_units(metric_raw),
      metric_name  = clean_metric_name(metric_raw),
      metric_value = parse_metric_numeric(metric_value_raw)
    ) %>%
    filter(!is.na(metric_value)) %>%
    select(-metric_value_raw)
}


ingest_catapult <- function(path) {
  raw <- read_csv(path, show_col_types = FALSE)
  names(raw) <- str_squish(names(raw))
  
  raw <- raw %>%
    mutate(
      player_name = str_squish(athlete_name),
      player_id   = standardize_name(player_name),
      date        = mdy(as.character(date)),    # catapult.csv is m/d/y in your file
      datetime    = as.POSIXct(date),
      source      = "Catapult",
      test_type   = "Catapult"
    )
  
  id_cols <- c("player_id", "player_name", "date", "datetime", "source", "test_type")
  
  non_metric <- c(
    "athlete_name", "activity_name", "period_id", "period_name",
    "player_name", "player_id", "date", "datetime", "source", "test_type"
  )
  
  metric_cols <- setdiff(names(raw), non_metric)
  
  raw %>%
    select(any_of(id_cols), any_of(metric_cols)) %>%
    pivot_longer(
      cols = all_of(metric_cols),
      names_to = "metric_raw",
      values_to = "metric_value_raw",
      values_transform = list(metric_value_raw = as.character)
    ) %>%
    mutate(
      units        = NA_character_,
      # nicer display names in Shiny
      metric_name  = metric_raw %>%
        str_replace_all("_", " ") %>%
        str_squish() %>%
        str_to_title(),
      metric_value = parse_metric_numeric(metric_value_raw)
    ) %>%
    filter(!is.na(metric_value)) %>%
    select(-metric_value_raw)
}


ingest_vald_bundle <- function(nord_path, force_path, cat_path) {
  bind_rows(
    ingest_nordboard(nord_path),
    ingest_forcedecks(force_path),
    ingest_catapult(cat_path)
  ) %>%
    mutate(metric_value = as.numeric(metric_value))
}


# ---------------------------
# Build canonical long tables
# ---------------------------

vald_tests_long <- ingest_vald_bundle(NORD_PATH, FORCE_PATH, CAT_PATH)

vald_tests_long <- vald_tests_long %>%
  mutate(
    metric_value = round(metric_value, 2)
  )

players <- vald_tests_long %>%
  distinct(player_id, player_name) %>%
  arrange(player_name)

measurements <- ingest_measurements(MEAS_PATH)

players <- players %>%
  left_join(measurements, by = "player_id")

vald_tests_long <- vald_tests_long %>%
  left_join(measurements, by = "player_id")


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

pos_cols <- c(
  "pos_group", "pos_position",
  "class_year", "class_year_base", "is_redshirt",
  "height_display", "weight_display",
  "wingspan_display", "hand_display", "arm_display"
)

id_cols <- c("player_id", "player_name", pos_cols, "as_of_date")
metric_cols <- setdiff(names(vald_latest_wide), id_cols)


# ---------------------------
# Keep roster metrics (>=80% filled) — computed WITHIN SOURCE
# ---------------------------

# helper to extract source from metric_key
metric_source <- function(mk) strsplit(mk, "\\|")[[1]][1]

sources <- sort(unique(vald_tests_long_ui$source))

fill_summary <- lapply(sources, function(s) {
  
  players_s <- vald_tests_long_ui %>%
    filter(source == s, date <= as_of_date) %>%
    distinct(player_id)
  
  metric_cols_s <- metric_cols[startsWith(metric_cols, paste0(s, "|"))]
  if (length(metric_cols_s) == 0) return(dplyr::tibble())
  
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
          n_unique = ~ {                  # ✅ NEW
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
    n_unique > 1,                    # ✅ drops constant metrics within-source
    is.na(zero_frac) | zero_frac <= 0.90
  ) %>%
  pull(metric_key)


roster_view <- vald_latest_wide %>%
  select(all_of(c(id_cols, keep_roster_metrics)))

# ---- round roster_view numeric columns for display ----
roster_view <- roster_view %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 2)
    )
  )


# Position based percentiles
pos_pct_col <- if ("pos_position" %in% names(roster_view)) "pos_position" else NULL

roster_percentiles_pos_long <- NULL
roster_percentiles_pos_wide <- NULL

if (!is.null(pos_pct_col)) {
  roster_percentiles_pos_long <- roster_view %>%
    select(player_id, player_name, !!pos_pct_col, all_of(keep_roster_metrics)) %>%
    pivot_longer(
      cols = all_of(keep_roster_metrics),
      names_to = "metric_key",
      values_to = "metric_value"
    ) %>%
    group_by(metric_key, .data[[pos_pct_col]]) %>%
    mutate(
      pos_percentile = if_else(
        is_flip_metric(metric_key),
        pct_rank_100(-metric_value),   # flip direction: lower raw -> higher percentile
        pct_rank_100(metric_value)
      )
    ) %>%
    ungroup()
  
  roster_percentiles_pos_wide <- roster_percentiles_pos_long %>%
    select(player_id, metric_key, pos_percentile) %>%
    pivot_wider(
      names_from = metric_key,
      values_from = pos_percentile,
      names_glue = "{metric_key}__pos_pctl"
    )
}

roster_percentiles_wide <- roster_view %>% select(player_id) %>%
  left_join(roster_percentiles_pos_wide, by = "player_id")

roster_percentiles_long <- roster_percentiles_pos_long %>%
  transmute(player_id, player_name, metric_key, percentile = pos_percentile)


# ---------------------------
# Athleticism Score (weighted position percentiles)
# ---------------------------

athleticism_key <- "Composite|Score|Athleticism Score"

find_key_by_source_and_name <- function(source_name, metric_name) {
  cand <- keep_roster_metrics[startsWith(keep_roster_metrics, paste0(source_name, "|"))]
  hit  <- cand[stringr::str_ends(cand, paste0("|", metric_name))]
  if (length(hit) >= 1) hit[1] else NA_character_
}

k_jump <- find_key_by_source_and_name("ForceDecks", "Jump Height (Imp-Mom) in Inches")
k_ebi  <- find_key_by_source_and_name("ForceDecks", "Eccentric Braking Impulse")
k_pp   <- find_key_by_source_and_name("ForceDecks", "Force at Peak Power")
k_zv   <- find_key_by_source_and_name("ForceDecks", "Force at Zero Velocity")
k_lmf  <- find_key_by_source_and_name("NordBord",   "L Max Force")
k_rmf  <- find_key_by_source_and_name("NordBord",   "R Max Force")
k_imb  <- find_key_by_source_and_name("NordBord",   "Max Imbalance")  # already flipped earlier

keys_needed <- c(k_jump, k_ebi, k_pp, k_zv, k_lmf, k_rmf, k_imb)
keys_needed <- keys_needed[!is.na(keys_needed)]

pcts_wide <- roster_percentiles_long %>%
  filter(metric_key %in% keys_needed) %>%
  select(player_id, player_name, metric_key, percentile) %>%
  tidyr::pivot_wider(names_from = metric_key, values_from = percentile)

# weights (sum to 1.00)
w <- c(
  jump = 0.25,
  pp   = 0.20,
  zv   = 0.15,
  ebi  = 0.15,
  mf   = 0.20,
  imb  = 0.05
)

safe_col <- function(df, key) {
  if (is.na(key)) return(rep(NA_real_, nrow(df)))
  if (!key %in% names(df)) return(rep(NA_real_, nrow(df)))
  df[[key]]
}

ath <- pcts_wide %>%
  mutate(
    jump = safe_col(., k_jump),
    pp   = safe_col(., k_pp),
    zv   = safe_col(., k_zv),
    ebi  = safe_col(., k_ebi),
    imb  = safe_col(., k_imb),
    
    lmf = safe_col(., k_lmf),
    rmf = safe_col(., k_rmf),
    mf  = ifelse(!is.na(lmf) | !is.na(rmf), rowMeans(cbind(lmf, rmf), na.rm = TRUE), NA_real_),
    
    AthleticismScore = {
      vs <- cbind(jump, pp, zv, ebi, mf, imb)
      colnames(vs) <- names(w)
      apply(vs, 1, function(v) {
        ok <- !is.na(v)
        if (!any(ok)) return(NA_real_)
        sum(w[ok] * v[ok]) / sum(w[ok])
      })
    }
  ) %>%
  transmute(
    player_id,
    player_name,
    AthleticismScore = round(AthleticismScore, 1)
  )

# add to roster_view
roster_view <- roster_view %>%
  left_join(ath, by = c("player_id", "player_name")) %>%
  mutate(!!athleticism_key := AthleticismScore) %>%
  select(-AthleticismScore)

# allow UI to treat like metric column
keep_roster_metrics <- unique(c(keep_roster_metrics, athleticism_key))

# add to percentiles long (already 0-100 scale)
roster_percentiles_long <- roster_percentiles_long %>%
  bind_rows(
    ath %>% transmute(player_id, player_name, metric_key = athleticism_key, percentile = AthleticismScore)
  )


message("Built objects:")
message("  vald_tests_long:         ", nrow(vald_tests_long), " rows")
message("  players:                ", nrow(players), " players")
message("  vald_latest_wide:       ", nrow(vald_latest_wide), " rows, ", length(metric_cols), " metrics")
message("  roster metrics kept:    ", length(keep_roster_metrics), " (>= ", filled_threshold*100, "% filled)")
message("  roster_view:            ", nrow(roster_view), " rows")
message("  roster_percentiles_*:   computed for kept roster metrics")