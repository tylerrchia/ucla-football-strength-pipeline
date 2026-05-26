# app.R
# -----
# Shiny UI for VALD roster explorer + player card + correlations
# Uses objects produced by source("metrics.R"):
#   roster_view, roster_percentiles_wide, roster_percentiles_long,
#   vald_tests_long_ui, keep_roster_metrics, fill_summary, as_of_date
#
# NOTE: Your test data probably doesn't include Position/Class/Headshot.
# This app will gracefully fall back (filters hidden, radar = NordBord vs ForceDecks).

library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(plotly)

source("metrics.r")

# ---------------------------
# Helpers
# ---------------------------

# --- player name -> lastname_firstname.jpg (your convention) ---
player_headshot_slug <- function(player_name) {
  s <- tolower(stringr::str_trim(player_name))
  s <- stringr::str_replace_all(s, "['']", "")      # apostrophes
  s <- stringr::str_replace_all(s, "-", "")         # hyphens
  s <- stringr::str_replace_all(s, "[^a-z\\s]", "") # keep letters/spaces
  s <- stringr::str_squish(s)
  
  parts <- unlist(strsplit(s, "\\s+"))
  if (length(parts) == 0) return(NA_character_)
  if (length(parts) == 1) return(parts[1])
  
  last  <- parts[length(parts)]
  first <- paste0(parts[-length(parts)], collapse = "")  # First+Middle concatenated
  paste0(last, "_", first)
}


find_player_headshot <- function(player_name, exts=c("jpg","jpeg","png","webp")) {
  slug <- player_headshot_slug(player_name)
  if (is.na(slug) || !nzchar(slug)) return(NA_character_)
  
  for (ext in exts) {
    fn <- paste0(slug, ".", ext)
    if (file.exists(file.path("www", "HEADSHOTS", fn))) {
      return(paste0("HEADSHOTS/", fn))   # (no file.path)
      # return(file.path("HEADSHOTS", fn))  # served automatically from www/
    }
  }
  NA_character_
}



# --- name -> file slug used by images ---
player_photo_slug <- function(player_name) {
  # Expect player_name like "First Last" or "First Middle Last"
  # File rule: firstname chunk = all tokens except last, concatenated w/ no spaces;
  # hyphens removed; last name = last token.
  
  s <- tolower(stringr::str_trim(player_name))
  s <- stringr::str_replace_all(s, "['']", "")      # remove apostrophes
  s <- stringr::str_replace_all(s, "-", "")         # remove hyphens
  s <- stringr::str_replace_all(s, "[^a-z\\s]", "") # keep letters/spaces only
  s <- stringr::str_squish(s)
  
  parts <- unlist(strsplit(s, "\\s+"))
  if (length(parts) == 0) return(NA_character_)
  if (length(parts) == 1) return(parts[1])  # fallback: single token name
  
  last <- parts[length(parts)]
  first <- paste0(parts[-length(parts)], collapse = "")  # <- NO spaces
  
  paste0(first, "_", last)
}


# find best existing photo file for a given view
find_player_photo <- function(player_name, view = c("front","side","back"),
                              exts = c("jpg","jpeg","png","webp","heif","heic"),
                              base_url_prefix = "pics") {
  view <- match.arg(view)
  slug <- player_photo_slug(player_name)
  
  # try each extension in order
  for (ext in exts) {
    fn <- sprintf("%s_%s.%s", slug, view, ext)
    # physical path (for existence check)
    disk_path <- file.path("pics", fn)
    disk_path_www <- file.path("www", "pics", fn)
    
    if (file.exists(disk_path)) {
      return(file.path(base_url_prefix, fn)) # served via addResourcePath
    }
    if (file.exists(disk_path_www)) {
      return(file.path("pics", fn)) # served from www/
    }
  }
  NA_character_
}


render_plotly_radar <- function(df_long, selected_player, title = NULL, subtitle_empty = "No data available") {
  # df_long must have: player_name, axis, percentile
  
  # ---- axis levels (robust) ----
  axis_levels <- df_long$axis %>% unique()
  axis_levels <- axis_levels[!is.na(axis_levels) & nzchar(axis_levels)]
  
  # ---- radar-only label cleanup: clip trailing "in inches" ----
  df_long <- df_long %>%
    mutate(axis = stringr::str_replace(axis, "\\s*\\b(in\\s+Inches)\\b\\s*$", "")) %>%
    mutate(axis = stringr::str_squish(axis))
  
  axis_levels <- df_long$axis %>% unique()
  axis_levels <- axis_levels[!is.na(axis_levels) & nzchar(axis_levels)]
  
  
  # If truly nothing to plot and no axes exist, still force a polar plot
  if (length(axis_levels) == 0) {
    axis_levels <- c("—")
  }
  
  # Ensure every player has every axis (keeps polygons aligned)
  df_long <- df_long %>%
    tidyr::complete(
      player_name,
      axis = axis_levels,
      fill = list(percentile = NA_real_)
    )
  
  make_closed <- function(d) {
    d <- d %>% arrange(match(axis, axis_levels))
    bind_rows(d, d %>% dplyr::slice_head(n = 1))
  }
  
  p <- plotly::plot_ly()
  added_any <- FALSE
  
  for (pn in unique(df_long$player_name)) {
    d <- df_long %>% filter(player_name == pn) %>% make_closed()
    if (all(is.na(d$percentile))) next
    
    is_sel <- identical(pn, selected_player)
    
    p <- p %>%
      plotly::add_trace(
        data = d,
        type = "scatterpolar",
        mode = "lines+markers",
        r = ~percentile,
        theta = ~axis,
        name = pn,
        text = ~pn,
        hovertemplate = "%{theta}: %{r:.1f}<extra></extra>",
        connectgaps = FALSE,
        line = list(width = if (is_sel) 4 else 1.5, shape = "linear"),
        marker = list(size = if (is_sel) 6 else 4),
        fill = if (is_sel) "toself" else "none",
        fillcolor = if (is_sel) "rgba(39,116,174,0.15)" else NULL,
        opacity = if (is_sel) 1 else 0.15,
        showlegend = is_sel
      )
    
    added_any <- TRUE
  }
  
  # ---- if no data, force an empty polar frame (prevents XY fallback) ----
  if (!added_any) {
    p <- p %>%
      plotly::add_trace(
        type = "scatterpolar",
        r = rep(0, length(axis_levels)),
        theta = axis_levels,
        mode = "lines",
        line = list(color = "rgba(0,0,0,0)"),
        hoverinfo = "skip",
        showlegend = FALSE
      )
    
    if (!is.null(title) && nzchar(title)) {
      title <- paste0(title, "<br><sup>", subtitle_empty, "</sup>")
    } else {
      title <- paste0("<sup>", subtitle_empty, "</sup>")
    }
  }
  
  p %>%
    plotly::layout(
      title = list(text = title),
      polar = list(
        radialaxis  = list(range = c(0, 100), tickvals = c(0, 25, 50, 75, 100)),
        angularaxis = list(categoryorder = "array", categoryarray = axis_levels)
      ),
      margin = list(l = 40, r = 40, t = 60, b = 40)
    )
}



# --- drop unwanted test types globally (so Correlations + everything else won't see them) ---
vald_tests_long_ui <- vald_tests_long_ui %>% filter(!str_detect(str_to_lower(test_type), "iso\\s*prone"))
keep_roster_metrics <- keep_roster_metrics[!str_detect(str_to_lower(keep_roster_metrics), "\\|iso\\s*prone\\|")]


# ---------------------------
# Athleticism score (composite) key
# ---------------------------
ATH_KEY   <- "Composite|Score|Athleticism Score"
HAS_ATH   <- ATH_KEY %in% names(roster_view) || ATH_KEY %in% keep_roster_metrics
ATH_LABEL <- "Athleticism Score"

ath_tooltip_text <- paste(
  "Weighted composite of position-group percentiles:",
  "Jump Height 12.5%",
  "Force at Peak Power 12.5%",
  "RSI-modified 10%",
  "Eccentric Braking Impulse 10%",
  "Avg Max Force (L/R) 15%",
  "Max Imbalance 5% (lower is better)",
  "Max Velocity 10%",
  "Flying 10s 10%",
  "Max Effort Acceleration 7.5%",
  "Max Effort Deceleration 7.5% (lower is better)",
  "Weights re-normalize if some metrics are missing.",
  sep = "\n"
)

make_dt_container_with_ath_tooltip <- function(col_names) {
  # col_names = names(df_disp) AFTER you renamed them (so it includes "Athleticism Score")
  tags$table(
    class = "display",
    tags$thead(
      tags$tr(
        lapply(col_names, function(nm) {
          if (nm == "Athleticism Score") {
            tags$th(
              tags$span(HTML("Athleticism Score&nbsp;ⓘ"), title = ath_tooltip_text)
            )
          } else {
            tags$th(nm)
          }
        })
      )
    )
  )
}


metric_lut <- tibble::tibble(
  metric_key = keep_roster_metrics,
  system = sapply(keep_roster_metrics, function(x) strsplit(x, "\\|")[[1]][1]),
  test_type = sapply(keep_roster_metrics, function(x) {
    parts <- strsplit(x, "\\|")[[1]]
    if (length(parts) >= 2) parts[2] else NA_character_
  }),
  label = sapply(keep_roster_metrics, function(x) {
    parts <- strsplit(x, "\\|")[[1]]
    paste(parts[-1], collapse = " — ")
  })
) %>%
  arrange(system, test_type, label)

# match metric_name even if it has " (units)" etc at end
norm_metric <- function(x) str_trim(gsub("\\s*\\([^\\)]*\\)\\s*$", "", x))

pick_metric_key_by_name <- function(system_name, metric_name_target, df_keys) {
  # df_keys must have columns: metric_key, system, metric_name
  target <- norm_metric(metric_name_target)
  
  cand <- df_keys %>%
    filter(system == system_name) %>%
    mutate(metric_name_norm = norm_metric(metric_name))
  
  hit <- cand %>%
    filter(metric_name_norm == target) %>%
    dplyr::slice_head(n = 1)
  
  if (nrow(hit) == 0) return(NA_character_)
  hit$metric_key[1]
}




# ---------------------------
# Radar metric selection (fixed lists, pick best match by fill_frac)
# ---------------------------

metric_name_from_key <- function(mk) {
  parts <- strsplit(mk, "\\|")[[1]]
  if (length(parts) >= 3) parts[3] else mk
}

pick_best_keys_for_metric_names <- function(source_name, metric_names, fill_summary) {
  # source_name: "ForceDecks" or "NordBord"
  # metric_names: character vector of desired metric_name(s)
  # returns: named vector of metric_keys (names = metric_names), NA for missing
  
  cand <- keep_roster_metrics[startsWith(keep_roster_metrics, paste0(source_name, "|"))]
  if (length(cand) == 0) {
    out <- rep(NA_character_, length(metric_names))
    names(out) <- metric_names
    return(out)
  }
  
  cand_tbl <- tibble::tibble(
    metric_key = cand,
    metric_name = vapply(cand, metric_name_from_key, character(1))
  ) %>%
    left_join(fill_summary %>% select(metric_key, fill_frac), by = "metric_key") %>%
    mutate(fill_frac = dplyr::coalesce(fill_frac, 0))
  
  out <- vapply(metric_names, function(target_nm) {
    hits <- cand_tbl %>%
      filter(tolower(metric_name) == tolower(target_nm)) %>%
      arrange(desc(fill_frac))
    
    if (nrow(hits) == 0) return(NA_character_)
    hits$metric_key[1]
  }, character(1))
  
  names(out) <- metric_names
  out
}

# Radar Plot Metrics
force_metric_names <- c(
  "Jump Height (Imp-Mom)",
  "RSI-modified (Imp-Mom)",
  "Force at Peak Power",
  "Force at Zero Velocity",
  "Eccentric Braking Impulse",
  "Concentric Impulse"
)


nord_metric_names <- c(
  "L Max Force",
  "R Max Force",
  "L Max Impulse",
  "R Max Impulse",
  "Max Imbalance")



# Resolve to actual metric_keys in your data (best match by fill_frac)
force_metric_keys_map <- pick_best_keys_for_metric_names("ForceDecks", force_metric_names, fill_summary)
nord_metric_keys_map  <- pick_best_keys_for_metric_names("NordBord",   nord_metric_names,  fill_summary)

radar_force_metrics <- unname(force_metric_keys_map[!is.na(force_metric_keys_map)])
radar_nord_metrics  <- unname(nord_metric_keys_map[!is.na(nord_metric_keys_map)])

radar_force_labels <- tibble::tibble(
  metric_key = unname(force_metric_keys_map),
  radar_label = names(force_metric_keys_map)
) %>% filter(!is.na(metric_key))

radar_nord_labels <- tibble::tibble(
  metric_key = unname(nord_metric_keys_map),
  radar_label = names(nord_metric_keys_map)
) %>% filter(!is.na(metric_key))


catapult_metric_names <- c(
  "Player Load Per Minute",
  "Max Vel",
  "Max Effort Acceleration",
  "Max Effort Deceleration",
  "Total Player Load",
  "Explosive Efforts",
  "High Speed Distance (12 mph)",
  "Sprint Distance (16 mph)"
)

catapult_metric_keys_map <- pick_best_keys_for_metric_names("Catapult", catapult_metric_names, fill_summary)

radar_catapult_metrics <- unname(catapult_metric_keys_map[!is.na(catapult_metric_keys_map)])

radar_catapult_labels <- tibble::tibble(
  metric_key = unname(catapult_metric_keys_map),
  radar_label = names(catapult_metric_keys_map)
) %>% filter(!is.na(metric_key))

radar_catapult_labels <- radar_catapult_labels %>%
  mutate(radar_label = if_else(radar_label == "Total Player Load", "Recent Player Load", radar_label))



smartspeed_metric_names <- c(
  "Best Split Seconds"
)

smartspeed_metric_keys_map <- pick_best_keys_for_metric_names(
  "SmartSpeed",
  smartspeed_metric_names,
  fill_summary
)

radar_smartspeed_metrics <- unname(smartspeed_metric_keys_map[!is.na(smartspeed_metric_keys_map)])

radar_smartspeed_labels <- tibble::tibble(
  metric_key = unname(smartspeed_metric_keys_map),
  radar_label = c("Flying 10s")
) %>% filter(!is.na(metric_key))


# --- Pick important metrics for radar (data-driven) ---
pick_radar_metrics <- function(fill_summary, metric_lut, n_total = 6) {
  # fill_summary: metric_key, fill_frac
  # metric_lut: metric_key, system, test_type, label
  
  fs <- fill_summary %>%
    inner_join(metric_lut, by = "metric_key") %>%
    arrange(desc(fill_frac))
  
  # prefer "athletic" keywords if present; fallback to top fill
  key_pat <- "CMJ|JUMP|RSI|SPRINT|SPLIT|PEAK POWER|POWER|IMPULSE|FORCE|TORQUE|RFD|NORD|HAMSTRING|ECC"
  preferred <- fs %>% filter(str_detect(str_to_upper(metric_key), key_pat))
  pool <- bind_rows(preferred, fs) %>% distinct(metric_key, .keep_all = TRUE)
  
  # balance across systems if possible
  out <- pool %>%
    group_by(system) %>%
    slice_head(n = ceiling(n_total / n_distinct(pool$system))) %>%
    ungroup() %>%
    slice_head(n = n_total)
  
  out$metric_key
}

radar_metrics <- pick_radar_metrics(fill_summary, metric_lut, n_total = 6)
radar_labels  <- metric_lut %>%
  filter(metric_key %in% radar_metrics) %>%
  transmute(metric_key, radar_label = label) %>%
  distinct()

key_metric_axes <- {
  pats <- c("CMJ", "JUMP", "RSI", "IMPULSE", "AVG FORCE", "PEAK FORCE", "IMBALANCE", "ISO")
  cand <- keep_roster_metrics[str_detect(toupper(keep_roster_metrics), paste(pats, collapse="|"))]
  cand <- unique(cand)
  if (length(cand) < 6) cand <- unique(c(cand, keep_roster_metrics))
  head(cand, 8)
}

pretty_axis <- function(mk) {
  parts <- strsplit(mk, "\\|")[[1]]
  if (length(parts) >= 3) paste0(parts[2], " — ", paste(parts[3:length(parts)], collapse=" | ")) else mk
}


# ---- metric label helpers ----
pretty_metric_key <- function(mk) {
  # mk is "ForceDecks|CMJ|Additional Load" etc
  parts <- strsplit(mk, "\\|")[[1]]
  if (length(parts) >= 3) {
    paste0(parts[2], " — ", paste(parts[3:length(parts)], collapse = " | "))
  } else {
    mk
  }
}

metric_system <- function(mk) {
  # first token before |
  strsplit(mk, "\\|")[[1]][1]
}

metric_subcat <- function(mk) {
  # second token (test_type) if present
  parts <- strsplit(mk, "\\|")[[1]]
  if (length(parts) >= 2) parts[2] else NA_character_
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Roster-wide percentile lookup for a specific player + metric_key
get_player_pct <- function(player_id, metric_key) {
  x <- roster_percentiles_long %>%
    filter(player_id == !!player_id, metric_key == !!metric_key) %>%
    dplyr::slice_head(n = 1)
  if (nrow(x) == 0) return(NA_real_)
  x$percentile[1]
}

# Select a "primary percentile metric" for roster filter slider:
# if you don't have categories yet, default to A representative metric
default_metric_for_slider <- {
  # prefer something that looks like CMJ / jump / sprint if present
  candidates <- keep_roster_metrics
  pick <- candidates[str_detect(toupper(candidates), "CMJ|JUMP|10Y|10 Y|SPLIT|SPRINT|RSI|IMPULSE")]
  if (length(pick) == 0) candidates[1] else pick[1]
}

# Detect optional roster metadata columns
group_col <- if ("pos_group" %in% names(roster_view)) "pos_group" else if ("Group" %in% names(roster_view)) "Group" else NULL
pos_col   <- if ("pos_position" %in% names(roster_view)) "pos_position" else if ("Position" %in% names(roster_view)) "Position" else NULL

# Catapult tab position column (robust)
cat_pos_col <- if (!is.null(pos_col)) {
  pos_col
} else if ("pos_position" %in% names(roster_view)) {
  "pos_position"
} else if ("Position" %in% names(roster_view)) {
  "Position"
} else {
  NULL
}


class_col <- if ("class_year_base" %in% names(roster_view)) {
  "class_year_base"     # best for slicer: FR/SO/JR/SR
} else if ("class_year" %in% names(roster_view)) {
  "class_year"          # fallback if you kept RS-SR style labels
} else {
  NULL
}

headshot_col <- if ("headshot" %in% names(roster_view)) "headshot" else NULL


# For radar (until you build speed/power/strength/conditioning),
# we use "NordBord average percentile" vs "ForceDecks average percentile"
# computed from roster_percentiles_long and metric_key prefixes.
nord_metrics  <- keep_roster_metrics[startsWith(keep_roster_metrics, "NordBord|")]
force_metrics <- keep_roster_metrics[startsWith(keep_roster_metrics, "ForceDecks|")]


# ---------------------------
# UI
# ---------------------------

ui <- navbarPage(
  title = tags$span(
    "UCLA Performance Database",
    style = "font-weight:800; letter-spacing:0.5px;"
  ),
  id = "main_nav",
  header = tagList(
    tags$link(rel="stylesheet", href="ucla.css"),
    tags$link(rel="icon", type="image/x-icon", href="favicon_v2.ico")
  ),
  # ============== Page 1: Roster Explorer ==============
  tabPanel(
    "Roster Explorer",
    sidebarLayout(
      sidebarPanel(
        if (!is.null(class_col)) {
          selectInput("class_filter", "Class Year",
                      choices = c("All", sort(unique(na.omit(roster_view[[class_col]])))),
                      selected = "All", multiple = TRUE)
        },
        if (!is.null(pos_col)) {
          selectInput("pos_filter", "Position",
                      choices = c("All", sort(unique(na.omit(roster_view[[pos_col]])))),
                      selected = "All", multiple = TRUE)
        },
        if (!is.null(group_col)) {
          selectInput("group_filter", "Group",
                      choices = c("All", sort(unique(na.omit(roster_view[[group_col]])))),
                      selected = "All", multiple = TRUE)
        },
        
        selectInput(
          "pctl_system",
          "System",
          choices = c("All", unique(metric_lut$system)),
          selected = "All"
        ),
        
        selectInput(
          "pctl_test",
          "Test Type",
          choices = NULL
        ),
        
        selectInput(
          "pctl_metric",
          "Metric",
          choices = NULL,
          multiple = TRUE
        ),
        
        # sliderInput(
        #   "min_pctl",
        #   "Min percentile",
        #   min = 0, max = 100, value = 0, step = 1
        # ),
        
        tags$hr(),
        helpText("Click a row to open the Player Card."),
        helpText(
          style = "font-size:11px; color:#6b7280; margin-top:4px;",
          HTML("Values shown are each player's <b>season best</b>, except 
       <b>Athlete Standing Weight</b> (most recent) and 
       <b>Recent Player Load</b> (most recent session).")
        ),
        width = 3
      ),
      mainPanel(
        DTOutput("roster_table"),
        width = 9
      )
    )
  ),
  
  # ============== Page 2: Player Card ==============
  tabPanel(
    "Player Card",
    fluidPage(
      uiOutput("player_banner"),
      
      fluidRow(
        column(
          4,
          selectInput(
            "player_pick",
            "Select player",
            choices = sort(unique(roster_view$player_name)),
            selected = sort(unique(roster_view$player_name))[1]
          )
        )
      ),
      tags$hr(),
      
      fluidRow(
        column(
          width = 4,
          h4("ForceDecks Radar"),
          plotlyOutput("radar_force_plot", height = 280),
          
          h4("NordBord Radar"),
          plotlyOutput("radar_nord_plot", height = 280),
          
          h4("Catapult Radar"),
          plotlyOutput("radar_catapult_plot", height = 280),
          
          
          h4("Quick tags"),
          uiOutput("player_tags")
        ),
        column(
          width = 8,
          tabsetPanel(
            tabPanel(
              "Positional Comparison",
              fluidRow(
                column(
                  3,
                  radioButtons(
                    "poscomp_scope",
                    "Compare within",
                    choices = c("Position" = "pos", "Group" = "group"),
                    selected = "pos",
                    inline = TRUE
                  ),
                  checkboxInput("poscomp_show_radar", "Show overlay radar", value = TRUE)
                ),
                column(
                  9,
                  tags$p(
                    style = "font-size:12px; color:#6b7280; margin-bottom:6px;",
                    HTML("Values shown are each player's <b>career best</b>, except 
         <b>Athlete Standing Weight</b>, <b>Recent Player Load</b>, 
         <b>Player Load Per Minute</b>, <b>High Speed Distance</b>, 
         <b>Sprint Distance</b>, and <b>Explosive Efforts</b> 
         (most recent session).")
                  ),
                  DTOutput("poscomp_table"),
                  plotlyOutput("poscomp_radar", height = 420)
                )
              )
            ),
            tabPanel(
              "Trends",
              fluidRow(
                column(
                  3,
                  selectInput("trend_source", "Source", choices = NULL, selected = "All"),
                  selectInput("trend_test",   "Test",   choices = NULL, selected = "All"),
                  selectInput("trend_metric", "Metric", choices = NULL, selected = "All"),
                  selectInput(
                    "trend_overlay_player",
                    "Overlay another player (optional)",
                    choices = NULL,
                    selected = "None"
                  ),
                  checkboxInput("overlay_pos_group", "Overlay position group (all players)", value = FALSE),
                  checkboxInput("overlay_pos_avg", "Overlay position average", value = TRUE)
                ),
                column(9, plotlyOutput("trend_plot", height = 360))
              )
            ),
            tabPanel(
              "Player Comparisons",
              fluidRow(
                column(
                  4,
                  selectInput(
                    "compare_player",
                    "Compare player",
                    choices = sort(unique(roster_view$player_name)),
                    selected = sort(unique(roster_view$player_name))[1]
                  )
                ),
                column(
                  8,
                  h4("ForceDecks Radar"),
                  plotlyOutput("compare_force_radar_plot", height = 280),
                  tags$hr(),
                  h4("NordBord Radar"),
                  plotlyOutput("compare_nord_radar_plot", height = 280),
                  tags$hr(),
                  h4("Catapult Radar"),
                  plotlyOutput("compare_catapult_radar_plot", height = 280)
                )
              )
            ),
            tabPanel(
              "Performance",
              fluidRow(
                column(
                  3,
                  selectInput("perf_source", "Source", choices = NULL, selected = "All"),
                  selectInput("perf_test",   "Test Type",   choices = NULL, selected = "All"),
                  selectInput("perf_metric", "Metric", choices = NULL, selected = "All"),
                  dateRangeInput(
                    "perf_dates", "Date range",
                    start = Sys.Date() - 90,
                    end   = Sys.Date()
                  )
                ),
                column(
                  9,
                  DTOutput("player_perf_table")
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # Catapult tab
  tabPanel(
    "Catapult",
    sidebarLayout(
      sidebarPanel(
        dateRangeInput(
          "cat_dates",
          "Date range",
          start = min(vald_tests_long_ui$date[vald_tests_long_ui$source == "Catapult"], na.rm = TRUE),
          end   = max(vald_tests_long_ui$date[vald_tests_long_ui$source == "Catapult"], na.rm = TRUE)
        ),
        if (!is.null(cat_pos_col)) {
          selectInput(
            "cat_pos_filter", "Position",
            choices = c("All", sort(unique(na.omit(roster_view[[cat_pos_col]])))),
            selected = "All",
            multiple = TRUE
          )
        }
        ,
        helpText("Charts show latest Catapult value per player within the date range."),
        width = 3
      ),
      mainPanel(
        fluidRow(
          column(6, plotlyOutput("cat_plot_player_load", height = 320)),
          column(6, plotlyOutput("cat_plot_player_load_min", height = 320))
        ),
        fluidRow(
          column(6, plotlyOutput("cat_plot_hsd_12", height = 320)),
          column(6, plotlyOutput("cat_plot_sprint_16", height = 320))
        ),
        fluidRow(
          column(6, plotlyOutput("cat_plot_max_v", height = 320)),
          column(6, plotlyOutput("cat_plot_explosive", height = 320))
        ),
        width = 9
      )
    )
  ),
  
  
  # ============== Page 3: Correlations ==============
  tabPanel(
    "Correlations",
    sidebarLayout(
      sidebarPanel(
        tags$h4("Metric X"),
        selectInput("corr_x_source", "Source (X)", choices = c("All", sort(unique(na.omit(vald_tests_long_ui$source)))), selected = "All"),
        selectInput("corr_x_test",   "Test (X)",   choices = "All", selected = "All"),
        selectInput("corr_x_metric", "Metric X",   choices = character(0)),
        
        tags$hr(),
        
        tags$h4("Metric Y"),
        selectInput("corr_y_source", "Source (Y)", choices = c("All", sort(unique(na.omit(vald_tests_long_ui$source)))), selected = "All"),
        selectInput("corr_y_test",   "Test (Y)",   choices = "All", selected = "All"),
        
        fluidRow(
          column(
            8,
            selectInput("corr_y_metric", "Metric Y (choose 1+)", choices = character(0), multiple = TRUE)
          ),
          column(
            4,
            actionButton("corr_select_all_y", "Select all")
          )
        ),
        
        selectInput("corr_plot_y", "Scatterplot Y", choices = character(0)),
        
        selectInput(
          "corr_highlight_player",
          "Highlight player",
          choices = c("None", sort(unique(roster_view$player_name))),
          selected = "None"
        ),
        
        tags$hr(),
        actionButton("run_corr", "Run"),
        width = 3
      ),
      mainPanel(
        tags$p(
          style = "font-size:12px; color:#6b7280; margin-bottom:6px;",
          HTML("Correlations use each player's <b>most recent value</b> for each metric.")
        ),
        DTOutput("corr_table"),
        plotlyOutput("corr_plot", height = 360),
        width = 9
      )
    )
  )
  
)


# ---------------------------
# Server
# ---------------------------

server <- function(input, output, session) {
  
  
  
  fmt_measure <- function(x) {
    if (is.null(x) || length(x) == 0) return("—")
    s <- as.character(x[1])
    s <- stringr::str_squish(s)
    if (is.na(s) || !nzchar(s)) return("—")
    
    # If it looks like "78.0" or "10.0", strip the trailing .0
    s <- stringr::str_replace(s, "\\.0$", "")
    s
  }
  
  
  roster_allowed_metric_keys <- reactive({
    mk_sys  <- input$pctl_system
    mk_test <- input$pctl_test
    mk_met  <- input$pctl_metric
    
    metric_show <- keep_roster_metrics
    
    if (!is.null(mk_sys) && mk_sys != "All") {
      metric_show <- metric_show[startsWith(metric_show, paste0(mk_sys, "|"))]
    }
    
    if (!is.null(mk_test) && mk_test != "All") {
      metric_show <- metric_show[str_detect(metric_show, paste0("\\|", mk_test, "\\|"))]
    }
    
    # If user picked specific metrics (multi), only allow those
    if (!is.null(mk_met) && length(mk_met) > 0 && !("All" %in% mk_met)) {
      metric_show <- intersect(keep_roster_metrics, mk_met)
    }
    
    metric_show
  })
  
  # "All" behaves like a toggle (works even on first selection)
  make_all_toggle <- function(inputId) {
    prev <- reactiveVal("All")   # ✅ IMPORTANT: start in "All" state
    
    observeEvent(input[[inputId]], {
      sel <- input[[inputId]] %||% character(0)
      prev_sel <- prev() %||% character(0)
      
      # If nothing selected -> revert to All
      if (length(sel) == 0) {
        updateSelectInput(session, inputId, selected = "All")
        prev("All")
        return()
      }
      
      has_all <- "All" %in% sel
      prev_has_all <- "All" %in% prev_sel
      
      # If user selects something while All is active -> drop All
      if (has_all && prev_has_all && length(sel) > 1) {
        new_sel <- setdiff(sel, "All")
        updateSelectInput(session, inputId, selected = new_sel)
        prev(new_sel)
        return()
      }
      
      # If user clicks All while filtered -> revert to All only
      if (has_all && !prev_has_all) {
        updateSelectInput(session, inputId, selected = "All")
        prev("All")
        return()
      }
      
      prev(sel)
    }, ignoreInit = TRUE)
  }
  
  make_all_toggle("group_filter")
  make_all_toggle("pos_filter")
  make_all_toggle("class_filter")  # optional
  make_all_toggle("pctl_metric")   # ✅ do this too since metric is now multi
  make_all_toggle("cat_pos_filter")
  
  
  
  # Update test types when system changes
  observeEvent(input$pctl_system, {
    sub <- metric_lut
    if (!is.null(input$pctl_system) && input$pctl_system != "All") {
      sub <- sub %>% filter(system == input$pctl_system)
    }
    
    tests <- sub %>%
      pull(test_type) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "pctl_test",
                      choices = c("All", tests),
                      selected = "All")
  }, ignoreInit = FALSE)
  
  
  
  # Update metrics when test type changes
  observeEvent(list(input$pctl_system, input$pctl_test), {
    sub <- metric_lut
    
    if (!is.null(input$pctl_system) && input$pctl_system != "All") {
      sub <- sub %>% filter(system == input$pctl_system)
    }
    if (!is.null(input$pctl_test) && input$pctl_test != "All") {
      sub <- sub %>% filter(test_type == input$pctl_test)
    }
    
    metric_choices <- c("All" = "All", setNames(sub$metric_key, sub$label))
    
    updateSelectInput(
      session, "pctl_metric",
      choices = metric_choices,
      selected = "All"
    )
  }, ignoreInit = FALSE)
  
  
  
  # ---------- Roster filtered ----------
  roster_filtered <- reactive({
    df <- roster_view
    
    if (!is.null(group_col) && !is.null(input$group_filter) && length(input$group_filter) > 0 && !("All" %in% input$group_filter)) {
      df <- df[df[[group_col]] %in% input$group_filter, , drop = FALSE]
    }
    # optional metadata filters
    if (!is.null(pos_col) && !is.null(input$pos_filter) && length(input$pos_filter) > 0 && !("All" %in% input$pos_filter)) {
      df <- df[df[[pos_col]] %in% input$pos_filter, , drop = FALSE]
    }
    if (!is.null(class_col) && !is.null(input$class_filter) && length(input$class_filter) > 0 && !("All" %in% input$class_filter)) {
      df <- df[df[[class_col]] %in% input$class_filter, , drop = FALSE]
    }
    
    
    df
  })
  
  default_roster_metrics <- c(
    "Athleticism Score",
    
    # ForceDecks
    "Athlete Standing Weight",
    "Jump Height (Imp-Mom)",
    "RSI-modified",
    "Force at Peak Power",
    "Force at Zero Velocity",
    "Eccentric Braking Impulse",
    "Concentric Impulse",
    
    # NordBord
    "L Max Force",
    "R Max Force",
    "Avg Max Force",
    "L Max Impulse",
    "R Max Impulse",
    "Max Imbalance",
    "Impulse Imbalance",
    
    # Catapult
    "Total Distance",
    "Max Vel",
    "Max Effort Acceleration",
    "Max Effort Deceleration",
    "Total Player Load",
    "Player Load Per Minute",
    
    # SmartSpeed
    "Best Split Seconds"
  )
  
  
  
  output$roster_table <- renderDT({
    df <- roster_filtered()
    
    mk_sys  <- input$pctl_system
    mk_test <- input$pctl_test
    mk_met  <- input$pctl_metric
    
    # start with all metrics
    metric_show <- keep_roster_metrics[
      sapply(keep_roster_metrics, function(x) {
        any(default_roster_metrics %in% sub("^.*\\|", "", x))
      })
    ]
    
    # filter by System
    if (!is.null(mk_sys) && mk_sys != "All") {
      metric_show <- metric_show[startsWith(metric_show, paste0(mk_sys, "|"))]
    }
    
    # filter by Test Type
    if (!is.null(mk_test) && mk_test != "All") {
      metric_show <- metric_show[str_detect(metric_show, paste0("\\|", mk_test, "\\|"))]
    }
    
    # if specific Metrics are chosen (multi), override and show ONLY those
    if (!is.null(mk_met) && length(mk_met) > 0 && !("All" %in% mk_met)) {
      metric_show <- intersect(keep_roster_metrics, mk_met)
    }
    
    # Always include Athleticism Score as a leading metric (if present)
    if (HAS_ATH) {
      metric_show <- unique(c(ATH_KEY, metric_show))
    }
    
    # --- Identify keys for ordering ---
    max_vel_key <- keep_roster_metrics[
      grepl("\\|Max Vel$", keep_roster_metrics)
    ][1]
    
    flying10_key <- keep_roster_metrics[
      grepl("\\|Flying 10s\\|Best Split Seconds$", keep_roster_metrics)
    ][1]
    
    # --- Force Flying 10s to appear after Max Vel ---
    if (!is.na(max_vel_key) && !is.na(flying10_key)) {
      metric_show <- metric_show[metric_show != flying10_key]
      
      pos <- match(max_vel_key, metric_show)
      
      if (!is.na(pos)) {
        metric_show <- append(metric_show, flying10_key, after = pos)
      } else {
        metric_show <- c(metric_show, flying10_key)
      }
    }
    
    # ---- FRONT COLUMNS: force Name, then Position, then Group/Class ----
    front <- c("player_name")
    
    # ✅ class year immediately after player name
    if (!is.null(class_col)) front <- c(front, class_col)
    
    # then position
    if (!is.null(pos_col)) {
      front <- c(front, pos_col)
    } else if ("pos_position" %in% names(df)) {
      front <- c(front, "pos_position")
    }
    
    # then group
    if (!is.null(group_col)) {
      front <- c(front, group_col)
    } else if ("pos_group" %in% names(df)) {
      front <- c(front, "pos_group")
    }
    
    # Put Athleticism immediately after identity columns (if available)
    if (HAS_ATH) {
      metric_show <- unique(c(ATH_KEY, setdiff(metric_show, ATH_KEY)))
    }
    
    cols <- unique(c(front, metric_show))
    
    # Priority cols to appear right after identity columns
    priority_metrics <- c(
      ATH_KEY,
      keep_roster_metrics[grepl("\\|Athlete Standing Weight$",   keep_roster_metrics)][1],
      keep_roster_metrics[grepl("\\|Total Player Load$",         keep_roster_metrics)][1]
    )
    priority_metrics <- priority_metrics[!is.na(priority_metrics)]
    
    # Remaining metrics in original order, excluding the priority ones
    rest_metrics <- setdiff(metric_show, priority_metrics)
    
    cols <- unique(c(front, priority_metrics, rest_metrics))
    
    df_disp <- df %>% dplyr::select(dplyr::any_of(cols))
    
    name_with_headshot <- function(player_name) {
      src <- find_player_headshot(player_name)
      
      if (is.na(src) || !nzchar(src)) {
        return(player_name)
      }
      
      sprintf(
        "<div data-order='%s' style='display:flex; align-items:center; gap:8px;'>
     <img src='%s' style='width:32px; height:32px; border-radius:50%%; object-fit:cover;'/>
     <span>%s</span>
   </div>",
        htmltools::htmlEscape(player_name),
        src,
        htmltools::htmlEscape(player_name)
      )
    }
    
    df_disp <- df_disp %>%
      mutate(player_name = vapply(player_name, name_with_headshot, character(1)))
  
    
    # ---- CLEAN COLUMN NAMES FOR DISPLAY ----
    # 1) clean metric names: keep text after final |
    disp_names <- vapply(
      colnames(df_disp),
      function(x) {
        if (grepl("\\|", x)) sub("^.*\\|", "", x) else x
      },
      character(1)
    )
    
    # 2) friendly labels for the roster identity columns
    disp_names <- dplyr::recode(
      disp_names,
      player_name      = "Player Name",
      pos_position     = "Position",
      pos_group        = "Position Group",
      class_year_base  = "Class Year",
      class_year       = "Class Year"
    )
    
    # ---- Clean up VALD metric column names ----
    disp_names <- gsub("Jump Height \\(Imp-Mom\\) in Inches", "Jump Height", disp_names)
    disp_names <- gsub("Jump Height \\(Imp-Mom\\)", "Jump Height", disp_names)
    disp_names <- gsub(" in Inches", "", disp_names)
    disp_names <- gsub("Total Player Load", "Recent Player Load", disp_names)
    disp_names <- gsub("^Best Split Seconds$", "Flying 10s", disp_names)
    
    colnames(df_disp) <- disp_names
    
    
    container <- make_dt_container_with_ath_tooltip(names(df_disp))
    
    dt <- DT::datatable(
      df_disp,
      container = container,
      escape = FALSE,
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        searchHighlight = TRUE,
        columnDefs = list(
          list(targets = 0, className = "dt-nowrap")
        ),
        order = if ("Athleticism Score" %in% names(df_disp)) {
          list(list(which(names(df_disp) == "Athleticism Score") - 1, "desc"))
        } else {
          list()
        }
      )
    )
    
    
    
    # Color-code Athleticism Score if present
    if ("Athleticism Score" %in% names(df_disp)) {
      dt <- dt %>%
        DT::formatRound("Athleticism Score", 1) %>%
        DT::formatStyle(
          "Athleticism Score",
          backgroundColor = DT::styleInterval(
            c(20, 40, 60, 80),
            c("#ffe5e5", "#ffd6a5", "#fff3b0", "#d9f7be", "#b7eb8f")
          ),
          fontWeight = "bold"
        )
    }
    
    dt
    
  })
  
  # selected player state (set by roster click / picker)
  selected_player <- reactiveVal(NULL)
  
  # ---- init/reset selected player (top of current filtered table) ----
  observeEvent(roster_filtered(), {
    df <- roster_filtered()
    if (nrow(df) == 0) return()
    
    # match DT ordering: Athleticism desc if present
    if (HAS_ATH && ATH_KEY %in% names(df)) {
      df <- df %>% arrange(desc(.data[[ATH_KEY]]), player_name)
    } else {
      df <- df %>% arrange(player_name)
    }
    
    top_nm <- df$player_name[1] %||% ""
    cur_nm <- selected_player()
    
    # if nothing selected OR selected no longer in filtered df -> reset
    if (is.null(cur_nm) || !nzchar(cur_nm) || !(cur_nm %in% df$player_name)) {
      selected_player(top_nm)
    }
  }, ignoreInit = FALSE)
  
  # ---- keep player picker choices synced to current roster filters ----
  observe({
    df <- roster_filtered()
    req(nrow(df) > 0)
    
    choices <- df %>% pull(player_name) %>% unique() %>% sort()
    current <- selected_player()
    
    # keep current if valid; otherwise use first (which will be top_nm via observer above)
    selected <- if (!is.null(current) && nzchar(current) && current %in% choices) current else (choices[1] %||% "")
    
    updateSelectInput(session, "player_pick", choices = choices, selected = selected)
  })
  
  # Picker -> selected player
  observeEvent(input$player_pick, {
    nm <- input$player_pick
    if (!is.null(nm) && nzchar(nm)) selected_player(nm)
  }, ignoreInit = TRUE)
  
  # Update compare_player when selected changes
  observeEvent(selected_player(), {
    nm <- selected_player()
    players <- sort(unique(roster_view$player_name))
    pick <- players[players != nm][1] %||% nm
    updateSelectInput(session, "compare_player", choices = players, selected = pick)
  }, ignoreInit = TRUE)
  
  # Table click -> selected player
  observeEvent(input$roster_table_rows_selected, {
    idx <- input$roster_table_rows_selected
    df <- roster_filtered()
    if (length(idx) == 1 && nrow(df) >= idx) {
      selected_player(df$player_name[idx])
    }
  }, ignoreInit = TRUE)
  
  
  # ---------- Populate Performance/Trend filter dropdowns ----------
  
  player_pid <- reactive({
    nm <- selected_player()
    pid <- roster_view %>%
      dplyr::filter(player_name == nm) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::pull(player_id)
    
    req(length(pid) == 1)
    pid
  })
  
  
  poscomp_cohort <- reactive({
    nm <- selected_player()
    row <- roster_view %>% filter(player_name == nm) %>% dplyr::slice_head(n = 1)
    req(nrow(row) == 1)
    
    scope <- input$poscomp_scope %||% "pos"
    
    # choose column based on scope + availability
    comp_col <- NULL
    if (scope == "pos" && !is.null(pos_col)) comp_col <- pos_col
    if (scope == "group" && !is.null(group_col)) comp_col <- group_col
    if (is.null(comp_col)) return(roster_view[0, , drop = FALSE])
    
    key_val <- row[[comp_col]][1]
    req(!is.na(key_val), nzchar(as.character(key_val)))
    
    roster_view %>% filter(.data[[comp_col]] == key_val)
  })
  
  
  active_metric_keys <- reactive({
    sub <- metric_lut
    
    # System filter
    if (!is.null(input$pctl_system) && input$pctl_system != "All") {
      sub <- sub %>% filter(system == input$pctl_system)
    }
    
    # Test Type filter
    if (!is.null(input$pctl_test) && input$pctl_test != "All") {
      sub <- sub %>% filter(test_type == input$pctl_test)
    }
    
    keys <- sub$metric_key
    
    # Metric filter (multi-select): if user picked specific metric_key(s), override
    if (!is.null(input$pctl_metric) && length(input$pctl_metric) > 0 && !("All" %in% input$pctl_metric)) {
      keys <- intersect(keys, input$pctl_metric)
    }
    
    unique(keys)
  })
  
  perf_filtered_df <- reactive({
    df <- vald_tests_long_ui %>% filter(player_id == player_pid())
    
    keys <- active_metric_keys()
    if (!is.null(keys) && length(keys) > 0) {
      df <- df %>% filter(metric_key %in% keys)
    }
    
    if (!is.null(input$perf_source) && input$perf_source != "All") {
      df <- df %>% filter(source == input$perf_source)
    }
    if (!is.null(input$perf_test) && input$perf_test != "All") {
      df <- df %>% filter(test_type == input$perf_test)
    }
    if (!is.null(input$perf_metric) && input$perf_metric != "All") {
      df <- df %>% filter(metric_name == input$perf_metric)
    }
    if (!is.null(input$perf_dates) && all(!is.na(input$perf_dates))) {
      df <- df %>% filter(date >= input$perf_dates[1], date <= input$perf_dates[2])
    }
    
    df
  })
  
  trend_filtered_df <- reactive({
    df <- vald_tests_long_ui %>% filter(player_id == player_pid())
    
    keys <- active_metric_keys()
    if (!is.null(keys) && length(keys) > 0) {
      df <- df %>% filter(metric_key %in% keys)
    }
    
    if (!is.null(input$trend_source) && input$trend_source != "All") {
      df <- df %>% filter(source == input$trend_source)
    }
    if (!is.null(input$trend_test) && input$trend_test != "All") {
      df <- df %>% filter(test_type == input$trend_test)
    }
    if (!is.null(input$trend_metric) && input$trend_metric != "All") {
      df <- df %>% filter(metric_name == input$trend_metric)
    }
    
    df
  })
  
  
  observeEvent(selected_player(), {
    df <- vald_tests_long_ui %>% filter(player_id == player_pid())
    keys <- active_metric_keys()
    if (!is.null(keys) && length(keys) > 0) df <- df %>% filter(metric_key %in% keys)
    
    sources <- sort(unique(na.omit(df$source)))
    
    updateSelectInput(session, "perf_source", choices = c("All", sources), selected = "All")
    updateSelectInput(session, "perf_test",   choices = "All", selected = "All")
    updateSelectInput(session, "perf_metric", choices = "All", selected = "All")
  }, ignoreInit = FALSE)
  
  observeEvent(input$perf_source, {
    df <- vald_tests_long_ui %>% filter(player_id == player_pid())
    keys <- active_metric_keys()
    if (!is.null(keys) && length(keys) > 0) df <- df %>% filter(metric_key %in% keys)
    
    if (!is.null(input$perf_source) && input$perf_source != "All") {
      df <- df %>% filter(source == input$perf_source)
    }
    
    tests <- sort(unique(na.omit(df$test_type)))
    updateSelectInput(session, "perf_test", choices = c("All", tests), selected = "All")
  }, ignoreInit = FALSE)
  
  observeEvent(c(input$perf_source, input$perf_test), {
    df <- vald_tests_long_ui %>% filter(player_id == player_pid())
    keys <- active_metric_keys()
    if (!is.null(keys) && length(keys) > 0) df <- df %>% filter(metric_key %in% keys)
    
    if (!is.null(input$perf_source) && input$perf_source != "All") {
      df <- df %>% filter(source == input$perf_source)
    }
    if (!is.null(input$perf_test) && input$perf_test != "All") {
      df <- df %>% filter(test_type == input$perf_test)
    }
    
    metrics <- sort(unique(na.omit(df$metric_name)))
    metrics <- metrics[nzchar(metrics)]
    updateSelectInput(session, "perf_metric", choices = c("All", metrics), selected = "All")
  }, ignoreInit = FALSE)
  
  
  observeEvent(selected_player(), {
    df <- vald_tests_long_ui %>% filter(player_id == player_pid())
    keys <- active_metric_keys()
    if (!is.null(keys) && length(keys) > 0) df <- df %>% filter(metric_key %in% keys)
    
    sources <- sort(unique(na.omit(df$source)))
    
    updateSelectInput(session, "trend_source", choices = c("All", sources), selected = "All")
    updateSelectInput(session, "trend_test",   choices = "All", selected = "All")
    updateSelectInput(session, "trend_metric", choices = "All", selected = "All")
  }, ignoreInit = FALSE)
  
  
  observeEvent(input$trend_source, {
    df <- vald_tests_long_ui %>% filter(player_id == player_pid())
    keys <- active_metric_keys()
    if (!is.null(keys) && length(keys) > 0) df <- df %>% filter(metric_key %in% keys)
    
    if (!is.null(input$trend_source) && input$trend_source != "All") {
      df <- df %>% filter(source == input$trend_source)
    }
    
    tests <- sort(unique(na.omit(df$test_type)))
    updateSelectInput(session, "trend_test", choices = c("All", tests), selected = "All")
  }, ignoreInit = FALSE)
  
  
  observeEvent(c(input$trend_source, input$trend_test), {
    df <- vald_tests_long_ui %>% filter(player_id == player_pid())
    keys <- active_metric_keys()
    if (!is.null(keys) && length(keys) > 0) df <- df %>% filter(metric_key %in% keys)
    
    if (!is.null(input$trend_source) && input$trend_source != "All") {
      df <- df %>% filter(source == input$trend_source)
    }
    if (!is.null(input$trend_test) && input$trend_test != "All") {
      df <- df %>% filter(test_type == input$trend_test)
    }
    
    metrics <- sort(unique(na.omit(df$metric_name)))
    metrics <- metrics[nzchar(metrics)]
    updateSelectInput(session, "trend_metric", choices = c("All", metrics), selected = "All")
  }, ignoreInit = FALSE)
  
  
  
  # overlay player choices
  observe({
    players <- sort(unique(roster_view$player_name))
    updateSelectInput(session, "trend_overlay_player",
                      choices = c("None", players),
                      selected = "None")
  })

  
  # ---------- Player banner ----------
  output$player_banner <- renderUI({
    nm <- selected_player()
    row <- roster_view %>% filter(player_name == nm) %>% dplyr::slice_head(n = 1)
    if (nrow(row) == 0) return(NULL)
    
    group_val <- if (!is.null(group_col)) row[[group_col]][1] else NA
    pos_val   <- if (!is.null(pos_col))   row[[pos_col]][1] else NA
    class_val <- if (!is.null(class_col)) row[[class_col]][1] else NA
    headshot <- find_player_headshot(nm)
    
    
    ht_val   <- if ("height_display"   %in% names(row)) fmt_measure(row$height_display)   else "—"
    wt_val   <- if ("weight_display"   %in% names(row)) fmt_measure(row$weight_display)   else "—"
    wing_val <- if ("wingspan_display" %in% names(row)) fmt_measure(row$wingspan_display) else "—"
    hand_val <- if ("hand_display"     %in% names(row)) fmt_measure(row$hand_display)     else "—"
    arm_val  <- if ("arm_display"      %in% names(row)) fmt_measure(row$arm_display)      else "—"
    
    
    photos <- list(
      front = find_player_photo(nm, "front"),
      side  = find_player_photo(nm, "side"),
      back  = find_player_photo(nm, "back")
    )
    
    photo_thumb_click <- function(src, view_label, input_id) {
      box_style <- paste(
        "width:90px;",
        "height:90px;",
        "border-radius:8px;",
        "border:1px solid #e5e7eb;",
        "background:#f3f4f6;",
        "object-fit:cover;",
        "object-position:50% 22%;",  # 👈 move crop upward (try 10–25%)
        "display:block;",
        "cursor:pointer;"
      )
      
      if (!is.na(src) && nzchar(src)) {
        tags$img(
          src = src,
          title = paste("Click to enlarge —", view_label),
          style = box_style,
          onclick = sprintf(
            "Shiny.setInputValue('%s', Math.random(), {priority: 'event'})",
            input_id
          )
        )
      } else {
        tags$div(
          title = paste0("Missing: ", view_label),
          style = paste0(
            box_style,
            "display:flex;align-items:center;justify-content:center;font-size:11px;color:#6b7280;"
          ),
          "—"
        )
      }
    }
    
    # --- modal for enlarged photo ---
    modal_id <- sprintf("modal_%s", stringr::str_replace_all(tolower(nm), "[^a-z0-9]", "_"))
    
    showModal_click <- function(src, view_label) {
      if (is.na(src) || !nzchar(src)) return(NULL)
      
      sprintf(
        "var modal=document.getElementById('%s'); modal.style.display='block';",
        modal_id
      )
    }
    
    modal_content <- function(src, view_label) {
      if (is.na(src) || !nzchar(src)) return(NULL)
      
      tags$div(
        id = modal_id,
        style = "display:none; position:fixed; z-index:9999; left:0; top:0; width:100%; height:100%; background-color:rgba(0,0,0,0.6);",
        tags$div(
          style = "position:relative; background-color:#fff; margin:5% auto; padding:0; width:auto; max-width:90vh; box-shadow:0 4px 20px rgba(0,0,0,0.3);",
          tags$span(
            style = "color:#aaa; float:right; font-size:32px; font-weight:bold; padding:12px 16px; cursor:pointer;",
            onclick = sprintf("document.getElementById('%s').style.display='none'", modal_id),
            HTML("&times;")
          ),
          tags$img(
            src = src,
            style = "width:100%; display:block; object-fit:contain;"
          )
        )
      )
    }
    
    # end of readout content block
    
    fluidPage(
      tags$style(HTML("
        .player-banner-grid { display: grid; grid-template-columns: auto 1fr; gap: 20px; margin-bottom: 20px; }
        .player-banner-left { display: flex; flex-direction: column; gap: 10px; }
        .player-banner-right { display: flex; flex-direction: column; justify-content: space-around; }
        .player-banner-name { font-size: 28px; font-weight: bold; }
        .player-banner-meta { display: flex; gap: 16px; flex-wrap: wrap; }
        .player-banner-meta-item { display: flex; flex-direction: column; }
        .player-banner-meta-label { font-size: 12px; color: #6b7280; text-transform: uppercase; }
        .player-banner-meta-value { font-size: 16px; font-weight: 600; }
        .player-photos { display: flex; gap: 12px; margin-top: 8px; }
      ")),
      tags$div(
        class = "player-banner-grid",
        tags$div(
          class = "player-banner-left",
          tags$div(class = "player-banner-name", nm),
          tags$div(
            class = "player-banner-meta",
            if (!is.na(class_val)) {
              tags$div(
                class = "player-banner-meta-item",
                tags$div(class = "player-banner-meta-label", "Class"),
                tags$div(class = "player-banner-meta-value", class_val)
              )
            },
            if (!is.na(pos_val)) {
              tags$div(
                class = "player-banner-meta-item",
                tags$div(class = "player-banner-meta-label", "Position"),
                tags$div(class = "player-banner-meta-value", pos_val)
              )
            },
            if (!is.na(group_val)) {
              tags$div(
                class = "player-banner-meta-item",
                tags$div(class = "player-banner-meta-label", "Group"),
                tags$div(class = "player-banner-meta-value", group_val)
              )
            }
          ),
          tags$div(
            class = "player-photos",
            photo_thumb_click(photos$front, "Front View", "photo_front"),
            photo_thumb_click(photos$side, "Side View", "photo_side"),
            photo_thumb_click(photos$back, "Back View", "photo_back")
          ),
          modal_content(photos$front, "Front View"),
          modal_content(photos$side, "Side View"),
          modal_content(photos$back, "Back View")
        ),
        tags$div(
          class = "player-banner-right",
          if (nzchar(ht_val)) {
            tags$div(
              class = "player-banner-meta-item",
              tags$div(class = "player-banner-meta-label", "Height"),
              tags$div(class = "player-banner-meta-value", ht_val)
            )
          },
          if (nzchar(wt_val)) {
            tags$div(
              class = "player-banner-meta-item",
              tags$div(class = "player-banner-meta-label", "Weight"),
              tags$div(class = "player-banner-meta-value", wt_val)
            )
          },
          if (nzchar(wing_val)) {
            tags$div(
              class = "player-banner-meta-item",
              tags$div(class = "player-banner-meta-label", "Wingspan"),
              tags$div(class = "player-banner-meta-value", wing_val)
            )
          },
          if (nzchar(hand_val)) {
            tags$div(
              class = "player-banner-meta-item",
              tags$div(class = "player-banner-meta-label", "Hand"),
              tags$div(class = "player-banner-meta-value", hand_val)
            )
          },
          if (nzchar(arm_val)) {
            tags$div(
              class = "player-banner-meta-item",
              tags$div(class = "player-banner-meta-label", "Arm"),
              tags$div(class = "player-banner-meta-value", arm_val)
            )
          }
        )
      )
    )
  })
  
  
  # --- Radar plots (ForceDecks + NordBord + Catapult) ---
  output$radar_force_plot <- renderPlotly({
    df <- roster_percentiles_long %>%
      filter(player_id == player_pid(), metric_key %in% radar_force_metrics) %>%
      mutate(metric_key_disp = if_else(
        metric_key %in% radar_force_labels$metric_key,
        radar_force_labels$radar_label[match(metric_key, radar_force_labels$metric_key)],
        pretty_metric_key(metric_key)
      )) %>%
      rename(axis = metric_key_disp)
    
    render_plotly_radar(df, selected_player(), "ForceDecks", "No ForceDecks data")
  })
  
  output$radar_nord_plot <- renderPlotly({
    df <- roster_percentiles_long %>%
      filter(player_id == player_pid(), metric_key %in% radar_nord_metrics) %>%
      mutate(metric_key_disp = if_else(
        metric_key %in% radar_nord_labels$metric_key,
        radar_nord_labels$radar_label[match(metric_key, radar_nord_labels$metric_key)],
        pretty_metric_key(metric_key)
      )) %>%
      rename(axis = metric_key_disp)
    
    render_plotly_radar(df, selected_player(), "NordBord", "No NordBord data")
  })
  
  output$radar_catapult_plot <- renderPlotly({
    df <- roster_percentiles_long %>%
      filter(player_id == player_pid(), metric_key %in% radar_catapult_metrics) %>%
      mutate(metric_key_disp = if_else(
        metric_key %in% radar_catapult_labels$metric_key,
        radar_catapult_labels$radar_label[match(metric_key, radar_catapult_labels$metric_key)],
        pretty_metric_key(metric_key)
      )) %>%
      rename(axis = metric_key_disp)
    
    render_plotly_radar(df, selected_player(), "Catapult", "No Catapult data")
  })
  
  
  output$player_tags <- renderUI({
    pid <- player_pid()
    
    # get all of player's metrics
    all_metrics <- vald_tests_long_ui %>%
      filter(player_id == pid) %>%
      pull(metric_key) %>%
      unique()
    
    tags_to_show <- tibble::tibble(
      label = c("Force Testing", "Speed Testing", "Wellness"),
      detector = c(
        any(stringr::str_detect(all_metrics, "ForceDecks|NordBord")),
        any(stringr::str_detect(all_metrics, "SmartSpeed|Catapult")),
        any(stringr::str_detect(all_metrics, "HRV|Sleep|Readiness|Fatigue"))
      )
    ) %>%
      filter(detector)
    
    if (nrow(tags_to_show) == 0) {
      return(NULL)
    }
    
    tags$div(
      style = "display: flex; gap: 8px; flex-wrap: wrap;",
      lapply(tags_to_show$label, function(lbl) {
        tags$span(
          lbl,
          style = "display: inline-block; padding: 4px 12px; background: #e0e7ff; color: #4c1d95; border-radius: 4px; font-size: 12px; font-weight: 600;"
        )
      })
    )
  })
  
  
  # --- Positional Comparison ---
  output$poscomp_table <- renderDT({
    cohort <- poscomp_cohort()
    if (nrow(cohort) == 0) return(NULL)
    
    scope_label <- if (input$poscomp_scope == "pos") "Position" else "Group"
    
    scope_col <- if (input$poscomp_scope == "pos") pos_col else group_col
    if (is.null(scope_col)) return(NULL)
    
    scope_val <- cohort[[scope_col]][1]
    
    # Wide table
    cand_metric_keys <- metric_lut$metric_key
    cand_names <- metric_lut %>%
      dplyr::select(metric_key, label) %>%
      tibble::deframe()
    
    df_wide <- cohort %>%
      dplyr::select(player_name, dplyr::any_of(cand_metric_keys))
    
    disp_names <- c("Player Name")
    for (mk in intersect(cand_metric_keys, names(df_wide)[-1])) {
      disp_names <- c(disp_names, cand_names[mk] %||% mk)
    }
    
    colnames(df_wide) <- disp_names
    
    # Highlight selected player
    sel_nm <- selected_player()
    
    dt <- DT::datatable(
      df_wide,
      rownames = FALSE,
      options = list(pageLength = 50, scrollX = TRUE),
      caption = htmltools::HTML(paste0("Cohort: ", scope_label, " <b>", scope_val, "</b>"))
    )
    
    if (!is.na(sel_nm)) {
      idx <- which(df_wide[, 1] == sel_nm) - 1
      if (length(idx) > 0 && idx >= 0) {
        dt <- dt %>%
          DT::formatStyle(seq_len(ncol(df_wide)),
                         `border-left` = DT::JS("function(){ if(DT.Settings[0].aoData[row].DT_RowIndex === ", idx, ") return '3px solid #3b82f6'; }"))
      }
    }
    
    dt
  })
  
  
  output$poscomp_radar <- renderPlotly({
    cohort <- poscomp_cohort()
    if (nrow(cohort) == 0) return(NULL)
    
    if (!input$poscomp_show_radar) return(NULL)
    
    sel_pid <- player_pid()
    
    # Pull percentiles for everyone in the cohort
    pids <- cohort$player_id
    names_pids <- setNames(cohort$player_name, cohort$player_id)
    
    df_perc <- roster_percentiles_long %>%
      filter(player_id %in% pids, metric_key %in% radar_force_metrics) %>%
      mutate(
        player_name = names_pids[as.character(player_id)],
        metric_key_disp = if_else(
          metric_key %in% radar_force_labels$metric_key,
          radar_force_labels$radar_label[match(metric_key, radar_force_labels$metric_key)],
          pretty_metric_key(metric_key)
        )
      ) %>%
      rename(axis = metric_key_disp)
    
    sel_nm <- names_pids[as.character(sel_pid)]
    render_plotly_radar(df_perc, sel_nm, "ForceDecks (Position/Group)")
  })
  
  
  # --- Trends ---
  output$trend_plot <- renderPlotly({
    df <- trend_filtered_df()
    if (nrow(df) == 0) {
      plotly::plot_ly() %>%
        plotly::add_text(
          text = "No data available",
          x = 0.5, y = 0.5,
          showarrow = FALSE,
          textfont = list(size = 14, color = "gray")
        ) %>%
        plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
    }
    
    metric_name <- input$trend_metric
    
    # Get trend for this player + metric
    sel_metric_name <- metric_name
    
    df_player <- df %>%
      filter(metric_name == sel_metric_name) %>%
      arrange(date)
    
    if (nrow(df_player) == 0) {
      return(NULL)
    }
    
    sel_nm <- selected_player()
    
    # base trace
    p <- plotly::plot_ly() %>%
      plotly::add_trace(
        data = df_player,
        x = ~date,
        y = ~value,
        mode = "lines+markers",
        type = "scatter",
        name = sel_nm,
        line = list(color = "#3b82f6", width = 2),
        marker = list(size = 4)
      )
    
    # overlay another player
    overlay_nm <- input$trend_overlay_player
    if (!is.null(overlay_nm) && overlay_nm != "None") {
      overlay_pid <- roster_view %>%
        filter(player_name == overlay_nm) %>%
        slice_head(n = 1) %>%
        pull(player_id)
      
      if (length(overlay_pid) > 0) {
        df_overlay <- vald_tests_long_ui %>%
          filter(player_id == overlay_pid, metric_name == sel_metric_name) %>%
          arrange(date)
        
        if (nrow(df_overlay) > 0) {
          p <- p %>%
            plotly::add_trace(
              data = df_overlay,
              x = ~date,
              y = ~value,
              mode = "lines+markers",
              type = "scatter",
              name = overlay_nm,
              line = list(color = "#f59e0b", width = 2, dash = "dash"),
              marker = list(size = 4)
            )
        }
      }
    }
    
    # overlay position group (all players)
    if (input$overlay_pos_group) {
      pos_group_val <- roster_view %>%
        filter(player_name == sel_nm) %>%
        slice_head(n = 1) %>%
        pull(if (!is.null(group_col)) group_col else NULL)
      
      if (!is.na(pos_group_val) && nzchar(as.character(pos_group_val))) {
        group_pids <- roster_view %>%
          filter(.data[[group_col]] == pos_group_val) %>%
          pull(player_id)
        
        group_pids <- group_pids[group_pids != player_pid()]
        
        if (length(group_pids) > 0) {
          df_group <- vald_tests_long_ui %>%
            filter(player_id %in% group_pids, metric_name == sel_metric_name) %>%
            arrange(date, player_id)
          
          if (nrow(df_group) > 0) {
            for (gpid in unique(df_group$player_id)) {
              gpid_name <- roster_view %>%
                filter(player_id == gpid) %>%
                slice_head(n = 1) %>%
                pull(player_name)
              
              df_gpid <- df_group %>% filter(player_id == gpid)
              
              p <- p %>%
                plotly::add_trace(
                  data = df_gpid,
                  x = ~date,
                  y = ~value,
                  mode = "lines",
                  type = "scatter",
                  name = gpid_name,
                  line = list(color = "rgba(107,114,128,0.2)", width = 1),
                  hoverinfo = "skip",
                  showlegend = FALSE
                )
            }
          }
        }
      }
    }
    
    # overlay position average
    if (input$overlay_pos_avg) {
      # position of selected player
      pos_val <- roster_view %>%
        filter(player_name == sel_nm) %>%
        slice_head(n = 1) %>%
        pull(if (!is.null(pos_col)) pos_col else NULL)
      
      if (!is.na(pos_val) && nzchar(as.character(pos_val))) {
        pos_pids <- roster_view %>%
          filter(if (!is.null(pos_col)) .data[[pos_col]] == pos_val else FALSE) %>%
          pull(player_id)
        
        pos_pids <- pos_pids[pos_pids != player_pid()]
        
        if (length(pos_pids) > 0) {
          df_pos <- vald_tests_long_ui %>%
            filter(player_id %in% pos_pids, metric_name == sel_metric_name) %>%
            arrange(date)
          
          if (nrow(df_pos) > 0) {
            df_pos_avg <- df_pos %>%
              group_by(date) %>%
              summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
              arrange(date)
            
            p <- p %>%
              plotly::add_trace(
                data = df_pos_avg,
                x = ~date,
                y = ~value,
                mode = "lines",
                type = "scatter",
                name = paste0("Position (", pos_val, ") Average"),
                line = list(color = "#10b981", width = 2, dash = "dot"),
                marker = list(size = 3)
              )
          }
        }
      }
    }
    
    p %>%
      plotly::layout(
        title = list(text = paste0(sel_metric_name, " Over Time")),
        xaxis = list(title = "Date"),
        yaxis = list(title = sel_metric_name),
        hovermode = "x unified"
      )
  })
  
  
  # --- Player Comparisons Radars ---
  output$compare_force_radar_plot <- renderPlotly({
    comp_nm <- input$compare_player
    
    comp_pid <- roster_view %>%
      filter(player_name == comp_nm) %>%
      slice_head(n = 1) %>%
      pull(player_id)
    
    df1 <- roster_percentiles_long %>%
      filter(player_id == player_pid(), metric_key %in% radar_force_metrics) %>%
      mutate(metric_key_disp = if_else(
        metric_key %in% radar_force_labels$metric_key,
        radar_force_labels$radar_label[match(metric_key, radar_force_labels$metric_key)],
        pretty_metric_key(metric_key)
      )) %>%
      rename(axis = metric_key_disp)
    
    df2 <- roster_percentiles_long %>%
      filter(player_id == comp_pid, metric_key %in% radar_force_metrics) %>%
      mutate(metric_key_disp = if_else(
        metric_key %in% radar_force_labels$metric_key,
        radar_force_labels$radar_label[match(metric_key, radar_force_labels$metric_key)],
        pretty_metric_key(metric_key)
      )) %>%
      rename(axis = metric_key_disp)
    
    df_comb <- bind_rows(df1, df2)
    
    render_plotly_radar(df_comb, selected_player(), "ForceDecks Comparison")
  })
  
  output$compare_nord_radar_plot <- renderPlotly({
    comp_nm <- input$compare_player
    
    comp_pid <- roster_view %>%
      filter(player_name == comp_nm) %>%
      slice_head(n = 1) %>%
      pull(player_id)
    
    df1 <- roster_percentiles_long %>%
      filter(player_id == player_pid(), metric_key %in% radar_nord_metrics) %>%
      mutate(metric_key_disp = if_else(
        metric_key %in% radar_nord_labels$metric_key,
        radar_nord_labels$radar_label[match(metric_key, radar_nord_labels$metric_key)],
        pretty_metric_key(metric_key)
      )) %>%
      rename(axis = metric_key_disp)
    
    df2 <- roster_percentiles_long %>%
      filter(player_id == comp_pid, metric_key %in% radar_nord_metrics) %>%
      mutate(metric_key_disp = if_else(
        metric_key %in% radar_nord_labels$metric_key,
        radar_nord_labels$radar_label[match(metric_key, radar_nord_labels$metric_key)],
        pretty_metric_key(metric_key)
      )) %>%
      rename(axis = metric_key_disp)
    
    df_comb <- bind_rows(df1, df2)
    
    render_plotly_radar(df_comb, selected_player(), "NordBord Comparison")
  })
  
  output$compare_catapult_radar_plot <- renderPlotly({
    comp_nm <- input$compare_player
    
    comp_pid <- roster_view %>%
      filter(player_name == comp_nm) %>%
      slice_head(n = 1) %>%
      pull(player_id)
    
    df1 <- roster_percentiles_long %>%
      filter(player_id == player_pid(), metric_key %in% radar_catapult_metrics) %>%
      mutate(metric_key_disp = if_else(
        metric_key %in% radar_catapult_labels$metric_key,
        radar_catapult_labels$radar_label[match(metric_key, radar_catapult_labels$metric_key)],
        pretty_metric_key(metric_key)
      )) %>%
      rename(axis = metric_key_disp)
    
    df2 <- roster_percentiles_long %>%
      filter(player_id == comp_pid, metric_key %in% radar_catapult_metrics) %>%
      mutate(metric_key_disp = if_else(
        metric_key %in% radar_catapult_labels$metric_key,
        radar_catapult_labels$radar_label[match(metric_key, radar_catapult_labels$metric_key)],
        pretty_metric_key(metric_key)
      )) %>%
      rename(axis = metric_key_disp)
    
    df_comb <- bind_rows(df1, df2)
    
    render_plotly_radar(df_comb, selected_player(), "Catapult Comparison")
  })
  
  
  # --- Performance Table ---
  output$player_perf_table <- renderDT({
    df <- perf_filtered_df()
    
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No data available")))
    }
    
    df_disp <- df %>%
      dplyr::select(date, source, test_type, metric_name, value) %>%
      arrange(desc(date))
    
    colnames(df_disp) <- c("Date", "Source", "Test Type", "Metric", "Value")
    
    DT::datatable(
      df_disp,
      rownames = FALSE,
      options = list(pageLength = 20, scrollX = TRUE)
    ) %>%
      DT::formatDate("Date", method = "toDateString")
  })
  
  
  # --- Correlations ---
  observeEvent(list(input$corr_x_source, input$corr_x_test), {
    df <- vald_tests_long_ui
    
    if (!is.null(input$corr_x_source) && input$corr_x_source != "All") {
      df <- df %>% filter(source == input$corr_x_source)
    }
    if (!is.null(input$corr_x_test) && input$corr_x_test != "All") {
      df <- df %>% filter(test_type == input$corr_x_test)
    }
    
    metrics_x <- sort(unique(na.omit(df$metric_name)))
    metrics_x <- metrics_x[nzchar(metrics_x)]
    
    updateSelectInput(session, "corr_x_metric", choices = metrics_x, selected = metrics_x[1])
  }, ignoreInit = FALSE)
  
  
  observeEvent(list(input$corr_y_source, input$corr_y_test), {
    df <- vald_tests_long_ui
    
    if (!is.null(input$corr_y_source) && input$corr_y_source != "All") {
      df <- df %>% filter(source == input$corr_y_source)
    }
    if (!is.null(input$corr_y_test) && input$corr_y_test != "All") {
      df <- df %>% filter(test_type == input$corr_y_test)
    }
    
    metrics_y <- sort(unique(na.omit(df$metric_name)))
    metrics_y <- metrics_y[nzchar(metrics_y)]
    
    updateSelectInput(session, "corr_y_metric", choices = metrics_y)
    updateSelectInput(session, "corr_plot_y", choices = metrics_y, selected = if (length(metrics_y) > 0) metrics_y[1] else NULL)
  }, ignoreInit = FALSE)
  
  
  observeEvent(input$corr_select_all_y, {
    df <- vald_tests_long_ui
    
    if (!is.null(input$corr_y_source) && input$corr_y_source != "All") {
      df <- df %>% filter(source == input$corr_y_source)
    }
    if (!is.null(input$corr_y_test) && input$corr_y_test != "All") {
      df <- df %>% filter(test_type == input$corr_y_test)
    }
    
    metrics_y <- sort(unique(na.omit(df$metric_name)))
    metrics_y <- metrics_y[nzchar(metrics_y)]
    
    updateSelectInput(session, "corr_y_metric", selected = metrics_y)
  })
  
  
  corr_result <- reactiveVal(NULL)
  
  observeEvent(input$run_corr, {
    # Collect all unique player IDs and their latest values
    metric_x <- input$corr_x_metric
    metrics_y <- input$corr_y_metric
    
    if (length(metrics_y) == 0 || is.null(metric_x)) {
      corr_result(NULL)
      return()
    }
    
    # Get all players' data
    all_metrics <- c(metric_x, metrics_y)
    
    df_wide <- vald_tests_long_ui %>%
      filter(metric_name %in% all_metrics) %>%
      group_by(player_id, metric_name) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      pivot_wider(
        names_from = metric_name,
        values_from = value,
        values_fill = NA
      )
    
    # Compute correlations (Pearson)
    corr_data <- NULL
    if (metric_x %in% names(df_wide)) {
      for (m_y in metrics_y) {
        if (m_y %in% names(df_wide)) {
          x_vals <- df_wide[[metric_x]]
          y_vals <- df_wide[[m_y]]
          
          # only use pairs with both values present
          complete_idx <- !is.na(x_vals) & !is.na(y_vals)
          if (sum(complete_idx) >= 3) {
            rho <- cor(x_vals[complete_idx], y_vals[complete_idx], use = "complete.obs")
            n_pairs <- sum(complete_idx)
            
            corr_data <- bind_rows(
              corr_data,
              tibble::tibble(
                metric_x = metric_x,
                metric_y = m_y,
                correlation = rho,
                n_pairs = n_pairs
              )
            )
          }
        }
      }
    }
    
    corr_result(corr_data)
  })
  
  
  output$corr_table <- renderDT({
    cd <- corr_result()
    if (is.null(cd) || nrow(cd) == 0) {
      return(DT::datatable(data.frame(Message = "Run correlations to see results")))
    }
    
    df_disp <- cd %>%
      mutate(correlation = round(correlation, 3)) %>%
      dplyr::select(metric_y, correlation, n_pairs)
    
    colnames(df_disp) <- c("Metric Y", "Correlation", "N Pairs")
    
    DT::datatable(
      df_disp,
      rownames = FALSE,
      options = list(pageLength = 20, scrollX = TRUE)
    )
  })
  
  
  output$corr_plot <- renderPlotly({
    cd <- corr_result()
    if (is.null(cd) || nrow(cd) == 0) return(NULL)
    
    plot_metric_y <- input$corr_plot_y
    if (!plot_metric_y %in% cd$metric_y) return(NULL)
    
    # Get all players' data
    all_metrics <- c(input$corr_x_metric, plot_metric_y)
    
    df_wide <- vald_tests_long_ui %>%
      filter(metric_name %in% all_metrics) %>%
      group_by(player_id, metric_name) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      pivot_wider(
        names_from = metric_name,
        values_from = value,
        values_fill = NA
      ) %>%
      left_join(roster_view %>% dplyr::select(player_id, player_name), by = "player_id")
    
    col_x <- input$corr_x_metric
    col_y <- plot_metric_y
    
    df_plot <- df_wide %>%
      dplyr::select(all_of(c(col_x, col_y, "player_name"))) %>%
      filter(!is.na(.data[[col_x]]), !is.na(.data[[col_y]]))
    
    if (nrow(df_plot) == 0) return(NULL)
    
    # Highlight player if selected
    highlight_player <- input$corr_highlight_player
    df_plot <- df_plot %>%
      mutate(is_highlight = player_name == highlight_player)
    
    p <- plotly::plot_ly() %>%
      plotly::add_trace(
        data = df_plot %>% filter(!is_highlight),
        x = as.formula(paste0("~`", col_x, "`")),
        y = as.formula(paste0("~`", col_y, "`")),
        mode = "markers",
        type = "scatter",
        marker = list(size = 6, color = "#3b82f6"),
        text = ~player_name,
        hovertemplate = "<b>%{text}</b><br>" %+% col_x %+% ": %{x}<br>" %+% col_y %+% ": %{y}<extra></extra>",
        showlegend = FALSE
      )
    
    if (any(df_plot$is_highlight)) {
      p <- p %>%
        plotly::add_trace(
          data = df_plot %>% filter(is_highlight),
          x = as.formula(paste0("~`", col_x, "`")),
          y = as.formula(paste0("~`", col_y, "`")),
          mode = "markers",
          type = "scatter",
          marker = list(size = 8, color = "#f59e0b"),
          text = ~player_name,
          name = highlight_player,
          hovertemplate = "<b>%{text}</b><br>" %+% col_x %+% ": %{x}<br>" %+% col_y %+% ": %{y}<extra></extra>",
          showlegend = TRUE
        )
    }
    
    p %>%
      plotly::layout(
        title = list(text = paste(col_y, "vs", col_x)),
        xaxis = list(title = col_x),
        yaxis = list(title = col_y),
        hovermode = "closest"
      )
  })
  
  
  # --- Catapult ---
  output$cat_plot_player_load <- renderPlotly({
    cat_data <- vald_tests_long_ui %>%
      filter(source == "Catapult", metric_name == "Total Player Load") %>%
      group_by(player_id) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      left_join(roster_view %>% dplyr::select(player_id, player_name, if (!is.null(cat_pos_col)) cat_pos_col), by = "player_id") %>%
      arrange(desc(value))
    
    # position filter
    if (!is.null(cat_pos_col) && !is.null(input$cat_pos_filter) && !("All" %in% input$cat_pos_filter)) {
      cat_data <- cat_data %>%
        filter(.data[[cat_pos_col]] %in% input$cat_pos_filter)
    }
    
    if (nrow(cat_data) == 0) {
      return(plotly::plot_ly() %>% plotly::add_text(text = "No data"))
    }
    
    plotly::plot_ly(data = cat_data) %>%
      plotly::add_trace(
        x = ~reorder(player_name, value),
        y = ~value,
        type = "bar",
        marker = list(color = "#3b82f6")
      ) %>%
      plotly::layout(
        title = "Total Player Load",
        xaxis = list(title = "Player"),
        yaxis = list(title = "Load"),
        showlegend = FALSE
      )
  })
  
  output$cat_plot_player_load_min <- renderPlotly({
    cat_data <- vald_tests_long_ui %>%
      filter(source == "Catapult", metric_name == "Player Load Per Minute") %>%
      group_by(player_id) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      left_join(roster_view %>% dplyr::select(player_id, player_name, if (!is.null(cat_pos_col)) cat_pos_col), by = "player_id") %>%
      arrange(desc(value))
    
    # position filter
    if (!is.null(cat_pos_col) && !is.null(input$cat_pos_filter) && !("All" %in% input$cat_pos_filter)) {
      cat_data <- cat_data %>%
        filter(.data[[cat_pos_col]] %in% input$cat_pos_filter)
    }
    
    if (nrow(cat_data) == 0) {
      return(plotly::plot_ly() %>% plotly::add_text(text = "No data"))
    }
    
    plotly::plot_ly(data = cat_data) %>%
      plotly::add_trace(
        x = ~reorder(player_name, value),
        y = ~value,
        type = "bar",
        marker = list(color = "#10b981")
      ) %>%
      plotly::layout(
        title = "Player Load Per Minute",
        xaxis = list(title = "Player"),
        yaxis = list(title = "Load/Min"),
        showlegend = FALSE
      )
  })
  
  output$cat_plot_hsd_12 <- renderPlotly({
    cat_data <- vald_tests_long_ui %>%
      filter(source == "Catapult", str_detect(metric_name, "High Speed Distance")) %>%
      group_by(player_id) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      left_join(roster_view %>% dplyr::select(player_id, player_name, if (!is.null(cat_pos_col)) cat_pos_col), by = "player_id") %>%
      arrange(desc(value))
    
    # position filter
    if (!is.null(cat_pos_col) && !is.null(input$cat_pos_filter) && !("All" %in% input$cat_pos_filter)) {
      cat_data <- cat_data %>%
        filter(.data[[cat_pos_col]] %in% input$cat_pos_filter)
    }
    
    if (nrow(cat_data) == 0) {
      return(plotly::plot_ly() %>% plotly::add_text(text = "No data"))
    }
    
    plotly::plot_ly(data = cat_data) %>%
      plotly::add_trace(
        x = ~reorder(player_name, value),
        y = ~value,
        type = "bar",
        marker = list(color = "#8b5cf6")
      ) %>%
      plotly::layout(
        title = "High Speed Distance (12 mph)",
        xaxis = list(title = "Player"),
        yaxis = list(title = "Distance"),
        showlegend = FALSE
      )
  })
  
  output$cat_plot_sprint_16 <- renderPlotly({
    cat_data <- vald_tests_long_ui %>%
      filter(source == "Catapult", str_detect(metric_name, "Sprint Distance")) %>%
      group_by(player_id) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      left_join(roster_view %>% dplyr::select(player_id, player_name, if (!is.null(cat_pos_col)) cat_pos_col), by = "player_id") %>%
      arrange(desc(value))
    
    # position filter
    if (!is.null(cat_pos_col) && !is.null(input$cat_pos_filter) && !("All" %in% input$cat_pos_filter)) {
      cat_data <- cat_data %>%
        filter(.data[[cat_pos_col]] %in% input$cat_pos_filter)
    }
    
    if (nrow(cat_data) == 0) {
      return(plotly::plot_ly() %>% plotly::add_text(text = "No data"))
    }
    
    plotly::plot_ly(data = cat_data) %>%
      plotly::add_trace(
        x = ~reorder(player_name, value),
        y = ~value,
        type = "bar",
        marker = list(color = "#ec4899")
      ) %>%
      plotly::layout(
        title = "Sprint Distance (16 mph)",
        xaxis = list(title = "Player"),
        yaxis = list(title = "Distance"),
        showlegend = FALSE
      )
  })
  
  output$cat_plot_max_v <- renderPlotly({
    cat_data <- vald_tests_long_ui %>%
      filter(source == "Catapult", metric_name == "Max Vel") %>%
      group_by(player_id) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      left_join(roster_view %>% dplyr::select(player_id, player_name, if (!is.null(cat_pos_col)) cat_pos_col), by = "player_id") %>%
      arrange(desc(value))
    
    # position filter
    if (!is.null(cat_pos_col) && !is.null(input$cat_pos_filter) && !("All" %in% input$cat_pos_filter)) {
      cat_data <- cat_data %>%
        filter(.data[[cat_pos_col]] %in% input$cat_pos_filter)
    }
    
    if (nrow(cat_data) == 0) {
      return(plotly::plot_ly() %>% plotly::add_text(text = "No data"))
    }
    
    plotly::plot_ly(data = cat_data) %>%
      plotly::add_trace(
        x = ~reorder(player_name, value),
        y = ~value,
        type = "bar",
        marker = list(color = "#06b6d4")
      ) %>%
      plotly::layout(
        title = "Max Velocity",
        xaxis = list(title = "Player"),
        yaxis = list(title = "Velocity"),
        showlegend = FALSE
      )
  })
  
  output$cat_plot_explosive <- renderPlotly({
    cat_data <- vald_tests_long_ui %>%
      filter(source == "Catapult", metric_name == "Explosive Efforts") %>%
      group_by(player_id) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      left_join(roster_view %>% dplyr::select(player_id, player_name, if (!is.null(cat_pos_col)) cat_pos_col), by = "player_id") %>%
      arrange(desc(value))
    
    # position filter
    if (!is.null(cat_pos_col) && !is.null(input$cat_pos_filter) && !("All" %in% input$cat_pos_filter)) {
      cat_data <- cat_data %>%
        filter(.data[[cat_pos_col]] %in% input$cat_pos_filter)
    }
    
    if (nrow(cat_data) == 0) {
      return(plotly::plot_ly() %>% plotly::add_text(text = "No data"))
    }
    
    plotly::plot_ly(data = cat_data) %>%
      plotly::add_trace(
        x = ~reorder(player_name, value),
        y = ~value,
        type = "bar",
        marker = list(color = "#f97316")
      ) %>%
      plotly::layout(
        title = "Explosive Efforts",
        xaxis = list(title = "Player"),
        yaxis = list(title = "Efforts"),
        showlegend = FALSE
      )
  })
}


# Run the Shiny app
shinyApp(ui, server)
