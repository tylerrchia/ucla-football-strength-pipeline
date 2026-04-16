# app.R
# -----
# Shiny UI for VALD roster explorer + player card + correlations
# Uses objects produced by source("metrics_automated.r")

library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(plotly)

source("metrics_automated.r")

# ---------------------------
# Helpers
# ---------------------------

player_headshot_slug <- function(player_name) {
  s <- tolower(stringr::str_trim(player_name))
  s <- stringr::str_replace_all(s, "['']", "")
  s <- stringr::str_replace_all(s, "-", "")
  s <- stringr::str_replace_all(s, "[^a-z\\s]", "")
  s <- stringr::str_squish(s)
  parts <- unlist(strsplit(s, "\\s+"))
  if (length(parts) == 0) return(NA_character_)
  if (length(parts) == 1) return(parts[1])
  last  <- parts[length(parts)]
  first <- paste0(parts[-length(parts)], collapse = "")
  paste0(last, "_", first)
}

find_player_headshot <- function(player_name, exts = c("jpg","jpeg","png","webp")) {
  slug <- player_headshot_slug(player_name)
  if (is.na(slug) || !nzchar(slug)) return(NA_character_)
  for (ext in exts) {
    fn <- paste0(slug, ".", ext)
    if (file.exists(file.path("www", "HEADSHOTS", fn))) {
      return(paste0("HEADSHOTS/", fn))
    }
  }
  NA_character_
}

player_photo_slug <- function(player_name) {
  s <- tolower(stringr::str_trim(player_name))
  s <- stringr::str_replace_all(s, "['']", "")
  s <- stringr::str_replace_all(s, "-", "")
  s <- stringr::str_replace_all(s, "[^a-z\\s]", "")
  s <- stringr::str_squish(s)
  parts <- unlist(strsplit(s, "\\s+"))
  if (length(parts) == 0) return(NA_character_)
  if (length(parts) == 1) return(parts[1])
  last  <- parts[length(parts)]
  first <- paste0(parts[-length(parts)], collapse = "")
  paste0(first, "_", last)
}

find_player_photo <- function(player_name, view = c("front","side","back"),
                              exts = c("jpg","jpeg","png","webp","heif","heic"),
                              base_url_prefix = "pics") {
  view <- match.arg(view)
  slug <- player_photo_slug(player_name)
  for (ext in exts) {
    fn        <- sprintf("%s_%s.%s", slug, view, ext)
    disk_path <- file.path("prepics", fn)
    disk_path_www <- file.path("www", "prepics", fn)
    if (file.exists(disk_path))     return(file.path(base_url_prefix, fn))
    if (file.exists(disk_path_www)) return(file.path("prepics", fn))
  }
  NA_character_
}

render_plotly_radar <- function(df_long, selected_player, title = NULL, subtitle_empty = "No data available") {
  axis_levels <- df_long$axis %>% unique()
  axis_levels <- axis_levels[!is.na(axis_levels) & nzchar(axis_levels)]
  df_long <- df_long %>%
    mutate(axis = stringr::str_replace(axis, "\\s*\\b(in\\s+Inches)\\b\\s*$", "")) %>%
    mutate(axis = stringr::str_squish(axis))
  axis_levels <- df_long$axis %>% unique()
  axis_levels <- axis_levels[!is.na(axis_levels) & nzchar(axis_levels)]
  if (length(axis_levels) == 0) axis_levels <- c("—")
  df_long <- df_long %>%
    tidyr::complete(player_name, axis = axis_levels, fill = list(percentile = NA_real_))
  make_closed <- function(d) {
    d <- d %>% arrange(match(axis, axis_levels))
    bind_rows(d, d %>% dplyr::slice_head(n = 1))
  }
  p <- plotly::plot_ly()
  added_any <- FALSE
  for (pn in unique(df_long$player_name)) {
    d     <- df_long %>% filter(player_name == pn) %>% make_closed()
    if (all(is.na(d$percentile))) next
    is_sel <- identical(pn, selected_player)
    p <- p %>%
      plotly::add_trace(
        data = d, type = "scatterpolar", mode = "lines+markers",
        r = ~percentile, theta = ~axis, name = pn, text = ~pn,
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
  if (!added_any) {
    p <- p %>%
      plotly::add_trace(
        type = "scatterpolar", r = rep(0, length(axis_levels)), theta = axis_levels,
        mode = "lines", line = list(color = "rgba(0,0,0,0)"),
        hoverinfo = "skip", showlegend = FALSE
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

vald_tests_long_ui <- vald_tests_long_ui %>%
  filter(!str_detect(str_to_lower(test_type), "iso\\s*prone"))
keep_roster_metrics <- keep_roster_metrics[!str_detect(str_to_lower(keep_roster_metrics), "\\|iso\\s*prone\\|")]

# ---------------------------
# Athleticism score key
# ---------------------------
ATH_KEY   <- "Composite|Score|Athleticism Score"
HAS_ATH   <- ATH_KEY %in% names(roster_view) || ATH_KEY %in% keep_roster_metrics
ATH_LABEL <- "Athleticism Score"

# ---------------------------
# ACWR key & color helper
# ---------------------------
ACWR_KEY   <- "Catapult|Catapult|ACWR"
HAS_ACWR   <- ACWR_KEY %in% names(roster_view)

acwr_zone_color <- function(val) {
  dplyr::case_when(
    is.na(val)    ~ NA_character_,
    val < 0.80    ~ "#FFF3B0",   # Undertrained  – yellow
    val <= 1.30   ~ "#D9F7BE",   # Sweet Spot    – green
    val <= 1.50   ~ "#FFD6A5",   # Warning Zone  – orange
    TRUE          ~ "#FFB8B8"    # Danger Zone   – red
  )
}

acwr_zone_label <- function(val) {
  dplyr::case_when(
    is.na(val)  ~ "—",
    val < 0.80  ~ "Undertrained",
    val <= 1.30 ~ "Sweet Spot",
    val <= 1.50 ~ "Warning Zone",
    TRUE        ~ "Danger Zone"
  )
}

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
  tags$table(
    class = "display",
    tags$thead(
      tags$tr(
        lapply(col_names, function(nm) {
          if (nm == "Athleticism Score") {
            tags$th(tags$span(HTML("Athleticism Score&nbsp;ⓘ"), title = ath_tooltip_text))
          } else if (nm == "ACWR") {
            tags$th(tags$span(
              HTML("ACWR&nbsp;ⓘ"),
              title = paste(
                "Acute:Chronic Workload Ratio",
                "Acute = sum of Player Load (last 7 days)",
                "Chronic = avg weekly load (last 28 days)",
                "< 0.80: Undertrained",
                "0.80–1.30: Sweet Spot",
                "1.31–1.50: Warning Zone",
                "> 1.50: Danger Zone",
                sep = "\n"
              )
            ))
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

norm_metric <- function(x) str_trim(gsub("\\s*\\([^\\)]*\\)\\s*$", "", x))

pick_metric_key_by_name <- function(system_name, metric_name_target, df_keys) {
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

metric_name_from_key <- function(mk) {
  parts <- strsplit(mk, "\\|")[[1]]
  if (length(parts) >= 3) parts[3] else mk
}

pick_best_keys_for_metric_names <- function(source_name, metric_names, fill_summary) {
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

force_metric_names <- c(
  "Jump Height (Imp-Mom)", "RSI-modified (Imp-Mom)", "Force at Peak Power",
  "Force at Zero Velocity", "Eccentric Braking Impulse", "Concentric Impulse"
)
nord_metric_names <- c("L Max Force", "R Max Force", "L Max Impulse", "R Max Impulse", "Max Imbalance")

force_metric_keys_map <- pick_best_keys_for_metric_names("ForceDecks", force_metric_names, fill_summary)
nord_metric_keys_map  <- pick_best_keys_for_metric_names("NordBord",   nord_metric_names,  fill_summary)

radar_force_metrics <- unname(force_metric_keys_map[!is.na(force_metric_keys_map)])
radar_nord_metrics  <- unname(nord_metric_keys_map[!is.na(nord_metric_keys_map)])

radar_force_labels <- tibble::tibble(
  metric_key  = unname(force_metric_keys_map),
  radar_label = names(force_metric_keys_map)
) %>% filter(!is.na(metric_key))

radar_nord_labels <- tibble::tibble(
  metric_key  = unname(nord_metric_keys_map),
  radar_label = names(nord_metric_keys_map)
) %>% filter(!is.na(metric_key))

catapult_metric_names <- c(
  "Player Load Per Minute", "Max Vel", "Max Effort Acceleration", "Max Effort Deceleration",
  "Total Player Load", "Explosive Efforts", "High Speed Distance (12 mph)", "Sprint Distance (16 mph)"
)
catapult_metric_keys_map <- pick_best_keys_for_metric_names("Catapult", catapult_metric_names, fill_summary)
radar_catapult_metrics   <- unname(catapult_metric_keys_map[!is.na(catapult_metric_keys_map)])
radar_catapult_labels    <- tibble::tibble(
  metric_key  = unname(catapult_metric_keys_map),
  radar_label = names(catapult_metric_keys_map)
) %>% filter(!is.na(metric_key)) %>%
  mutate(radar_label = if_else(radar_label == "Total Player Load", "Recent Player Load", radar_label))

smartspeed_metric_names    <- c("Best Split Seconds")
smartspeed_metric_keys_map <- pick_best_keys_for_metric_names("SmartSpeed", smartspeed_metric_names, fill_summary)
radar_smartspeed_metrics   <- unname(smartspeed_metric_keys_map[!is.na(smartspeed_metric_keys_map)])
radar_smartspeed_labels    <- tibble::tibble(
  metric_key  = unname(smartspeed_metric_keys_map),
  radar_label = c("Flying 10s")
) %>% filter(!is.na(metric_key))

lift_metric_names    <- c("Vertical Jump", "Squat", "Bench", "Clean")
lift_metric_keys_map <- pick_best_keys_for_metric_names("Lifts", lift_metric_names, fill_summary)
radar_lift_metrics   <- unname(lift_metric_keys_map[!is.na(lift_metric_keys_map)])
radar_lift_labels    <- tibble::tibble(
  metric_key  = unname(lift_metric_keys_map),
  radar_label = names(lift_metric_keys_map)
) %>% filter(!is.na(metric_key))

pick_radar_metrics <- function(fill_summary, metric_lut, n_total = 6) {
  fs <- fill_summary %>%
    inner_join(metric_lut, by = "metric_key") %>%
    arrange(desc(fill_frac))
  key_pat   <- "CMJ|JUMP|RSI|SPRINT|SPLIT|PEAK POWER|POWER|IMPULSE|FORCE|TORQUE|RFD|NORD|HAMSTRING|ECC"
  preferred <- fs %>% filter(str_detect(str_to_upper(metric_key), key_pat))
  pool <- bind_rows(preferred, fs) %>% distinct(metric_key, .keep_all = TRUE)
  out  <- pool %>%
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

pretty_axis       <- function(mk) {
  parts <- strsplit(mk, "\\|")[[1]]
  if (length(parts) >= 3) paste0(parts[2], " — ", paste(parts[3:length(parts)], collapse=" | ")) else mk
}
pretty_metric_key <- function(mk) {
  parts <- strsplit(mk, "\\|")[[1]]
  if (length(parts) >= 3) paste0(parts[2], " — ", paste(parts[3:length(parts)], collapse = " | ")) else mk
}
metric_system <- function(mk) strsplit(mk, "\\|")[[1]][1]
metric_subcat <- function(mk) {
  parts <- strsplit(mk, "\\|")[[1]]
  if (length(parts) >= 2) parts[2] else NA_character_
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

get_player_pct <- function(player_id, metric_key) {
  x <- roster_percentiles_long %>%
    filter(player_id == !!player_id, metric_key == !!metric_key) %>%
    dplyr::slice_head(n = 1)
  if (nrow(x) == 0) return(NA_real_)
  x$percentile[1]
}

default_metric_for_slider <- {
  candidates <- keep_roster_metrics
  pick <- candidates[str_detect(toupper(candidates), "CMJ|JUMP|10Y|10 Y|SPLIT|SPRINT|RSI|IMPULSE")]
  if (length(pick) == 0) candidates[1] else pick[1]
}

group_col <- if ("pos_group" %in% names(roster_view)) "pos_group" else if ("Group" %in% names(roster_view)) "Group" else NULL
pos_col   <- if ("pos_position" %in% names(roster_view)) "pos_position" else if ("Position" %in% names(roster_view)) "Position" else NULL

cat_pos_col <- if (!is.null(pos_col)) {
  pos_col
} else if ("pos_position" %in% names(roster_view)) {
  "pos_position"
} else if ("Position" %in% names(roster_view)) {
  "Position"
} else {
  NULL
}

class_col   <- if ("class_year_base" %in% names(roster_view)) {
  "class_year_base"
} else if ("class_year" %in% names(roster_view)) {
  "class_year"
} else {
  NULL
}

headshot_col <- if ("headshot" %in% names(roster_view)) "headshot" else NULL

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
    tags$link(rel="icon", type="image/x-icon", href="favicon_v2.ico"),
    tags$style(HTML("
    :-webkit-full-screen .plotly { height: 100vh !important; }
    :fullscreen .plotly { height: 100vh !important; }
  "))
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
        selectInput("pctl_system", "System",
                    choices  = c("All", unique(metric_lut$system)),
                    selected = "All"),
        selectInput("pctl_test", "Test Type", choices = NULL),
        selectInput("pctl_metric", "Metric",  choices = NULL, multiple = TRUE),
        tags$hr(),
        helpText("Click a row to open the Player Card."),
        helpText(
          style = "font-size:11px; color:#6b7280; margin-top:4px;",
          HTML("Values shown are each player's <b>season best</b>, except
               <b>Athlete Standing Weight</b> (most recent),
               <b>Recent Player Load</b> (most recent session), and
               <b>ACWR</b> (last 7 vs 28 days).")
        ),
        width = 3
      ),
      mainPanel(DTOutput("roster_table"), width = 9)
    )
  ),

  # ============== Page 2: Player Card ==============
  tabPanel(
    "Player Card",
    fluidPage(
      uiOutput("player_banner"),
      fluidRow(
        column(4,
               selectInput("player_pick", "Select player",
                           choices  = sort(unique(roster_view$player_name)),
                           selected = sort(unique(roster_view$player_name))[1]))
      ),
      tags$hr(),
      fluidRow(
        column(
          width = 4,
          h4("ForceDecks Radar"),  plotlyOutput("radar_force_plot",    height = 280),
          h4("NordBord Radar"),    plotlyOutput("radar_nord_plot",      height = 280),
          h4("Catapult Radar"),    plotlyOutput("radar_catapult_plot",  height = 280),
          h4("Quick tags"),        uiOutput("player_tags")
        ),
        column(
          width = 8,
          tabsetPanel(
            tabPanel(
              "Positional Comparison",
              fluidRow(
                column(3,
                       radioButtons("poscomp_scope", "Compare within",
                                    choices  = c("Position" = "pos", "Group" = "group"),
                                    selected = "pos", inline = TRUE),
                       checkboxInput("poscomp_show_radar", "Show overlay radar", value = TRUE)),
                column(9,
                       tags$p(style = "font-size:12px; color:#6b7280; margin-bottom:6px;",
                              HTML("Values shown are each player's <b>career best</b>, except
                                   <b>Athlete Standing Weight</b>, <b>Recent Player Load</b>,
                                   <b>Player Load Per Minute</b>, <b>High Speed Distance</b>,
                                   <b>Sprint Distance</b>, and <b>Explosive Efforts</b>
                                   (most recent session).")),
                       DTOutput("poscomp_table"),
                       plotlyOutput("poscomp_radar", height = 420))
              )
            ),
            tabPanel(
              "Trends",
              fluidRow(
                column(3,
                       selectInput("trend_source", "Source",  choices = NULL, selected = "All"),
                       selectInput("trend_test",   "Test",    choices = NULL, selected = "All"),
                       selectInput("trend_metric", "Metric",  choices = NULL, selected = "All"),
                       selectInput("trend_overlay_player", "Overlay another player (optional)",
                                   choices = NULL, selected = "None"),
                       checkboxInput("overlay_pos_group", "Overlay position group (all players)", value = FALSE),
                       checkboxInput("overlay_pos_avg",   "Overlay position average",             value = TRUE)),
                column(9, plotlyOutput("trend_plot", height = 360))
              )
            ),
            tabPanel(
              "Player Comparisons",
              fluidRow(
                column(4,
                       selectInput("compare_player", "Compare player",
                                   choices  = sort(unique(roster_view$player_name)),
                                   selected = sort(unique(roster_view$player_name))[1])),
                column(8,
                       h4("ForceDecks Radar"),  plotlyOutput("compare_force_radar_plot",    height = 280),
                       tags$hr(),
                       h4("NordBord Radar"),    plotlyOutput("compare_nord_radar_plot",      height = 280),
                       tags$hr(),
                       h4("Catapult Radar"),    plotlyOutput("compare_catapult_radar_plot",  height = 280))
              )
            ),
            tabPanel(
              "Performance",
              fluidRow(
                column(3,
                       selectInput("perf_source", "Source",    choices = NULL, selected = "All"),
                       selectInput("perf_test",   "Test Type", choices = NULL, selected = "All"),
                       selectInput("perf_metric", "Metric",    choices = NULL, selected = "All"),
                       dateRangeInput("perf_dates", "Date range",
                                      start = Sys.Date() - 90, end = Sys.Date())),
                column(9, DTOutput("player_perf_table"))
              )
            )
          )
        )
      )
    )
  ),

  # ============== Catapult Tab ==============
  tabPanel(
    "Catapult",
    sidebarLayout(
      sidebarPanel(
        dateRangeInput(
          "cat_dates", "Date range",
          start = min(vald_tests_long_ui$date[vald_tests_long_ui$source == "Catapult"], na.rm = TRUE),
          end   = max(vald_tests_long_ui$date[vald_tests_long_ui$source == "Catapult"], na.rm = TRUE)
        ),
        if (!is.null(cat_pos_col)) {
          selectInput("cat_pos_filter", "Position",
                      choices  = c("All", sort(unique(na.omit(roster_view[[cat_pos_col]])))),
                      selected = "All", multiple = TRUE)
        },
        helpText("Charts show latest Catapult value per player within the date range."),
        tags$hr(),
        helpText(
          style = "font-size:11px; color:#6b7280;",
          HTML("<b>ACWR Zone Key (last 7 vs 28 days):</b><br>",
               "<span style='background:#FFF3B0;padding:1px 6px;border-radius:3px;'>",
               "&lt; 0.80 — Undertrained</span><br>",
               "<span style='background:#D9F7BE;padding:1px 6px;border-radius:3px;'>",
               "0.80–1.30 — Sweet Spot</span><br>",
               "<span style='background:#FFD6A5;padding:1px 6px;border-radius:3px;'>",
               "1.31–1.50 — Warning Zone</span><br>",
               "<span style='background:#FFB8B8;padding:1px 6px;border-radius:3px;'>",
               "&gt; 1.50 — Danger Zone</span>")
        ),
        width = 3
      ),
      mainPanel(
        # Row 1: ACWR (full width – most important, show first)
        fluidRow(
          column(12,
                 div(style = "position:relative; max-width:900px; margin:0 auto;",
                     tags$button("⛶", 
                                 onclick = "var el=this.parentElement; if(!document.fullscreenElement){el.requestFullscreen();}else{document.exitFullscreen();}",
                                 style = "position:absolute;top:4px;right:4px;z-index:999;background:rgba(255,255,255,0.85);border:1px solid #ccc;border-radius:4px;padding:2px 8px;font-size:16px;cursor:pointer;"
                     ),
                     plotlyOutput("cat_plot_acwr", height = 340)
                 )
          )
        ),
        # Row 2
        fluidRow(
          column(6, plotlyOutput("cat_plot_player_load",     height = 320)),
          column(6, plotlyOutput("cat_plot_player_load_min", height = 320))
        ),
        # Row 3
        fluidRow(
          column(6, plotlyOutput("cat_plot_hsd_12",   height = 320)),
          column(6, plotlyOutput("cat_plot_sprint_16", height = 320))
        ),
        # Row 4
        fluidRow(
          column(6, plotlyOutput("cat_plot_max_v",    height = 320)),
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
        selectInput("corr_x_source", "Source (X)",
                    choices  = c("All", sort(unique(na.omit(vald_tests_long_ui$source)))),
                    selected = "All"),
        selectInput("corr_x_test",   "Test (X)",   choices = "All", selected = "All"),
        selectInput("corr_x_metric", "Metric X",   choices = character(0)),
        tags$hr(),
        tags$h4("Metric Y"),
        selectInput("corr_y_source", "Source (Y)",
                    choices  = c("All", sort(unique(na.omit(vald_tests_long_ui$source)))),
                    selected = "All"),
        selectInput("corr_y_test", "Test (Y)", choices = "All", selected = "All"),
        fluidRow(
          column(8, selectInput("corr_y_metric", "Metric Y (choose 1+)",
                                choices = character(0), multiple = TRUE)),
          column(4, actionButton("corr_select_all_y", "Select all"))
        ),
        selectInput("corr_plot_y", "Scatterplot Y", choices = character(0)),
        selectInput("corr_highlight_player", "Highlight player",
                    choices  = c("None", sort(unique(roster_view$player_name))),
                    selected = "None"),
        tags$hr(),
        actionButton("run_corr", "Run"),
        width = 3
      ),
      mainPanel(
        tags$p(style = "font-size:12px; color:#6b7280; margin-bottom:6px;",
               HTML("Correlations use each player's <b>most recent value</b> for each metric.")),
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
    s <- stringr::str_replace(s, "\\.0$", "")
    s
  }

  roster_allowed_metric_keys <- reactive({
    mk_sys  <- input$pctl_system
    mk_test <- input$pctl_test
    mk_met  <- input$pctl_metric
    metric_show <- keep_roster_metrics
    if (!is.null(mk_sys)  && mk_sys  != "All") metric_show <- metric_show[startsWith(metric_show, paste0(mk_sys, "|"))]
    if (!is.null(mk_test) && mk_test != "All") metric_show <- metric_show[str_detect(metric_show, paste0("\\|", mk_test, "\\|"))]
    if (!is.null(mk_met)  && length(mk_met) > 0 && !("All" %in% mk_met)) metric_show <- intersect(keep_roster_metrics, mk_met)
    metric_show
  })

  make_all_toggle <- function(inputId) {
    prev <- reactiveVal("All")
    observeEvent(input[[inputId]], {
      sel      <- input[[inputId]] %||% character(0)
      prev_sel <- prev() %||% character(0)
      if (length(sel) == 0) { updateSelectInput(session, inputId, selected = "All"); prev("All"); return() }
      has_all      <- "All" %in% sel
      prev_has_all <- "All" %in% prev_sel
      if (has_all && prev_has_all && length(sel) > 1) {
        new_sel <- setdiff(sel, "All"); updateSelectInput(session, inputId, selected = new_sel); prev(new_sel); return()
      }
      if (has_all && !prev_has_all) { updateSelectInput(session, inputId, selected = "All"); prev("All"); return() }
      prev(sel)
    }, ignoreInit = TRUE)
  }

  make_all_toggle("group_filter")
  make_all_toggle("pos_filter")
  make_all_toggle("class_filter")
  make_all_toggle("pctl_metric")
  make_all_toggle("cat_pos_filter")

  observeEvent(input$pctl_system, {
    sub <- metric_lut
    if (!is.null(input$pctl_system) && input$pctl_system != "All") sub <- sub %>% filter(system == input$pctl_system)
    tests <- sub %>% pull(test_type) %>% unique() %>% sort()
    updateSelectInput(session, "pctl_test", choices = c("All", tests), selected = "All")
  }, ignoreInit = FALSE)

  observeEvent(list(input$pctl_system, input$pctl_test), {
    sub <- metric_lut
    if (!is.null(input$pctl_system) && input$pctl_system != "All") sub <- sub %>% filter(system == input$pctl_system)
    if (!is.null(input$pctl_test)   && input$pctl_test   != "All") sub <- sub %>% filter(test_type == input$pctl_test)
    metric_choices <- c("All" = "All", setNames(sub$metric_key, sub$label))
    updateSelectInput(session, "pctl_metric", choices = metric_choices, selected = "All")
  }, ignoreInit = FALSE)

  roster_filtered <- reactive({
    df <- roster_view
    if (!is.null(group_col) && !is.null(input$group_filter) && length(input$group_filter) > 0 && !("All" %in% input$group_filter))
      df <- df[df[[group_col]] %in% input$group_filter, , drop = FALSE]
    if (!is.null(pos_col) && !is.null(input$pos_filter) && length(input$pos_filter) > 0 && !("All" %in% input$pos_filter))
      df <- df[df[[pos_col]] %in% input$pos_filter, , drop = FALSE]
    if (!is.null(class_col) && !is.null(input$class_filter) && length(input$class_filter) > 0 && !("All" %in% input$class_filter))
      df <- df[df[[class_col]] %in% input$class_filter, , drop = FALSE]
    df
  })

  default_roster_metrics <- c(
    "Athleticism Score",
    "Athlete Standing Weight",
    "Jump Height (Imp-Mom)", "RSI-modified (Imp-Mom)", "Force at Peak Power",
    "Force at Zero Velocity", "Eccentric Braking Impulse", "Concentric Impulse",
    "L Max Force", "R Max Force", "Avg Max Force",
    "L Max Impulse", "R Max Impulse", "Max Imbalance", "Impulse Imbalance",
    "Total Distance", "Max Vel", "Max Effort Acceleration", "Max Effort Deceleration",
    "Total Player Load", "Player Load Per Minute", "ACWR",
    "Best Split Seconds",
    "Vertical Jump", "Squat", "Bench", "Clean"
  )

  output$roster_table <- renderDT({
    df <- roster_filtered()

    mk_sys  <- input$pctl_system
    mk_test <- input$pctl_test
    mk_met  <- input$pctl_metric

    metric_show <- keep_roster_metrics[
      sapply(keep_roster_metrics, function(x) {
        any(default_roster_metrics %in% sub("^.*\\|", "", x))
      })
    ]

    if (!is.null(mk_sys)  && mk_sys  != "All") metric_show <- metric_show[startsWith(metric_show, paste0(mk_sys, "|"))]
    if (!is.null(mk_test) && mk_test != "All") metric_show <- metric_show[str_detect(metric_show, paste0("\\|", mk_test, "\\|"))]
    if (!is.null(mk_met)  && length(mk_met) > 0 && !("All" %in% mk_met)) metric_show <- intersect(keep_roster_metrics, mk_met)

    if (HAS_ATH) metric_show <- unique(c(ATH_KEY, metric_show))

    # --- Identify ordering keys ---
    max_vel_key <- keep_roster_metrics[grepl("\\|Max Vel$", keep_roster_metrics)][1]
    flying10_key <- keep_roster_metrics[grepl("\\|Flying 10s\\|Best Split Seconds$", keep_roster_metrics)][1]
    total_load_key <- keep_roster_metrics[grepl("\\|Total Player Load$", keep_roster_metrics)][1]

    # Force Flying 10s after Max Vel
    if (!is.na(max_vel_key) && !is.na(flying10_key)) {
      metric_show <- metric_show[metric_show != flying10_key]
      pos <- match(max_vel_key, metric_show)
      if (!is.na(pos)) metric_show <- append(metric_show, flying10_key, after = pos)
      else             metric_show <- c(metric_show, flying10_key)
    }

    # Force ACWR after Total Player Load
    if (HAS_ACWR && !is.na(total_load_key)) {
      metric_show <- metric_show[metric_show != ACWR_KEY]
      pos <- match(total_load_key, metric_show)
      if (!is.na(pos)) metric_show <- append(metric_show, ACWR_KEY, after = pos)
      else             metric_show <- c(metric_show, ACWR_KEY)
    }

    # ---- FRONT COLUMNS ----
    front <- c("player_name")
    if (!is.null(class_col))  front <- c(front, class_col)
    if (!is.null(pos_col))    front <- c(front, pos_col)
    if (!is.null(group_col))  front <- c(front, group_col)

    if (HAS_ATH) metric_show <- unique(c(ATH_KEY, setdiff(metric_show, ATH_KEY)))

    priority_metrics <- c(
      ATH_KEY,
      keep_roster_metrics[grepl("\\|Athlete Standing Weight$", keep_roster_metrics)][1],
      keep_roster_metrics[grepl("\\|Total Player Load$",       keep_roster_metrics)][1],
      if (HAS_ACWR) ACWR_KEY else NULL
    )
    priority_metrics <- priority_metrics[!is.na(priority_metrics)]
    rest_metrics     <- setdiff(metric_show, priority_metrics)
    cols             <- unique(c(front, priority_metrics, rest_metrics))

    df_disp <- df %>% dplyr::select(dplyr::any_of(cols))

    name_with_headshot <- function(player_name) {
      src <- find_player_headshot(player_name)
      if (is.na(src) || !nzchar(src)) return(player_name)
      sprintf(
        "<div data-order='%s' style='display:flex;align-items:center;gap:8px;'>
         <img src='%s' style='width:32px;height:32px;border-radius:50%%;object-fit:cover;'/>
         <span>%s</span></div>",
        htmltools::htmlEscape(player_name), src, htmltools::htmlEscape(player_name)
      )
    }
    df_disp <- df_disp %>%
      mutate(player_name = vapply(player_name, name_with_headshot, character(1)))

    # ---- CLEAN COLUMN NAMES ----
    disp_names <- vapply(colnames(df_disp), function(x) {
      if (grepl("\\|", x)) sub("^.*\\|", "", x) else x
    }, character(1))
    disp_names <- dplyr::recode(
      disp_names,
      player_name     = "Player Name",
      pos_position    = "Position",
      pos_group       = "Position Group",
      class_year_base = "Class Year",
      class_year      = "Class Year"
    )
    disp_names <- gsub("Jump Height \\(Imp-Mom\\) in Inches", "Jump Height", disp_names)
    disp_names <- gsub("Jump Height \\(Imp-Mom\\)",           "Jump Height", disp_names)
    disp_names <- gsub(" in Inches", "",        disp_names)
    disp_names <- gsub("Total Player Load",    "Recent Player Load", disp_names)
    disp_names <- gsub("^Best Split Seconds$", "Flying 10s",         disp_names)
    # ACWR stays as "ACWR"

    colnames(df_disp) <- disp_names

    container <- make_dt_container_with_ath_tooltip(names(df_disp))

    dt <- DT::datatable(
      df_disp,
      container = container,
      escape    = FALSE,
      rownames  = FALSE,
      selection = "single",
      options   = list(
        pageLength      = 50,
        scrollX         = TRUE,
        searchHighlight = TRUE,
        columnDefs      = list(list(targets = 0, className = "dt-nowrap")),
        order = if ("Athleticism Score" %in% names(df_disp)) {
          list(list(which(names(df_disp) == "Athleticism Score") - 1, "desc"))
        } else {
          list()
        }
      )
    )

    # Athleticism Score colors
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

    # ACWR colors
    if ("ACWR" %in% names(df_disp)) {
      dt <- dt %>%
        DT::formatRound("ACWR", 2) %>%
        DT::formatStyle(
          "ACWR",
          backgroundColor = DT::styleInterval(
            c(0.80, 1.30, 1.50),
            c("#FFF3B0",   # < 0.80  Undertrained  yellow
              "#D9F7BE",   # 0.80-1.30 Sweet Spot  green
              "#FFD6A5",   # 1.31-1.50 Warning Zone orange
              "#FFB8B8")   # > 1.50  Danger Zone   red
          ),
          fontWeight = "bold"
        )
    }

    dt
  })

  selected_player <- reactiveVal(NULL)

  observeEvent(roster_filtered(), {
    df <- roster_filtered()
    if (nrow(df) == 0) return()
    if (HAS_ATH && ATH_KEY %in% names(df)) df <- df %>% arrange(desc(.data[[ATH_KEY]]), player_name)
    else df <- df %>% arrange(player_name)
    top_nm  <- df$player_name[1] %||% ""
    cur_nm  <- selected_player()
    if (is.null(cur_nm) || !nzchar(cur_nm) || !(cur_nm %in% df$player_name)) selected_player(top_nm)
  }, ignoreInit = FALSE)

  observe({
    df      <- roster_filtered()
    req(nrow(df) > 0)
    choices <- df %>% pull(player_name) %>% unique() %>% sort()
    current <- selected_player()
    selected <- if (!is.null(current) && nzchar(current) && current %in% choices) current else (choices[1] %||% "")
    updateSelectInput(session, "player_pick", choices = choices, selected = selected)
  })

  observeEvent(input$player_pick, {
    nm <- input$player_pick
    if (!is.null(nm) && nzchar(nm)) selected_player(nm)
  }, ignoreInit = TRUE)

  observeEvent(selected_player(), {
    nm      <- selected_player()
    players <- sort(unique(roster_view$player_name))
    pick    <- players[players != nm][1] %||% nm
    updateSelectInput(session, "compare_player", choices = players, selected = pick)
  }, ignoreInit = TRUE)

  observeEvent(input$roster_table_rows_selected, {
    idx <- input$roster_table_rows_selected
    df  <- roster_filtered()
    if (length(idx) == 1 && nrow(df) >= idx) selected_player(df$player_name[idx])
  }, ignoreInit = TRUE)

  player_pid <- reactive({
    nm  <- selected_player()
    pid <- roster_view %>% dplyr::filter(player_name == nm) %>% dplyr::slice_head(n = 1) %>% dplyr::pull(player_id)
    req(length(pid) == 1)
    pid
  })

  poscomp_cohort <- reactive({
    nm   <- selected_player()
    row  <- roster_view %>% filter(player_name == nm) %>% dplyr::slice_head(n = 1)
    req(nrow(row) == 1)
    scope    <- input$poscomp_scope %||% "pos"
    comp_col <- NULL
    if (scope == "pos"   && !is.null(pos_col))   comp_col <- pos_col
    if (scope == "group" && !is.null(group_col))  comp_col <- group_col
    if (is.null(comp_col)) return(roster_view[0, , drop = FALSE])
    key_val <- row[[comp_col]][1]
    req(!is.na(key_val), nzchar(as.character(key_val)))
    roster_view %>% filter(.data[[comp_col]] == key_val)
  })

  active_metric_keys <- reactive({
    sub <- metric_lut
    if (!is.null(input$pctl_system) && input$pctl_system != "All") sub <- sub %>% filter(system == input$pctl_system)
    if (!is.null(input$pctl_test)   && input$pctl_test   != "All") sub <- sub %>% filter(test_type == input$pctl_test)
    keys <- sub$metric_key
    if (!is.null(input$pctl_metric) && length(input$pctl_metric) > 0 && !("All" %in% input$pctl_metric))
      keys <- intersect(keys, input$pctl_metric)
    unique(keys)
  })

  perf_filtered_df <- reactive({
    df   <- vald_tests_long_ui %>% filter(player_id == player_pid())
    keys <- active_metric_keys()
    if (!is.null(keys) && length(keys) > 0) df <- df %>% filter(metric_key %in% keys)
    if (!is.null(input$perf_source) && input$perf_source != "All") df <- df %>% filter(source    == input$perf_source)
    if (!is.null(input$perf_test)   && input$perf_test   != "All") df <- df %>% filter(test_type == input$perf_test)
    if (!is.null(input$perf_metric) && input$perf_metric != "All") df <- df %>% filter(metric_name == input$perf_metric)
    if (!is.null(input$perf_dates)  && all(!is.na(input$perf_dates)))
      df <- df %>% filter(date >= input$perf_dates[1], date <= input$perf_dates[2])
    df
  })

  trend_filtered_df <- reactive({
    df   <- vald_tests_long_ui %>% filter(player_id == player_pid())
    keys <- active_metric_keys()
    if (!is.null(keys) && length(keys) > 0) df <- df %>% filter(metric_key %in% keys)
    if (!is.null(input$trend_source) && input$trend_source != "All") df <- df %>% filter(source      == input$trend_source)
    if (!is.null(input$trend_test)   && input$trend_test   != "All") df <- df %>% filter(test_type   == input$trend_test)
    if (!is.null(input$trend_metric) && input$trend_metric != "All") df <- df %>% filter(metric_name == input$trend_metric)
    df
  })

  observeEvent(selected_player(), {
    df   <- vald_tests_long_ui %>% filter(player_id == player_pid())
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
    if (!is.null(input$perf_source) && input$perf_source != "All") df <- df %>% filter(source == input$perf_source)
    tests <- sort(unique(na.omit(df$test_type)))
    updateSelectInput(session, "perf_test", choices = c("All", tests), selected = "All")
  }, ignoreInit = FALSE)

  observeEvent(c(input$perf_source, input$perf_test), {
    df <- vald_tests_long_ui %>% filter(player_id == player_pid())
    keys <- active_metric_keys()
    if (!is.null(keys) && length(keys) > 0) df <- df %>% filter(metric_key %in% keys)
    if (!is.null(input$perf_source) && input$perf_source != "All") df <- df %>% filter(source    == input$perf_source)
    if (!is.null(input$perf_test)   && input$perf_test   != "All") df <- df %>% filter(test_type == input$perf_test)
    metrics <- sort(unique(na.omit(df$metric_name))); metrics <- metrics[nzchar(metrics)]
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
    if (!is.null(input$trend_source) && input$trend_source != "All") df <- df %>% filter(source == input$trend_source)
    tests <- sort(unique(na.omit(df$test_type)))
    updateSelectInput(session, "trend_test", choices = c("All", tests), selected = "All")
  }, ignoreInit = FALSE)

  observeEvent(c(input$trend_source, input$trend_test), {
    df <- vald_tests_long_ui %>% filter(player_id == player_pid())
    keys <- active_metric_keys()
    if (!is.null(keys) && length(keys) > 0) df <- df %>% filter(metric_key %in% keys)
    if (!is.null(input$trend_source) && input$trend_source != "All") df <- df %>% filter(source    == input$trend_source)
    if (!is.null(input$trend_test)   && input$trend_test   != "All") df <- df %>% filter(test_type == input$trend_test)
    metrics <- sort(unique(na.omit(df$metric_name))); metrics <- metrics[nzchar(metrics)]
    updateSelectInput(session, "trend_metric", choices = c("All", metrics), selected = "All")
  }, ignoreInit = FALSE)

  observe({
    players <- sort(unique(roster_view$player_name))
    updateSelectInput(session, "trend_overlay_player", choices = c("None", players), selected = "None")
  })

  # ---------- Player banner ----------
  output$player_banner <- renderUI({
    nm  <- selected_player()
    row <- roster_view %>% filter(player_name == nm) %>% dplyr::slice_head(n = 1)
    if (nrow(row) == 0) return(NULL)
    group_val <- if (!is.null(group_col)) row[[group_col]][1] else NA
    pos_val   <- if (!is.null(pos_col))   row[[pos_col]][1]   else NA
    class_val <- if (!is.null(class_col)) row[[class_col]][1] else NA
    headshot  <- find_player_headshot(nm)

    ht_val   <- if ("height_display"   %in% names(row)) fmt_measure(row$height_display)   else "—"
    wt_val   <- if ("weight_display"   %in% names(row)) fmt_measure(row$weight_display)   else "—"
    wing_val <- if ("wingspan_display" %in% names(row)) fmt_measure(row$wingspan_display) else "—"
    hand_val <- if ("hand_display"     %in% names(row)) fmt_measure(row$hand_display)     else "—"
    arm_val  <- if ("arm_display"      %in% names(row)) fmt_measure(row$arm_display)      else "—"

    pid_val <- row$player_id[1]
    lift_df <- vald_tests_long_ui %>%
      filter(player_id == pid_val, source == "Lifts") %>%
      group_by(metric_name) %>%
      summarise(max_val = max(as.numeric(metric_value), na.rm = TRUE), .groups = "drop")

    get_lift <- function(lift_nm) {
      v <- lift_df %>% filter(metric_name == lift_nm) %>% pull(max_val)
      if (length(v) == 0 || all(!is.finite(v))) "—" else as.character(round(v[1]))
    }
    vj_val <- get_lift("Vertical Jump")
    sq_val <- get_lift("Squat")
    bn_val <- get_lift("Bench")
    cl_val <- get_lift("Clean")

    photos <- list(
      front = find_player_photo(nm, "front"),
      side  = find_player_photo(nm, "side"),
      back  = find_player_photo(nm, "back")
    )

    photo_thumb_click <- function(src, view_label, input_id) {
      box_style <- paste("width:90px;height:90px;border-radius:8px;border:1px solid #e5e7eb;",
                         "background:#f3f4f6;object-fit:cover;object-position:50% 22%;",
                         "display:block;cursor:pointer;")
      if (!is.na(src) && nzchar(src)) {
        tags$img(src = src, title = paste("Click to enlarge —", view_label), style = box_style,
                 onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'})", input_id))
      } else {
        tags$div(title = paste0("Missing: ", view_label),
                 style = paste0(box_style, "display:flex;align-items:center;justify-content:center;font-size:11px;color:#6b7280;"), "—")
      }
    }

    fluidRow(
      column(12,
             div(style = "padding:12px;border:1px solid #e5e7eb;border-radius:12px;margin-bottom:12px;",
                 fluidRow(
                   column(2, style = "display:flex;align-items:center;justify-content:center;padding-right:6px;",
                          if (!is.na(headshot) && nzchar(headshot)) {
                            tags$div(style = "width:150px;height:170px;display:flex;align-items:center;justify-content:center;background:transparent;border-radius:16px;overflow:hidden;",
                                     tags$img(src = headshot, style = "max-width:100%;max-height:100%;object-fit:contain;display:block;border-radius:14px;filter:drop-shadow(0 4px 10px rgba(0,0,0,0.15));"))
                          } else {
                            tags$div(style = "width:150px;height:170px;border-radius:16px;background:#f3f4f6;display:flex;align-items:center;justify-content:center;font-size:12px;color:#6b7280;", span("No headshot"))
                          }),
                   column(10,
                          h3(nm, style = "border-bottom:4px solid #FFD100;padding-bottom:4px;"),
                          tags$div(style = "color:#374151;",
                                   paste(if (!is.na(group_val)) paste0("Group: ", group_val) else "",
                                         if (!is.na(pos_val))   paste0(" | Pos: ", pos_val)   else "",
                                         if (!is.na(class_val)) paste0(" | Class: ", class_val) else "",
                                         paste0(" | As-of: ", as.character(as_of_date)))),
                          tags$div(style = "color:#374151;margin-top:4px;",
                                   paste0("Ht: ", ht_val, " | Wt: ", wt_val,
                                          " | Wing: ", wing_val, " | Hand: ", hand_val, " | Arm: ", arm_val)),
                          tags$div(style = "color:#374151;margin-top:4px;",
                                   HTML(paste0("<b>VJ:</b> ", vj_val, "\" &nbsp;|&nbsp; ",
                                               "<b>Squat:</b> ", sq_val, " &nbsp;|&nbsp; ",
                                               "<b>Bench:</b> ", bn_val, " &nbsp;|&nbsp; ",
                                               "<b>Clean:</b> ", cl_val))),
                          tags$div(style = "display:flex;flex-direction:row;flex-wrap:nowrap;gap:8px;align-items:center;",
                                   photo_thumb_click(photos$front, "Front", "photo_front_click"),
                                   photo_thumb_click(photos$side,  "Side",  "photo_side_click"),
                                   photo_thumb_click(photos$back,  "Back",  "photo_back_click"))
                   )
                 )
             ))
    )
  })

  show_photo_modal <- function(src, title) {
    if (is.na(src) || !nzchar(src)) return()
    showModal(modalDialog(title = title, size = "l", easyClose = TRUE, footer = modalButton("Close"),
                          tags$div(style = "display:flex;justify-content:center;",
                                   tags$img(src = src, style = "max-width:100%;max-height:75vh;object-fit:contain;border-radius:12px;border:1px solid #e5e7eb;"))))
  }
  observeEvent(input$photo_front_click, { nm <- selected_player(); src <- find_player_photo(nm, "front"); show_photo_modal(src, paste0(nm, " — Front")) })
  observeEvent(input$photo_side_click,  { nm <- selected_player(); src <- find_player_photo(nm, "side");  show_photo_modal(src, paste0(nm, " — Side"))  })
  observeEvent(input$photo_back_click,  { nm <- selected_player(); src <- find_player_photo(nm, "back");  show_photo_modal(src, paste0(nm, " — Back"))  })

  # ---------- Radar plots ----------
  output$radar_force_plot <- renderPlotly({
    nm  <- selected_player()
    pid <- roster_view %>% filter(player_name == nm) %>% dplyr::slice_head(n = 1) %>% pull(player_id)
    req(length(pid) == 1)
    df <- roster_percentiles_long %>%
      filter(player_id == pid, metric_key %in% radar_force_metrics) %>%
      left_join(radar_force_labels, by = "metric_key") %>%
      mutate(axis = radar_label) %>%
      transmute(player_name = nm, axis, percentile)
    render_plotly_radar(df, nm, "ForceDecks (position-group percentiles)")
  })

  output$radar_nord_plot <- renderPlotly({
    nm  <- selected_player()
    pid <- roster_view %>% filter(player_name == nm) %>% dplyr::slice_head(n = 1) %>% pull(player_id)
    req(length(pid) == 1)
    df <- roster_percentiles_long %>%
      filter(player_id == pid, metric_key %in% radar_nord_metrics) %>%
      left_join(radar_nord_labels, by = "metric_key") %>%
      mutate(axis = radar_label) %>%
      transmute(player_name = nm, axis, percentile)
    render_plotly_radar(df, nm, "NordBord (position-group percentiles)")
  })

  output$radar_catapult_plot <- renderPlotly({
    nm  <- selected_player()
    pid <- roster_view %>% filter(player_name == nm) %>% dplyr::slice_head(n = 1) %>% pull(player_id)
    req(length(pid) == 1)
    df <- roster_percentiles_long %>%
      filter(player_id == pid, metric_key %in% radar_catapult_metrics) %>%
      left_join(radar_catapult_labels, by = "metric_key") %>%
      mutate(axis = radar_label) %>%
      transmute(player_name = nm, axis, percentile)
    render_plotly_radar(df, nm, "Catapult (position-group percentiles)")
  })

  # ---------- Quick tags ----------
  output$player_tags <- renderUI({
    nm  <- selected_player()
    pid <- roster_view %>% filter(player_name == nm) %>% dplyr::slice_head(n = 1) %>% pull(player_id)
    if (length(pid) == 0) return(NULL)
    latest_pcts <- roster_percentiles_long %>% filter(player_id == pid, !is.na(percentile))
    if (nrow(latest_pcts) == 0) return(tags$ul(tags$li("No percentile tags yet.")))
    strengths  <- latest_pcts %>% filter(percentile >= 80) %>% arrange(desc(percentile)) %>% slice_head(n = 5) %>%
      mutate(metric_short = sub("^.*\\|", "", metric_key), label = paste0(metric_short, " (", round(percentile, 1), "th)")) %>% pull(label)
    weaknesses <- latest_pcts %>% filter(percentile <= 20) %>% arrange(percentile) %>% slice_head(n = 5) %>%
      mutate(metric_short = sub("^.*\\|", "", metric_key), label = paste0(metric_short, " (", round(percentile, 1), "th)")) %>% pull(label)
    tags$div(
      tags$h5(style = "color:#2774AE;", "Strengths"),
      if (length(strengths)  == 0) tags$ul(tags$li("—")) else tags$ul(lapply(strengths,  tags$li)),
      tags$h5(style = "color:#9B1C1C;", "Weaknesses"),
      if (length(weaknesses) == 0) tags$ul(tags$li("—")) else tags$ul(lapply(weaknesses, tags$li))
    )
  })

  position_group_values <- reactive({
    pid <- player_pid()
    row <- roster_view %>% filter(player_id == pid) %>% dplyr::slice_head(n = 1)
    req(nrow(row) == 1)
    pos_val <- if (!is.null(pos_col)) row[[pos_col]][1] else NA
    req(!is.na(pos_val))
    pos_player_ids <- roster_view %>% filter(.data[[pos_col]] == pos_val) %>% pull(player_id)
    vald_tests_long_ui %>%
      filter(player_id %in% pos_player_ids, date <= as_of_date) %>%
      mutate(val_num = suppressWarnings(as.numeric(metric_value)))
  })

  # ---------- Performance table ----------
  output$player_perf_table <- renderDT({
    pid <- player_pid()
    df  <- perf_filtered_df()
    row <- roster_view %>% filter(player_id == pid) %>% dplyr::slice_head(n = 1)
    pos_val <- if (!is.null(pos_col) && nrow(row) == 1) row[[pos_col]][1] else NA
    pos_player_ids <- if (!is.na(pos_val) && !is.null(pos_col)) {
      roster_view %>% filter(.data[[pos_col]] == pos_val) %>% pull(player_id)
    } else { character(0) }
    pos_vals <- vald_tests_long_ui %>%
      filter(player_id %in% pos_player_ids, date <= as_of_date) %>%
      mutate(val_num = suppressWarnings(as.numeric(metric_value)))
    compute_pct <- function(metric_key_val, session_date, session_val) {
      same_day_vals <- pos_vals %>% filter(metric_key == metric_key_val, date == session_date) %>% pull(val_num)
      same_day_vals <- same_day_vals[is.finite(same_day_vals)]
      if (length(same_day_vals) < 2 || is.na(session_val) || !is.finite(session_val)) return(NA_real_)
      idx <- match(session_val, same_day_vals)
      if (is.na(idx)) return(NA_real_)
      if (is_flip_metric(metric_key_val)) round(pct_rank_100(-same_day_vals)[idx], 1)
      else                                round(pct_rank_100(same_day_vals)[idx],  1)
    }
    df_out <- df %>%
      mutate(Value = suppressWarnings(as.numeric(metric_value)),
             session_pct = mapply(compute_pct, metric_key, date, Value)) %>%
      arrange(desc(date), source, test_type, metric_name) %>%
      transmute(Date = date, Source = source, Test = test_type, Metric = metric_name,
                Value, Units = units, `Position Percentile (same day)` = session_pct)
    coldefs <- list(list(targets = 0, className = "dt-nowrap"))
    val_idx <- match("Value", names(df_out))
    if (!is.na(val_idx)) coldefs <- append(coldefs, list(list(targets = val_idx - 1, render = DT::JS(
      "function(data,type,row,meta){if(data===null||data===undefined||data==='')return'';var num=Number(data);if(type==='display'){if(Number.isInteger(num))return num.toString();return num.toFixed(2).replace(/\\.00$/,'').replace(/0$/,'');}return num;}"))))
    pct_idx <- match("Position Percentile (same day)", names(df_out))
    if (!is.na(pct_idx)) coldefs <- append(coldefs, list(list(targets = pct_idx - 1, render = DT::JS(
      "function(data,type,row,meta){if(data===null||data===undefined||data==='')return'';var num=Number(data);if(type==='display'){if(Number.isInteger(num))return num.toString();return num.toFixed(2).replace(/\\.00$/,'');}return num;}"))))
    DT::datatable(df_out, rownames = FALSE, selection = "none",
                  options = list(pageLength = 25, scrollX = TRUE, columnDefs = coldefs))
  })

  # ---------- Trend plot ----------
  output$trend_plot <- renderPlotly({
    nm  <- selected_player()
    pid <- roster_view %>% filter(player_name == nm) %>% dplyr::slice_head(n = 1) %>% pull(player_id)
    if (length(pid) == 0) return(NULL)
    df_base <- trend_filtered_df()
    if (is.null(input$trend_metric) || input$trend_metric == "All") return(NULL)
    if (nrow(df_base) == 0) return(NULL)
    mk        <- df_base %>% distinct(metric_key) %>% dplyr::slice_head(n = 1) %>% pull(metric_key)
    pretty_mk <- sub("^([^|]*\\|){2}", "", mk)
    df_all    <- vald_tests_long_ui %>% filter(metric_key == mk)
    df_main   <- df_all %>% filter(player_id == pid) %>% arrange(date) %>% mutate(Line = nm)
    if (nrow(df_main) == 0) return(NULL)
    plot_players <- df_main
    overlay_nm   <- input$trend_overlay_player
    if (!is.null(overlay_nm) && overlay_nm != "None" && overlay_nm != nm) {
      pid2 <- roster_view %>% filter(player_name == overlay_nm) %>% dplyr::slice_head(n = 1) %>% pull(player_id)
      if (length(pid2) == 1) {
        df_other <- df_all %>% filter(player_id == pid2) %>% arrange(date) %>% mutate(Line = overlay_nm)
        if (nrow(df_other) > 0) plot_players <- bind_rows(plot_players, df_other)
      }
    }
    pos_avg <- NULL
    if (isTRUE(input$overlay_pos_avg) && !is.null(pos_col)) {
      pos_val <- roster_view %>% filter(player_id == pid) %>% dplyr::slice_head(n = 1) %>% pull(.data[[pos_col]])
      if (!is.na(pos_val)) {
        pos_players <- roster_view %>% filter(.data[[pos_col]] == pos_val) %>% pull(player_id)
        pos_avg <- df_all %>% filter(player_id %in% pos_players) %>%
          group_by(date) %>%
          summarise(metric_value = if (all(is.na(metric_value))) NA_real_ else mean(metric_value, na.rm = TRUE), .groups = "drop") %>%
          filter(is.finite(metric_value)) %>% mutate(Line = paste0(pos_val, " Avg"))
      }
    }
    pos_pts <- NULL
    if (isTRUE(input$overlay_pos_group) && !is.null(pos_col) && pos_col %in% names(roster_view)) {
      pos_val <- roster_view %>% filter(player_id == pid) %>% dplyr::slice_head(n = 1) %>% pull(.data[[pos_col]])
      if (length(pos_val) == 1 && !is.na(pos_val) && nzchar(pos_val)) {
        pos_ids <- roster_view %>% filter(.data[[pos_col]] == pos_val) %>% pull(player_id)
        pos_pts <- df_all %>% filter(player_id %in% pos_ids) %>%
          select(player_id, player_name, date, metric_value) %>% mutate(Group = paste0("Pos: ", pos_val))
      }
    }
    p <- ggplot() +
      geom_line(data = plot_players, aes(x = date, y = metric_value, color = Line, group = Line, text = player_name), linewidth = 1) +
      geom_point(data = plot_players, aes(x = date, y = metric_value, color = Line, text = player_name), size = 2)
    if (!is.null(pos_avg) && nrow(pos_avg) > 0) {
      p <- p + geom_point(data = pos_avg, aes(x = date, y = metric_value, color = Line), size = 2.8)
      if (nrow(pos_avg) >= 2) p <- p + geom_line(data = pos_avg, aes(x = date, y = metric_value, color = Line, group = Line), linewidth = 1, linetype = "dashed", na.rm = TRUE)
    }
    if (!is.null(pos_pts) && nrow(pos_pts) > 0) p <- p + geom_point(data = pos_pts, aes(x = date, y = metric_value, text = player_name), inherit.aes = FALSE, alpha = 0.25, size = 2)
    p <- p + theme_minimal(base_size = 12) + labs(x = "Date", y = "Value", color = NULL, title = paste(nm, "—", pretty_mk)) + theme(legend.position = "bottom")
    ggplotly(p, tooltip = "text")
  })

  # ---------- Positional comparison table ----------
  poscomp_table_df <- reactive({
    cohort <- poscomp_cohort()
    if (nrow(cohort) < 1) return(tibble::tibble())
    weight_key <- keep_roster_metrics[startsWith(keep_roster_metrics, "ForceDecks|") & grepl("\\|Athlete Standing Weight$", keep_roster_metrics)][1]
    if (length(weight_key) == 0) weight_key <- NA_character_
    keys <- unique(c(radar_force_metrics, radar_nord_metrics, radar_catapult_metrics, radar_smartspeed_metrics, radar_lift_metrics, weight_key))
    keys <- keys[!is.na(keys) & nzchar(as.character(keys))]
    if (length(keys) < 1) return(tibble::tibble())
    label_df <- bind_rows(radar_force_labels, radar_nord_labels, radar_catapult_labels, radar_smartspeed_labels, radar_lift_labels) %>%
      distinct(metric_key, radar_label) %>%
      mutate(radar_label = as.character(radar_label), radar_label = if_else(is.na(radar_label), as.character(metric_key), radar_label))
    if (!is.na(weight_key)) label_df <- bind_rows(label_df, tibble::tibble(metric_key = weight_key, radar_label = "Athlete Standing Weight")) %>% distinct(metric_key, .keep_all = TRUE)
    label_df <- label_df %>% group_by(radar_label) %>%
      mutate(radar_label = if_else(n() > 1, paste0(radar_label, " [", row_number(), "]"), radar_label)) %>% ungroup()
    latest_preferred_keys <- keys[grepl("Total Player Load|Player Load Per Minute|High Speed Distance|Sprint Distance|Explosive Efforts", keys, ignore.case = TRUE) & startsWith(keys, "Catapult|")]
    best_keys   <- setdiff(keys, latest_preferred_keys)
    best_vals   <- vald_best_wide %>% filter(player_id %in% cohort$player_id) %>%
      select(player_id, player_name, any_of(best_keys)) %>%
      pivot_longer(cols = any_of(best_keys), names_to = "metric_key", values_to = "raw_value") %>%
      filter(!is.na(raw_value)) %>%
      mutate(player_name = as.character(player_name), metric_key = as.character(metric_key), raw_value = as.numeric(raw_value))
    recent_vals <- vald_tests_long_ui %>%
      filter(player_id %in% cohort$player_id, metric_key %in% latest_preferred_keys, date <= as_of_date) %>%
      group_by(player_id, player_name, metric_key) %>% slice_max(date, n = 1, with_ties = FALSE) %>% ungroup() %>%
      transmute(player_name = as.character(player_name), metric_key = as.character(metric_key), raw_value = suppressWarnings(as.numeric(metric_value))) %>%
      filter(!is.na(raw_value))
    latest_vals <- bind_rows(best_vals, recent_vals)
    if (nrow(latest_vals) == 0) return(tibble::tibble())
    long <- latest_vals %>%
      left_join(label_df, by = "metric_key") %>%
      mutate(radar_label = as.character(radar_label), col = if_else(is.na(radar_label) | !nzchar(radar_label), as.character(metric_key), radar_label)) %>%
      filter(!is.na(col), nzchar(as.character(col)))
    out <- long %>%
      transmute(player_name = as.character(player_name), col = as.character(col), raw_value = as.numeric(raw_value)) %>%
      group_by(player_name, col) %>%
      summarise(raw_value = { v <- raw_value[!is.na(raw_value)]; if (length(v) == 0) NA_real_ else v[1] }, .groups = "drop") %>%
      tidyr::pivot_wider(names_from = col, values_from = raw_value, values_fill = NA_real_)
    if (HAS_ATH && ATH_KEY %in% names(roster_view)) {
      ath_df <- roster_view %>% filter(player_id %in% cohort$player_id) %>%
        select(player_name, !!ATH_KEY) %>% mutate(player_name = as.character(player_name)) %>%
        rename(`Athleticism Score` = !!ATH_KEY)
      out <- out %>% left_join(ath_df, by = "player_name")
    }
    nm         <- selected_player()
    weight_col <- "Athlete Standing Weight"
    out %>%
      rename(`Player Name` = player_name) %>%
      { base <- .; cols <- c("Player Name"); if ("Athleticism Score" %in% names(base)) cols <- c(cols, "Athleticism Score"); if (weight_col %in% names(base)) cols <- c(cols, weight_col); dplyr::select(base, dplyr::any_of(cols), dplyr::everything()) } %>%
      mutate(.sel = (`Player Name` == nm)) %>% arrange(desc(.sel), `Player Name`) %>% select(-.sel)
  })

  output$poscomp_table <- renderDT({
    df_vals <- poscomp_table_df()
    if (!is.data.frame(df_vals) || nrow(df_vals) == 0) {
      return(DT::datatable(data.frame(Message = "No positional comparison data available."), rownames = FALSE, options = list(dom = 't')))
    }
    df_vals <- as.data.frame(df_vals, check.names = FALSE, stringsAsFactors = FALSE)
    id_col  <- "Player Name"
    clean_poscomp_header <- function(nm) {
      if (is.null(nm) || length(nm) == 0) return("")
      nm <- as.character(nm)[1]; if (is.na(nm) || !nzchar(nm)) return("")
      nm <- gsub("Total Player Load", "Recent Player Load", nm)
      nm <- gsub("Jump Height \\(Imp-Mom\\) in Inches", "Jump Height", nm)
      nm <- gsub("Jump Height \\(Imp-Mom\\)", "Jump Height", nm)
      nm <- gsub("RSI-modified \\(Imp-Mom\\)", "RSI-modified", nm)
      nm <- gsub("\\(Imp-Mom\\)", "", nm); nm <- gsub(" in Inches", "", nm)
      trimws(nm)
    }
    new_names <- sapply(names(df_vals), clean_poscomp_header, USE.NAMES = FALSE)
    names(df_vals) <- as.character(new_names)
    dt <- DT::datatable(df_vals, rownames = FALSE, selection = "none",
                        options = list(pageLength = 25, scrollX = TRUE, dom = "t",
                                       columnDefs = list(list(targets = 0, className = "dt-nowrap")),
                                       order = if ("Athleticism Score" %in% names(df_vals)) list(list(which(names(df_vals) == "Athleticism Score") - 1, "desc")) else list()))
    if ("Athleticism Score" %in% names(df_vals)) {
      dt <- dt %>% DT::formatRound("Athleticism Score", 1) %>%
        DT::formatStyle("Athleticism Score",
                        backgroundColor = DT::styleInterval(c(20, 40, 60, 80), c("#ffe5e5","#ffd6a5","#fff3b0","#d9f7be","#b7eb8f")),
                        fontWeight = "bold")
    }
    dt
  })

  output$poscomp_radar <- renderPlotly({
    req(isTRUE(input$poscomp_show_radar))
    cohort <- poscomp_cohort()
    if (nrow(cohort) < 1) return(plotly::plot_ly() %>% plotly::layout(title = "No cohort available for positional comparison"))
    keys <- unique(c(radar_force_metrics, radar_nord_metrics, radar_catapult_metrics, radar_smartspeed_metrics))
    keys <- keys[!is.na(keys) & nzchar(as.character(keys))]
    if (length(keys) < 3) return(plotly::plot_ly() %>% plotly::layout(title = "Not enough radar metrics available to plot"))
    label_df <- bind_rows(radar_force_labels, radar_nord_labels, radar_catapult_labels, radar_smartspeed_labels, radar_lift_labels) %>%
      distinct(metric_key, radar_label) %>% mutate(radar_label = as.character(radar_label))
    long <- roster_percentiles_long %>%
      filter(player_id %in% cohort$player_id, metric_key %in% keys) %>%
      left_join(label_df, by = "metric_key") %>%
      mutate(radar_label = as.character(radar_label), metric_key = as.character(metric_key),
             axis = case_when(!is.na(radar_label) & nzchar(radar_label) ~ radar_label,
                              TRUE ~ tryCatch(as.character(pretty_metric_key(metric_key)), error = function(e) as.character(metric_key))),
             player_name = as.character(player_name), percentile = as.numeric(percentile)) %>%
      filter(!is.na(axis), nzchar(axis), !is.na(player_name), nzchar(player_name))
    if (nrow(long) == 0) return(plotly::plot_ly() %>% plotly::layout(title = "No valid metric data available for radar plot"))
    axis_order <- c("Jump Height (Imp-Mom)","Force at Zero Velocity","Force at Peak Power","Concentric Impulse",
                    "RSI-modified (Imp-Mom)","Eccentric Braking Impulse","L Max Force","R Max Force","L Max Impulse","R Max Impulse",
                    "Max Imbalance","Recent Player Load","Player Load Per Minute","High Speed Distance (12 mph)",
                    "Sprint Distance (16 mph)","Explosive Efforts","Max Effort Acceleration","Max Effort Deceleration","Max Vel","Flying 10s")
    axis_levels <- axis_order[axis_order %in% unique(long$axis)]
    long <- long %>% mutate(axis = factor(axis, levels = axis_levels)) %>%
      tidyr::complete(player_id, player_name, axis = axis_levels, fill = list(percentile = NA_real_)) %>%
      mutate(player_name = as.character(player_name), axis = factor(as.character(axis), levels = axis_levels))
    make_closed <- function(df_one, axis_levels) {
      df_one <- df_one %>% arrange(match(axis, axis_levels))
      bind_rows(df_one, df_one %>% dplyr::slice_head(n = 1))
    }
    nm_sel <- selected_player()
    p <- plotly::plot_ly()
    for (pn in unique(long$player_name)) {
      df_one <- long %>% filter(player_name == pn) %>% make_closed(axis_levels)
      if (all(is.na(df_one$percentile))) next
      is_sel <- identical(pn, nm_sel)
      p <- p %>%
        plotly::add_trace(data = df_one, type = "scatterpolar", mode = "lines+markers",
                          r = ~percentile, theta = ~axis, name = pn, text = ~player_name,
                          hovertemplate = "%{text}<br>%{theta}: %{r:.1f}<extra></extra>",
                          connectgaps = TRUE,
                          line = list(width = if (is_sel) 4 else 1.5, shape = "linear"),
                          fill = if (is_sel) "toself" else "none",
                          fillcolor = if (is_sel) "rgba(39,116,174,0.12)" else NULL,
                          marker = list(size = if (is_sel) 6 else 4),
                          opacity = if (is_sel) 1 else 0.15, showlegend = is_sel)
    }
    p %>% plotly::layout(
      polar = list(radialaxis = list(range = c(0, 100), tickvals = c(0, 25, 50, 75, 100)),
                   angularaxis = list(categoryorder = "array", categoryarray = axis_levels)),
      margin = list(l = 40, r = 40, t = 40, b = 40), showlegend = TRUE)
  })

  # ---------- Player Comparisons ----------
  make_empty_polar <- function(msg) {
    plotly::plot_ly(type = "scatterpolar", r = c(0), theta = c("—"), mode = "lines",
                    line = list(color = "rgba(0,0,0,0)"), showlegend = FALSE) %>%
      plotly::layout(title = list(text = msg),
                     polar = list(radialaxis = list(visible = FALSE), angularaxis = list(visible = FALSE)))
  }

  output$compare_force_radar_plot <- renderPlotly({
    nm2 <- input$compare_player; req(!is.null(nm2), nzchar(nm2))
    pid2 <- roster_view %>% filter(player_name == nm2) %>% dplyr::slice_head(n = 1) %>% pull(player_id); req(length(pid2) == 1)
    key_df <- vald_tests_long_ui %>% distinct(metric_key, source, metric_name, test_type) %>% mutate(system = source)
    force_keys <- vapply(c("Jump Height (Imp-Mom) in Inches","RSI-modified","Force at Peak Power","Force at Zero Velocity","Eccentric Braking Impulse","Concentric Impulse"),
                         function(nm) pick_metric_key_by_name("ForceDecks", nm, key_df), character(1))
    force_keys <- force_keys[!is.na(force_keys)]
    if (length(force_keys) < 3) return(make_empty_polar("Not enough ForceDecks metrics configured"))
    df <- roster_percentiles_long %>% filter(player_id == pid2, metric_key %in% force_keys) %>%
      left_join(key_df %>% distinct(metric_key, metric_name), by = "metric_key") %>%
      mutate(metric_name = as.character(metric_name), metric_name = if_else(is.na(metric_name) | !nzchar(metric_name), "Unknown", metric_name)) %>%
      transmute(player_name = as.character(nm2), axis = as.character(norm_metric(metric_name)), percentile = as.numeric(percentile)) %>%
      filter(!is.na(axis), nzchar(axis), !is.na(percentile))
    if (nrow(df) < 3) return(make_empty_polar("Not enough ForceDecks data available"))
    render_plotly_radar(df, nm2, paste0("ForceDecks — ", nm2, " (position-group percentiles)"))
  })

  output$compare_nord_radar_plot <- renderPlotly({
    nm2 <- input$compare_player; req(!is.null(nm2), nzchar(nm2))
    pid2 <- roster_view %>% filter(player_name == nm2) %>% dplyr::slice_head(n = 1) %>% pull(player_id); req(length(pid2) == 1)
    if (length(radar_nord_metrics) < 3) return(make_empty_polar("Not enough NordBord metrics configured"))
    df <- roster_percentiles_long %>% filter(player_id == pid2, metric_key %in% radar_nord_metrics) %>%
      left_join(radar_nord_labels, by = "metric_key") %>%
      mutate(radar_label = as.character(radar_label), radar_label = if_else(is.na(radar_label) | !nzchar(radar_label), "Unknown", radar_label)) %>%
      transmute(player_name = as.character(nm2), axis = as.character(radar_label), percentile = as.numeric(percentile)) %>%
      filter(!is.na(axis), nzchar(axis), !is.na(percentile))
    if (nrow(df) < 3) return(make_empty_polar("Not enough NordBord data available"))
    render_plotly_radar(df, nm2, paste0("NordBord — ", nm2, " (position-group percentiles)"))
  })

  output$compare_catapult_radar_plot <- renderPlotly({
    nm2 <- input$compare_player; req(!is.null(nm2), nzchar(nm2))
    pid2 <- roster_view %>% filter(player_name == nm2) %>% dplyr::slice_head(n = 1) %>% pull(player_id); req(length(pid2) == 1)
    if (length(radar_catapult_metrics) < 3) return(make_empty_polar("Not enough Catapult metrics configured"))
    df <- roster_percentiles_long %>% filter(player_id == pid2, metric_key %in% radar_catapult_metrics) %>%
      left_join(radar_catapult_labels, by = "metric_key") %>%
      mutate(radar_label = as.character(radar_label), radar_label = if_else(is.na(radar_label) | !nzchar(radar_label), "Unknown", radar_label)) %>%
      transmute(player_name = as.character(nm2), axis = as.character(radar_label), percentile = as.numeric(percentile)) %>%
      filter(!is.na(axis), nzchar(axis), !is.na(percentile))
    if (nrow(df) < 3) return(make_empty_polar("Not enough Catapult data available"))
    render_plotly_radar(df, nm2, paste0("Catapult — ", nm2, " (position-group percentiles)"))
  })

  # ---------------------------
  # Catapult tab helpers
  # ---------------------------

  cat_metric_key_for <- function(df_cat, patterns) {
    for (pat in patterns) {
      hit <- df_cat %>%
        filter(stringr::str_detect(stringr::str_to_lower(metric_name), stringr::str_to_lower(pat))) %>%
        count(metric_key, sort = TRUE) %>% dplyr::slice_head(n = 1) %>% pull(metric_key)
      if (length(hit) == 1) return(hit)
    }
    NA_character_
  }

  catapult_base <- reactive({
    df <- vald_tests_long_ui %>% filter(source == "Catapult")
    if (!is.null(input$cat_dates) && all(!is.na(input$cat_dates)))
      df <- df %>% filter(date >= input$cat_dates[1], date <= input$cat_dates[2])
    if (!is.null(pos_col) && pos_col %in% names(roster_view)) {
      roster_pos <- roster_view %>% distinct(player_id, .data[[pos_col]]) %>% rename(cat_position = .data[[pos_col]])
      df <- df %>% left_join(roster_pos, by = "player_id")
      if (!is.null(input$cat_pos_filter) && length(input$cat_pos_filter) > 0 && !("All" %in% input$cat_pos_filter))
        df <- df %>% filter(cat_position %in% input$cat_pos_filter)
    }
    df
  })

  catapult_latest_per_player <- function(df, metric_key, title, agg_method = "latest") {
    if (is.na(metric_key)) return(plotly::plot_ly() %>% plotly::layout(title = list(text = paste0(title, "<br><sup>Metric not found in data</sup>"))))
    d_base <- df %>% filter(metric_key == !!metric_key) %>% mutate(value = suppressWarnings(as.numeric(metric_value))) %>% filter(!is.na(value))
    if (nrow(d_base) == 0) return(plotly::plot_ly() %>% plotly::layout(title = list(text = paste0(title, "<br><sup>No data in selected range</sup>"))))
    if (agg_method == "sum") {
      d <- d_base %>% group_by(player_id, player_name) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>% filter(value > 0)
      title_final <- paste0("Total ", title)
    } else if (agg_method == "max") {
      d <- d_base %>% group_by(player_id, player_name) %>% summarise(value = max(value, na.rm = TRUE), .groups = "drop") %>% filter(is.finite(value))
      title_final <- title
    } else if (agg_method == "avg") {
      d <- d_base %>% group_by(player_id, player_name) %>% summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>% filter(is.finite(value))
      title_final <- paste0("Avg ", title)
    } else {
      d <- d_base %>% group_by(player_id, player_name) %>% slice_max(date, n = 1, with_ties = FALSE) %>% ungroup()
      title_final <- title
    }
    if (nrow(d) == 0) return(plotly::plot_ly() %>% plotly::layout(title = list(text = paste0(title_final, "<br><sup>No data in selected range</sup>"))))
    d <- d %>% arrange(desc(value)); if (nrow(d) > 30) d <- d %>% slice_head(n = 30)
    plotly::plot_ly(data = d, x = ~reorder(player_name, value), y = ~value, type = "bar",
                    text = ~player_name, hovertemplate = "%{text}<br>%{y}<extra></extra>") %>%
      plotly::layout(title = list(text = title_final),
                     xaxis = list(title = "", tickangle = -45), yaxis = list(title = ""),
                     margin = list(l = 60, r = 20, t = 60, b = 100))
  }

  catapult_keys <- reactive({
    df_cat <- vald_tests_long_ui %>% filter(source == "Catapult") %>% distinct(metric_key, metric_name)
    list(
      player_load     = cat_metric_key_for(df_cat, c("^total player load$", "player load$")),
      player_load_min = cat_metric_key_for(df_cat, c("player load.*min", "player load/min", "player load per min")),
      hsd_12          = cat_metric_key_for(df_cat, c("high speed.*12", "high speed.*12 mph", "high speed dist")),
      sprint_16       = cat_metric_key_for(df_cat, c("sprint.*16", "sprint.*16 mph", "sprint dist")),
      max_v           = cat_metric_key_for(df_cat, c("^max vel$", "max v", "max velocity")),
      explosive       = cat_metric_key_for(df_cat, c("explosive efforts", "explosive effort"))
    )
  })

  # ---- ACWR bar chart (Catapult tab) ----
  output$cat_plot_acwr <- renderPlotly({
    # filter to players visible under current position filter
    df_base <- catapult_base()
    date_end   <- if (!is.null(input$cat_dates) && !is.na(input$cat_dates[2])) input$cat_dates[2] else as_of_date
    date_start <- date_end - 27          # full 28-day window

    # raw Player Load rows within 28-day window
    pl_raw <- df_base %>%
      filter(
        str_detect(str_to_lower(metric_name), "^total player load$|^player load$"),
        date >= date_start, date <= date_end
      ) %>%
      mutate(value = suppressWarnings(as.numeric(metric_value))) %>%
      filter(!is.na(value), is.finite(value))

    if (nrow(pl_raw) == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = list(text = "Acute:Chronic Workload Ratio<br><sup>No Catapult Player Load data in selected range</sup>")))
    }

    acute_start <- date_end - 6   # last 7 days

    acwr_chart <- pl_raw %>%
      group_by(player_id, player_name) %>%
      summarise(
        acute   = sum(value[date >= acute_start], na.rm = TRUE),
        chronic = sum(value, na.rm = TRUE) / 4,
        .groups = "drop"
      ) %>%
      mutate(
        acwr  = if_else(chronic > 0, round(acute / chronic, 2), NA_real_),
        zone  = acwr_zone_label(acwr),
        color = acwr_zone_color(acwr)
      ) %>%
      filter(!is.na(acwr)) %>%
      arrange(desc(acwr))

    if (nrow(acwr_chart) == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = list(text = "Acute:Chronic Workload Ratio<br><sup>Insufficient data for ACWR calculation</sup>")))
    }

    # Build reference lines data
    zone_lines <- list(
      list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 0.80, y1 = 0.80,
           line = list(color = "#FADB14", dash = "dot", width = 1.5)),
      list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 1.30, y1 = 1.30,
           line = list(color = "#FA8C16", dash = "dot", width = 1.5)),
      list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 1.50, y1 = 1.50,
           line = list(color = "#FF4D4F", dash = "dot", width = 1.5))
    )

    plotly::plot_ly(
      data = acwr_chart,
      x    = ~reorder(player_name, acwr),
      y    = ~acwr,
      type = "bar",
      marker = list(color = ~color, line = list(color = "rgba(0,0,0,0.15)", width = 0.5)),
      text = ~paste0(player_name, "<br>ACWR: ", acwr, "<br>Zone: ", zone,
                     "<br>Acute (7d): ", round(acute, 0),
                     "<br>Chronic (28d avg wk): ", round(chronic, 0)),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      plotly::layout(
        title  = list(text = "Acute:Chronic Workload Ratio (Player Load, 7-day vs 28-day avg)"),
        xaxis  = list(title = "", tickangle = -45),
        yaxis  = list(title = "ACWR", range = c(0, max(c(acwr_chart$acwr * 1.15, 1.6), na.rm = TRUE))),
        shapes = zone_lines,
        annotations = list(
          list(x = 1.01, xref = "paper", y = 0.80, text = "0.80", showarrow = FALSE,
               xanchor = "left", font = list(size = 10, color = "#FADB14")),
          list(x = 1.01, xref = "paper", y = 1.30, text = "1.30", showarrow = FALSE,
               xanchor = "left", font = list(size = 10, color = "#FA8C16")),
          list(x = 1.01, xref = "paper", y = 1.50, text = "1.50", showarrow = FALSE,
               xanchor = "left", font = list(size = 10, color = "#FF4D4F"))
        ),
        margin = list(l = 60, r = 60, t = 60, b = 120)
      )
  })

  output$cat_plot_player_load     <- renderPlotly({ keys <- catapult_keys(); catapult_latest_per_player(catapult_base(), keys$player_load,     "Player Load",                "sum") })
  output$cat_plot_player_load_min <- renderPlotly({ keys <- catapult_keys(); catapult_latest_per_player(catapult_base(), keys$player_load_min, "Player Load / Min",          "avg") })
  output$cat_plot_hsd_12          <- renderPlotly({ keys <- catapult_keys(); catapult_latest_per_player(catapult_base(), keys$hsd_12,          "High Speed Distance (12 mph)","sum") })
  output$cat_plot_sprint_16       <- renderPlotly({ keys <- catapult_keys(); catapult_latest_per_player(catapult_base(), keys$sprint_16,       "Sprint Distance (16 mph)",   "sum") })
  output$cat_plot_max_v           <- renderPlotly({ keys <- catapult_keys(); catapult_latest_per_player(catapult_base(), keys$max_v,           "Max Velocity",               "max") })
  output$cat_plot_explosive       <- renderPlotly({ keys <- catapult_keys(); catapult_latest_per_player(catapult_base(), keys$explosive,       "Explosive Efforts",          "sum") })

  # ---------------------------
  # Correlations
  # ---------------------------

  corr_base_df <- reactive({
    df <- vald_tests_long_ui
    allow_keys <- roster_allowed_metric_keys()
    if (!is.null(allow_keys) && length(allow_keys) > 0) df <- df %>% filter(metric_key %in% allow_keys)
    df %>% filter(date <= as_of_date)
  })

  observeEvent(input$corr_x_source, {
    df <- corr_base_df()
    if (!is.null(input$corr_x_source) && input$corr_x_source != "All") df <- df %>% filter(source == input$corr_x_source)
    tests <- sort(unique(na.omit(df$test_type)))
    updateSelectInput(session, "corr_x_test",   choices = c("All", tests), selected = "All")
    updateSelectInput(session, "corr_x_metric", choices = character(0),    selected = character(0))
  }, ignoreInit = FALSE)

  observeEvent(input$corr_y_source, {
    df <- corr_base_df()
    if (!is.null(input$corr_y_source) && input$corr_y_source != "All") df <- df %>% filter(source == input$corr_y_source)
    tests <- sort(unique(na.omit(df$test_type)))
    updateSelectInput(session, "corr_y_test",   choices = c("All", tests), selected = "All")
    updateSelectInput(session, "corr_y_metric", choices = character(0),    selected = character(0))
    updateSelectInput(session, "corr_plot_y",   choices = character(0),    selected = character(0))
  }, ignoreInit = FALSE)

  observeEvent(list(input$corr_x_source, input$corr_x_test, input$pctl_system, input$pctl_test, input$pctl_metric), {
    df <- corr_base_df()
    if (!is.null(input$corr_x_source) && input$corr_x_source != "All") df <- df %>% filter(source    == input$corr_x_source)
    if (!is.null(input$corr_x_test)   && input$corr_x_test   != "All") df <- df %>% filter(test_type == input$corr_x_test)
    metrics <- sort(unique(na.omit(df$metric_name))); metrics <- metrics[nzchar(metrics)]
    updateSelectInput(session, "corr_x_metric", choices = metrics, selected = metrics[1] %||% "")
  }, ignoreInit = FALSE)

  observeEvent(list(input$corr_y_source, input$corr_y_test, input$pctl_system, input$pctl_test, input$pctl_metric), {
    df <- corr_base_df()
    if (!is.null(input$corr_y_source) && input$corr_y_source != "All") df <- df %>% filter(source    == input$corr_y_source)
    if (!is.null(input$corr_y_test)   && input$corr_y_test   != "All") df <- df %>% filter(test_type == input$corr_y_test)
    metrics <- sort(unique(na.omit(df$metric_name))); metrics <- metrics[nzchar(metrics)]
    updateSelectInput(session, "corr_y_metric", choices = metrics, selected = metrics[1] %||% "")
  }, ignoreInit = FALSE)

  observeEvent(input$corr_select_all_y, {
    df <- corr_base_df()
    if (!is.null(input$corr_y_source) && input$corr_y_source != "All") df <- df %>% filter(source    == input$corr_y_source)
    if (!is.null(input$corr_y_test)   && input$corr_y_test   != "All") df <- df %>% filter(test_type == input$corr_y_test)
    metrics <- sort(unique(na.omit(df$metric_name))); metrics <- metrics[nzchar(metrics)]
    updateSelectInput(session, "corr_y_metric", selected = metrics)
  }, ignoreInit = TRUE)

  observeEvent(input$corr_y_metric, {
    ys <- input$corr_y_metric
    if (is.null(ys) || length(ys) == 0) return()
    updateSelectInput(session, "corr_plot_y", choices = ys, selected = ys[1])
  }, ignoreInit = TRUE)

  corr_result <- eventReactive(input$run_corr, {
    req(!is.null(input$corr_x_metric), nzchar(input$corr_x_metric))
    req(!is.null(input$corr_y_metric), length(input$corr_y_metric) >= 1)
    df <- corr_base_df()
    df_x_pool <- df
    if (!is.null(input$corr_x_source) && input$corr_x_source != "All") df_x_pool <- df_x_pool %>% filter(source    == input$corr_x_source)
    if (!is.null(input$corr_x_test)   && input$corr_x_test   != "All") df_x_pool <- df_x_pool %>% filter(test_type == input$corr_x_test)
    df_y_pool <- df
    if (!is.null(input$corr_y_source) && input$corr_y_source != "All") df_y_pool <- df_y_pool %>% filter(source    == input$corr_y_source)
    if (!is.null(input$corr_y_test)   && input$corr_y_test   != "All") df_y_pool <- df_y_pool %>% filter(test_type == input$corr_y_test)
    latest_x <- df_x_pool %>% group_by(player_id, player_name, metric_key) %>% slice_max(date, n = 1, with_ties = FALSE) %>% ungroup()
    latest_y <- df_y_pool %>% group_by(player_id, player_name, metric_key) %>% slice_max(date, n = 1, with_ties = FALSE) %>% ungroup()
    x_key <- latest_x %>% filter(metric_name == input$corr_x_metric) %>% count(metric_key, sort = TRUE) %>% dplyr::slice_head(n = 1) %>% pull(metric_key)
    req(length(x_key) == 1)
    df_x <- latest_x %>% filter(metric_key == x_key) %>% transmute(player_id, player_name, x = suppressWarnings(as.numeric(metric_value)))
    y_names <- input$corr_y_metric
    summary <- lapply(y_names, function(y_nm) {
      y_key <- latest_y %>% filter(metric_name == y_nm) %>% count(metric_key, sort = TRUE) %>% dplyr::slice_head(n = 1) %>% pull(metric_key)
      if (length(y_key) != 1) return(tibble::tibble(Metric_X = input$corr_x_metric, Metric_Y = y_nm, N = 0, Correlation = NA_real_, P_value = NA_real_))
      df_y   <- latest_y %>% filter(metric_key == y_key) %>% transmute(player_id, player_name, y = suppressWarnings(as.numeric(metric_value)))
      joined <- inner_join(df_x, df_y, by = c("player_id", "player_name")) %>% filter(!is.na(x), !is.na(y))
      if (nrow(joined) < 3) return(tibble::tibble(Metric_X = input$corr_x_metric, Metric_Y = y_nm, N = nrow(joined), Correlation = NA_real_, P_value = NA_real_))
      ct <- suppressWarnings(cor.test(joined$x, joined$y, method = "spearman"))
      tibble::tibble(Metric_X = input$corr_x_metric, Metric_Y = y_nm, N = nrow(joined), Correlation = unname(ct$estimate), P_value = ct$p.value)
    }) %>% bind_rows() %>% arrange(desc(abs(Correlation)))
    y_plot_nm  <- input$corr_plot_y %||% y_names[1]
    y_plot_key <- latest_y %>% filter(metric_name == y_plot_nm) %>% count(metric_key, sort = TRUE) %>% dplyr::slice_head(n = 1) %>% pull(metric_key)
    scatter <- tibble::tibble()
    if (length(y_plot_key) == 1) {
      df_y_plot <- latest_y %>% filter(metric_key == y_plot_key) %>% transmute(player_id, player_name, y = suppressWarnings(as.numeric(metric_value)))
      scatter   <- inner_join(df_x, df_y_plot, by = c("player_id", "player_name")) %>% filter(!is.na(x), !is.na(y))
    }
    list(summary = summary, scatter = scatter, y_plot_nm = y_plot_nm)
  })

  output$corr_table <- renderDT({
    res <- corr_result()
    datatable(res$summary, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE)) %>%
      formatRound(c("Correlation", "P_value"), 4)
  })

  output$corr_plot <- renderPlotly({
    res <- corr_result(); df <- res$scatter
    if (nrow(df) == 0) return(NULL)
    highlight_nm <- input$corr_highlight_player
    p <- ggplot(df, aes(x = x, y = y, text = player_name)) +
      geom_point(size = 2, alpha = 0.7) +
      geom_vline(xintercept = median(df$x, na.rm = TRUE), linetype = "dotted") +
      geom_hline(yintercept = median(df$y, na.rm = TRUE), linetype = "dotted")
    if (!is.null(highlight_nm) && highlight_nm != "None") {
      df_hi <- df %>% filter(player_name == highlight_nm)
      if (nrow(df_hi) == 1) p <- p + geom_point(data = df_hi, aes(x = x, y = y), inherit.aes = FALSE, size = 4, color = "#DC2626")
    }
    p <- p + theme_minimal(base_size = 12) +
      labs(x = input$corr_x_metric, y = res$y_plot_nm, title = paste0("Latest snapshot scatter: ", res$y_plot_nm, " vs ", input$corr_x_metric))
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)
