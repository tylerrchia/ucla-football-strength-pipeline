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

# --- name -> file slug used by images ---
player_photo_slug <- function(player_name) {
  # Expect player_name like "First Last" or "First Middle Last"
  # File rule: firstname chunk = all tokens except last, concatenated w/ no spaces;
  # hyphens removed; last name = last token.
  
  s <- tolower(stringr::str_trim(player_name))
  s <- stringr::str_replace_all(s, "[’']", "")      # remove apostrophes
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
    bind_rows(d, d %>% slice(1))
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
  "Jump Height 25%",
  "Force at Peak Power 20%",
  "Force at Zero Velocity 15%",
  "Eccentric Braking Impulse 15%",
  "Avg Max Force (L/R) 20%",
  "Max Imbalance 5% (lower is better)",
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
    slice(1)
  
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
  "Jump Height (Imp-Mom) in Inches",
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
  "Max Imbalance",
  "Impulse Imbalance"
)



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
  "Total Distance",
  "Max Vel",
  "Max Effort Acceleration",
  "Max Effort Deceleration",
  "Total Player Load"
)

catapult_metric_keys_map <- pick_best_keys_for_metric_names("Catapult", catapult_metric_names, fill_summary)

radar_catapult_metrics <- unname(catapult_metric_keys_map[!is.na(catapult_metric_keys_map)])

radar_catapult_labels <- tibble::tibble(
  metric_key = unname(catapult_metric_keys_map),
  radar_label = names(catapult_metric_keys_map)
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
    slice(1)
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
    tags$link(rel = "stylesheet", type = "text/css", href = "ucla.css")
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
              "Performance",
              fluidRow(
                column(
                  3,
                  selectInput("perf_source", "Source", choices = NULL, selected = "All"),
                  selectInput("perf_test",   "Test Type",   choices = NULL, selected = "All"),
                  selectInput("perf_metric", "Metric", choices = NULL, selected = "All"),
                  dateRangeInput(
                    "perf_dates", "Date range",
                    start = min(vald_tests_long_ui$date, na.rm = TRUE),
                    end   = max(vald_tests_long_ui$date, na.rm = TRUE)
                  )
                ),
                column(
                  9,
                  DTOutput("player_perf_table")
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
            )
            ,
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
                  DTOutput("poscomp_table"),
                  plotlyOutput("poscomp_radar", height = 420)
                )
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
                )              )
            )
          )
        )
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
    
    # ForceDecks...
    "Athlete Standing Weight",
    "Jump Height (Imp-Mom) in Inches",
    "RSI-modified",
    "Force at Peak Power",
    "Force at Zero Velocity",
    "Eccentric Braking Impulse",
    "Concentric Impulse",
    
    # NordBord...
    "L Max Force",
    "R Max Force",
    "L Max Impulse",
    "R Max Impulse",
    "Max Imbalance",
    "Impulse Imbalance",
    
    # Catapult
    "Total Distance",
    "Max Vel",
    "Max Effort Acceleration",
    "Max Effort Deceleration",
    "Total Player Load"
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
    df_disp <- df %>% dplyr::select(dplyr::any_of(cols))
    
    
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
    
    colnames(df_disp) <- disp_names
    
    
    container <- make_dt_container_with_ath_tooltip(names(df_disp))
    
    dt <- DT::datatable(
      df_disp,
      container = container,   # ✅ add this
      escape = FALSE,          # ✅ important so ⓘ renders
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        searchHighlight = TRUE,
        columnDefs = list(
          list(
            targets = 0,
            className = "dt-nowrap"
          )
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
    pid <- roster_view %>% filter(player_name == nm) %>% slice(1) %>% pull(player_id)
    req(length(pid) == 1)
    pid
  })
  
  poscomp_cohort <- reactive({
    nm <- selected_player()
    row <- roster_view %>% filter(player_name == nm) %>% slice(1)
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
    row <- roster_view %>% filter(player_name == nm) %>% slice(1)
    if (nrow(row) == 0) return(NULL)
    
    group_val <- if (!is.null(group_col)) row[[group_col]][1] else NA
    pos_val   <- if (!is.null(pos_col))   row[[pos_col]][1] else NA
    class_val <- if (!is.null(class_col)) row[[class_col]][1] else NA
    headshot  <- if (!is.null(headshot_col)) row[[headshot_col]][1] else NA
    
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
    
    
    fluidRow(
      column(
        12,
        div(
          style = "padding:12px; border:1px solid #e5e7eb; border-radius:12px; margin-bottom:12px;",
          fluidRow(
            column(
              2,
              if (!is.null(headshot_col) && !is.na(headshot) && nzchar(headshot)) {
                tags$img(src = headshot, style="width:100%; height:110px; object-fit:cover; border-radius:10px;")
              } else {
                div(
                  style="width:100%; height:110px; border-radius:10px; background:#f3f4f6; display:flex; align-items:center; justify-content:center;",
                  span("No headshot")
                )
              }
            ),
            column(
              10,
              h3(nm, style = "border-bottom:4px solid #FFD100; padding-bottom:4px;"),
              tags$div(
                style="color:#374151;",
                paste(
                  if (!is.na(group_val)) paste0("Group: ", group_val) else "",
                  if (!is.na(pos_val))   paste0(" | Pos: ", pos_val) else "",
                  if (!is.na(class_val)) paste0(" | Class: ", class_val) else "",
                  paste0(" | As-of: ", as.character(as_of_date))
                )
              ),
              tags$div(
                style="color:#374151; margin-top:4px;",
                paste0(
                  "Ht: ", ht_val,
                  " | Wt: ", wt_val,
                  " | Wing: ", wing_val,
                  " | Hand: ", hand_val,
                  " | Arm: ", arm_val
                )
              ),
              
              tags$div(
                style = "display:flex;flex-direction:row;flex-wrap:nowrap;gap:8px;align-items:center;",
                photo_thumb_click(photos$front, "Front", "photo_front_click"),
                photo_thumb_click(photos$side,  "Side",  "photo_side_click"),
                photo_thumb_click(photos$back,  "Back",  "photo_back_click")
              )
              
            )
          )
        )
      )
    )
  })
  
  show_photo_modal <- function(src, title) {
    if (is.na(src) || !nzchar(src)) return()
    
    showModal(
      modalDialog(
        title = title,
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tags$div(
          style="display:flex;justify-content:center;",
          tags$img(
            src = src,
            style = "max-width:100%; max-height:75vh; object-fit:contain; border-radius:12px; border:1px solid #e5e7eb;"
          )
        )
      )
    )
  }
  
  observeEvent(input$photo_front_click, {
    nm <- selected_player()
    src <- find_player_photo(nm, "front")
    show_photo_modal(src, paste0(nm, " — Front"))
  })
  
  observeEvent(input$photo_side_click, {
    nm <- selected_player()
    src <- find_player_photo(nm, "side")
    show_photo_modal(src, paste0(nm, " — Side"))
  })
  
  observeEvent(input$photo_back_click, {
    nm <- selected_player()
    src <- find_player_photo(nm, "back")
    show_photo_modal(src, paste0(nm, " — Back"))
  })
  
  
  
  # ---------- Radar (MVP: NordBord avg percentile vs ForceDecks avg percentile) ----------
  
  # output$radar_force_plot <- renderPlot({
  #   nm <- selected_player()
  #   pid <- roster_view %>% filter(player_name == nm) %>% slice(1) %>% pull(player_id)
  #   if (length(pid) == 0) return(NULL)
  #   
  #   df <- roster_percentiles_long %>%
  #     filter(player_id == pid, metric_key %in% radar_force_metrics) %>%
  #     left_join(radar_force_labels, by = "metric_key") %>%
  #     mutate(radar_label = factor(radar_label, levels = radar_force_labels$radar_label))
  #   
  #   if (nrow(df) < 3) {
  #     missing <- setdiff(force_metric_names, radar_force_labels$radar_label)
  #     msg <- if (length(missing) > 0) paste("Missing:", paste(missing, collapse = ", ")) else "Not enough metrics."
  #     return(ggplot() + theme_void() + labs(title = "ForceDecks radar unavailable", subtitle = msg))
  #   }
  #   
  #   df <- df %>%
  #     mutate(
  #       radar_label_wrap = stringr::str_wrap(radar_label, width = 16),
  #       radar_label_wrap = factor(radar_label_wrap, levels = unique(radar_label_wrap))
  #     )
  #   
  #   ggplot(df, aes(x = radar_label_wrap, y = percentile, group = 1)) +
  #     geom_polygon(alpha = 0.25) +
  #     geom_path(linewidth = 1.2) +     # ✅ ADD LINE
  #     geom_point(size = 2) +
  #     coord_polar(clip = "off") +
  #     ylim(0, 100) +
  #     theme_minimal(base_size = 12) +
  #     theme(
  #       axis.title = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       axis.text.x = element_text(size = 8),
  #       plot.margin = margin(12, 40, 12, 40)  # give labels room
  #     ) +
  #     labs(title = "ForceDecks (position-group percentiles)")
  # 
  # 
  # })
  
  output$radar_force_plot <- renderPlotly({
    nm <- selected_player()
    pid <- roster_view %>% filter(player_name == nm) %>% slice(1) %>% pull(player_id)
    req(length(pid) == 1)
    
    df <- roster_percentiles_long %>%
      filter(player_id == pid, metric_key %in% radar_force_metrics) %>%
      left_join(radar_force_labels, by = "metric_key") %>%
      mutate(axis = radar_label)
    
    render_plotly_radar(
      df_long = df,
      selected_player = nm,
      title = "ForceDecks (position-group percentiles)"
    )
  })
  
  

    
  
  # output$radar_nord_plot <- renderPlot({
  #   nm <- selected_player()
  #   pid <- roster_view %>% filter(player_name == nm) %>% slice(1) %>% pull(player_id)
  #   if (length(pid) == 0) return(NULL)
  #   
  #   df <- roster_percentiles_long %>%
  #     filter(player_id == pid, metric_key %in% radar_nord_metrics) %>%
  #     left_join(radar_nord_labels, by = "metric_key") %>%
  #     mutate(radar_label = factor(radar_label, levels = radar_nord_labels$radar_label))
  #   
  #   if (nrow(df) < 3) {
  #     missing <- setdiff(nord_metric_names, radar_nord_labels$radar_label)
  #     msg <- if (length(missing) > 0) paste("Missing:", paste(missing, collapse = ", ")) else "Not enough metrics."
  #     return(ggplot() + theme_void() + labs(title = "NordBord radar unavailable", subtitle = msg))
  #   }
  #   
  #   df <- df %>%
  #     mutate(
  #       radar_label_wrap = stringr::str_wrap(radar_label, width = 16),
  #       radar_label_wrap = factor(radar_label_wrap, levels = unique(radar_label_wrap))
  #     )
  #   
  #   ggplot(df, aes(x = radar_label_wrap, y = percentile, group = 1)) +
  #     geom_polygon(alpha = 0.25) +
  #     geom_path(linewidth = 1.2) +     # ✅ ADD LINE
  #     geom_point(size = 2) +
  #     coord_polar(clip = "off") +
  #     ylim(0, 100) +
  #     theme_minimal(base_size = 12) +
  #     theme(
  #       axis.title = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       axis.text.x = element_text(size = 8),
  #       plot.margin = margin(12, 40, 12, 40)  # give labels room
  #     ) + labs(title = "NordBord (position-group percentiles)")
  #   })
  
  output$radar_nord_plot <- renderPlotly({
    nm <- selected_player()
    pid <- roster_view %>% filter(player_name == nm) %>% slice(1) %>% pull(player_id)
    req(length(pid) == 1)
    
    df <- roster_percentiles_long %>%
      filter(player_id == pid, metric_key %in% radar_nord_metrics) %>%
      left_join(radar_nord_labels, by = "metric_key") %>%
      mutate(axis = radar_label)
    
    render_plotly_radar(
      df_long = df,
      selected_player = nm,
      title = "NordBord (position-group percentiles)"
    )
  })
  
  output$radar_catapult_plot <- renderPlotly({
    nm <- selected_player()
    pid <- roster_view %>% filter(player_name == nm) %>% slice(1) %>% pull(player_id)
    req(length(pid) == 1)
    
    df <- roster_percentiles_long %>%
      filter(player_id == pid, metric_key %in% radar_catapult_metrics) %>%
      left_join(radar_catapult_labels, by = "metric_key") %>%
      mutate(axis = radar_label) %>%
      transmute(player_name = nm, axis, percentile)
    
    render_plotly_radar(
      df_long = df,
      selected_player = nm,
      title = "Catapult (position-group percentiles)"
    )
  })
  
  
  
  # ---------- Quick tags ----------
  output$player_tags <- renderUI({
    nm <- selected_player()
    pid <- roster_view %>% filter(player_name == nm) %>% slice(1) %>% pull(player_id)
    if (length(pid) == 0) return(NULL)
    
    latest_long <- vald_tests_long_ui %>%
      filter(date <= as_of_date) %>%
      group_by(player_id, metric_key) %>%
      slice_max(date, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    # ---- MOST RECENT metric per metric_key ----
    latest_pcts <- latest_long %>%
      filter(player_id == pid) %>%
      select(player_id, metric_key, date) %>%
      left_join(
        roster_percentiles_long %>% select(player_id, metric_key, percentile),
        by = c("player_id", "metric_key")
      ) %>%
      filter(!is.na(percentile))
    
    if (nrow(latest_pcts) == 0) {
      return(tags$ul(tags$li("No percentile tags yet.")))
    }
    
    # ---- Strengths (top end) ----
    strengths <- latest_pcts %>%
      filter(percentile >= 80) %>%           # ← adjust threshold if you want
      arrange(desc(percentile)) %>%
      slice_head(n = 5) %>%
      mutate(
        metric_short = sub("^.*\\|", "", metric_key),
        label = paste0(metric_short, " (", round(percentile, 1), "th)")
      ) %>%
      pull(label)
    
    # ---- Weaknesses (bottom end) ----
    weaknesses <- latest_pcts %>%
      filter(percentile <= 20) %>%            # ← adjust threshold if you want
      arrange(percentile) %>%
      slice_head(n = 5) %>%
      mutate(
        metric_short = sub("^.*\\|", "", metric_key),
        label = paste0(metric_short, " (", round(percentile, 1), "th)")
      ) %>%
      pull(label)
    
    tags$div(
      tags$h5(style="color:#2774AE;", "Strengths"),
      if (length(strengths) == 0) {
        tags$ul(tags$li("—"))
      } else {
        tags$ul(lapply(strengths, tags$li))
      },
      tags$h5(style="color:#9B1C1C;", "Weaknesses"),
      if (length(weaknesses) == 0) {
        tags$ul(tags$li("—"))
      } else {
        tags$ul(lapply(weaknesses, tags$li))
      }
    )
  })
  

  
  # ---------- Performance table (all tests for player with percentile lookup) ----------
  output$player_perf_table <- renderDT({
    pid <- player_pid()
    df <- perf_filtered_df()
    
    pct_map <- roster_percentiles_long %>%
      filter(player_id == pid) %>%
      select(metric_key, RosterPercentile = percentile)
    
    df_out <- df %>%
      left_join(pct_map, by = "metric_key") %>%
      arrange(desc(date), source, test_type, metric_name) %>%
      transmute(
        Date   = date,
        Source = source,
        Test   = test_type,
        Metric = metric_name,
        Value  = as.numeric(metric_value),
        Units  = units,
        `Position Percentile (as-of)` = as.numeric(RosterPercentile)
      )
    
    DT::datatable(
      df_out,
      rownames = FALSE,
      selection = "none",
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        columnDefs = list(
          list(targets = 0, className = "dt-nowrap"),
          
          # Value column: 2dp max, trim trailing zeros
          list(
            targets = which(names(df_out) == "Value") - 1,
            render = DT::JS(
              "function(data, type, row, meta) {
               if (data === null || data === undefined || data === '') return '';
               var num = Number(data);
               if (type === 'display') {
                 if (Number.isInteger(num)) return num.toString();
                 return num.toFixed(2).replace(/\\.00$/, '').replace(/0$/, '');
               }
               return num; // keep numeric for sort/filter
             }"
            )
          ),
          
          # Position Percentile: max 2dp, drop .00
          list(
            targets = which(names(df_out) == "Position Percentile (as-of)") - 1,
            render = DT::JS(
              "function(data, type, row, meta) {
               if (data === null || data === undefined || data === '') return '';
               var num = Number(data);
               if (type === 'display') {
                 if (Number.isInteger(num)) return num.toString();
                 return num.toFixed(2).replace(/\\.00$/, '');
               }
               return num; // keep numeric for sort/filter
             }"
            )
          )
        )
      )
    )
  })
  
  
  
  # ---------- Trend plot ----------
  output$trend_plot <- renderPlotly({
    nm <- selected_player()
    pid <- roster_view %>% filter(player_name == nm) %>% slice(1) %>% pull(player_id)
    if (length(pid) == 0) return(NULL)
    
    df_base <- trend_filtered_df()
    
    if (is.null(input$trend_metric) || input$trend_metric == "All") {
      return(NULL)
    }
    if (nrow(df_base) == 0) return(NULL)
    
    mk <- df_base %>% distinct(metric_key) %>% slice(1) %>% pull(metric_key)
    pretty_mk <- sub("^([^|]*\\|){2}", "", mk)
    
    df_all <- vald_tests_long_ui %>% filter(metric_key == mk)
    
    # --- Selected player ---
    df_main <- df_all %>%
      filter(player_id == pid) %>%
      arrange(date) %>%
      mutate(Line = nm)
    
    if (nrow(df_main) == 0) return(NULL)
    
    plot_players <- df_main
    
    # --- Overlay another player (optional) ---
    overlay_nm <- input$trend_overlay_player
    if (!is.null(overlay_nm) && overlay_nm != "None" && overlay_nm != nm) {
      pid2 <- roster_view %>% filter(player_name == overlay_nm) %>% slice(1) %>% pull(player_id)
      if (length(pid2) == 1) {
        df_other <- df_all %>%
          filter(player_id == pid2) %>%
          arrange(date) %>%
          mutate(Line = overlay_nm)
        
        if (nrow(df_other) > 0) plot_players <- bind_rows(plot_players, df_other)
      }
    }
    
    # ---- Position average (optional) ----
    pos_avg <- NULL
    if (isTRUE(input$overlay_pos_avg) && !is.null(pos_col)) {
      
      # get selected player's position
      pos_val <- roster_view %>%
        filter(player_id == pid) %>%
        slice(1) %>%
        pull(.data[[pos_col]])
      
      if (!is.na(pos_val)) {
        pos_players <- roster_view %>%
          filter(.data[[pos_col]] == pos_val) %>%
          pull(player_id)
        
        pos_avg <- df_all %>%
          filter(player_id %in% pos_players) %>%
          group_by(date) %>%
          summarise(
            metric_value = if (all(is.na(metric_value)))
              NA_real_
            else
              mean(metric_value, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          filter(is.finite(metric_value)) %>%
          mutate(Line = paste0(pos_val, " Avg"))
      }
    }
    
    
    # --- Position group overlay (points, hover shows player name) ---
    pos_pts <- NULL
    if (isTRUE(input$overlay_pos_group) && !is.null(pos_col) && pos_col %in% names(roster_view)) {
      
      pos_val <- roster_view %>%
        filter(player_id == pid) %>%
        slice(1) %>%
        pull(.data[[pos_col]])
      
      if (length(pos_val) == 1 && !is.na(pos_val) && nzchar(pos_val)) {
        pos_ids <- roster_view %>%
          filter(.data[[pos_col]] == pos_val) %>%
          pull(player_id)
        
        pos_pts <- df_all %>%
          filter(player_id %in% pos_ids) %>%
          select(player_id, player_name, date, metric_value) %>%
          mutate(Group = paste0("Pos: ", pos_val))
      }
    }
    
    # Build plot
    p <- ggplot() +
      geom_line(
        data = plot_players,
        aes(x = date, y = metric_value, color = Line, group = Line, text = player_name),
        linewidth = 1
      ) +
      geom_point(
        data = plot_players,
        aes(x = date, y = metric_value, color = Line, text = player_name),
        size = 2
      )
    
if (!is.null(pos_avg) && nrow(pos_avg) > 0) {

  # always show at least a point
  p <- p +
    geom_point(
      data = pos_avg,
      aes(x = date, y = metric_value, color = Line),
      size = 2.8
    )

  # draw dashed line only if 2+ dates
  if (nrow(pos_avg) >= 2) {
    p <- p +
      geom_line(
        data = pos_avg,
        aes(x = date, y = metric_value, color = Line, group = Line),
        linewidth = 1,
        linetype = "dashed",
        na.rm = TRUE
      )
  }
}

    
    if (!is.null(pos_pts) && nrow(pos_pts) > 0) {
      p <- p +
        geom_point(
          data = pos_pts,
          aes(x = date, y = metric_value, text = player_name),
          inherit.aes = FALSE,
          alpha = 0.25,
          size = 2
        )
    }
    
    p <- p +
      theme_minimal(base_size = 12) +
      labs(
        x = "Date",
        y = "Value",
        color = NULL,
        title = paste(nm, "—", pretty_mk)
      ) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
  poscomp_table_df <- reactive({
    cohort <- poscomp_cohort()
    if (nrow(cohort) < 1) return(tibble::tibble())
    
    # --- ADD WEIGHT KEY (ForceDecks) ---
    # Try to find the metric_key that ends with "Athlete Standing Weight"
    weight_key <- keep_roster_metrics[
      startsWith(keep_roster_metrics, "ForceDecks|") &
        grepl("\\|Athlete Standing Weight$", keep_roster_metrics)
    ][1] %||% NA_character_
    
    # Keys used in the positional comparison table:
    keys <- unique(c(radar_force_metrics, radar_nord_metrics, radar_catapult_metrics, weight_key))
    keys <- keys[!is.na(keys)]
    if (length(keys) < 1) return(tibble::tibble())
    
    # Labels for radar metrics
    label_df <- bind_rows(radar_force_labels, radar_nord_labels, radar_catapult_labels) %>%
      distinct(metric_key, radar_label) %>%
      mutate(radar_label = dplyr::coalesce(radar_label, pretty_metric_key(metric_key)))
    
    
    # --- ADD LABEL FOR WEIGHT (so it pivots nicely) ---
    if (!is.na(weight_key)) {
      label_df <- bind_rows(
        label_df,
        tibble::tibble(metric_key = weight_key, radar_label = "Athlete Standing Weight")
      ) %>% distinct(metric_key, .keep_all = TRUE)
    }
    
    # make labels unique to prevent pivot_wider issues
    label_df <- label_df %>%
      group_by(radar_label) %>%
      mutate(radar_label = ifelse(n() > 1, paste0(radar_label, " [", row_number(), "]"), radar_label)) %>%
      ungroup()
    
    latest_vals <- vald_tests_long_ui %>%
      filter(player_id %in% cohort$player_id, metric_key %in% keys, date <= as_of_date) %>%
      group_by(player_id, player_name, metric_key) %>%
      slice_max(date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(
        player_name,
        metric_key,
        raw_value = suppressWarnings(as.numeric(metric_value))
      )
    
    if (nrow(latest_vals) == 0) return(tibble::tibble())
    
    long <- latest_vals %>%
      left_join(label_df, by = "metric_key") %>%
      mutate(col = dplyr::coalesce(radar_label, pretty_metric_key(metric_key))) %>%
      filter(!is.na(col), nzchar(col))
    
    out <- long %>%
      select(player_name, col, raw_value) %>%
      tidyr::pivot_wider(names_from = col, values_from = raw_value)
    
    # ---- Add Athleticism Score from roster_view (composite column) ----
    if (HAS_ATH) {
      ath_df <- roster_view %>%
        filter(player_id %in% cohort$player_id) %>%
        select(player_name, !!ATH_KEY) %>%
        rename(`Athleticism Score` = !!ATH_KEY)
      
      out <- out %>%
        left_join(ath_df, by = "player_name")
    }
    
    
    nm <- selected_player()
    
    weight_col <- "Athlete Standing Weight"
    
    out %>%
      rename(`Player Name` = player_name) %>%
      {
        # Put Athleticism right after Player Name (if present), then Weight (if present)
        base <- .
        cols <- c("Player Name")
        if ("Athleticism Score" %in% names(base)) cols <- c(cols, "Athleticism Score")
        if (weight_col %in% names(base)) cols <- c(cols, weight_col)
        dplyr::select(base, dplyr::any_of(cols), dplyr::everything())
      } %>%
      mutate(.sel = (`Player Name` == nm)) %>%
      arrange(desc(.sel), `Player Name`) %>%
      select(-.sel)
    
  })
  
  
  output$poscomp_table <- renderDT({
    df_vals <- poscomp_table_df()
    
    validate(need(nrow(df_vals) > 0, "No positional comparison data available."))
    
    id_col <- "Player Name"
    metric_cols <- setdiff(names(df_vals), id_col)
    
    # ---- Clean column headers (positional comparison table only) ----
    clean_poscomp_header <- function(nm) {
      nm <- gsub("Jump Height \\(Imp-Mom\\) in Inches", "Jump Height", nm)
      nm <- gsub("Jump Height \\(Imp-Mom\\)", "Jump Height", nm)
      nm <- gsub("RSI-modified \\(Imp-Mom\\)", "RSI-modified", nm)
      
      # generic cleanup for any future columns that include these suffixes
      nm <- gsub("\\(Imp-Mom\\)", "", nm)
      nm <- gsub(" in Inches", "", nm)
      trimws(nm)
    }
    
    names(df_vals) <- vapply(names(df_vals), clean_poscomp_header, character(1))
    
    # recompute metric_cols since names changed
    metric_cols <- setdiff(names(df_vals), id_col)
    
    
    dt <- DT::datatable(
      df_vals,
      rownames = FALSE,
      selection = "none",
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "t",
        columnDefs = list(list(targets = 0, className = "dt-nowrap")),
        order = if ("Athleticism Score" %in% names(df_vals)) {
          list(list(which(names(df_vals) == "Athleticism Score") - 1, "desc"))
        } else {
          list()
        }
      )
    )
    
    # Color-code Athleticism Score if present
    if ("Athleticism Score" %in% names(df_vals)) {
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
  
  
  
  output$poscomp_radar <- renderPlotly({
    req(isTRUE(input$poscomp_show_radar))
    
    cohort <- poscomp_cohort()
    validate(need(nrow(cohort) >= 1, "No cohort available for positional comparison."))
    
    keys <- unique(c(radar_force_metrics, radar_nord_metrics, radar_catapult_metrics))
    validate(need(length(keys) >= 3, "Not enough radar metrics available to plot."))
    
    # metric_key -> label
    label_df <- bind_rows(radar_force_labels, radar_nord_labels, radar_catapult_labels) %>%
      distinct(metric_key, radar_label)
    
    long <- roster_percentiles_long %>%
      filter(player_id %in% cohort$player_id, metric_key %in% keys) %>%
      left_join(label_df, by = "metric_key") %>%
      mutate(axis = dplyr::coalesce(radar_label, pretty_metric_key(metric_key)))
    
    # Make sure every player has every axis (keeps polygons aligned)
    axis_levels <- sort(unique(long$axis))
    long <- long %>%
      tidyr::complete(
        player_id, player_name, axis = axis_levels,
        fill = list(percentile = NA_real_)
      )
    
    make_closed <- function(df_one) {
      df_one <- df_one %>% arrange(axis)
      # close loop by repeating first axis point
      bind_rows(df_one, df_one %>% slice(1))
    }
    
    nm_sel <- selected_player()
    
    p <- plotly::plot_ly()
    
    for (pn in unique(long$player_name)) {
      df_one <- long %>%
        filter(player_name == pn) %>%
        make_closed()
      
      # if a player has all NA, skip (prevents plotly oddness)
      if (all(is.na(df_one$percentile))) next
      
      is_sel <- identical(pn, nm_sel)
      
      p <- p %>%
        plotly::add_trace(
          data = df_one,
          type = "scatterpolar",
          mode = "lines+markers",
          r = ~percentile,
          theta = ~axis,
          name = pn,
          text = ~player_name,
          hovertemplate = "%{text}<br>%{theta}: %{r:.1f}<extra></extra>",
          connectgaps = TRUE,
          
          # ✅ make lines actually visible
          line = list(
            width = if (is_sel) 4 else 1.5,
            shape = "linear"
          ),
          
          # ✅ fill only the selected player to make it obvious
          fill = if (is_sel) "toself" else "none",
          fillcolor = if (is_sel) "rgba(39,116,174,0.12)" else NULL,
          
          # ✅ make markers visible but not overwhelming
          marker = list(size = if (is_sel) 6 else 4),
          
          opacity = if (is_sel) 1 else 0.15,
          showlegend = is_sel
        )
      
    }
    
    p %>%
      plotly::layout(
        polar = list(
          radialaxis = list(range = c(0, 100), tickvals = c(0, 25, 50, 75, 100))
        ),
        margin = list(l = 40, r = 40, t = 40, b = 40),
        showlegend = TRUE
      )
    
  })
  

  
# comps
  output$compare_force_radar_plot <- renderPlotly({
    nm2 <- input$compare_player
    req(!is.null(nm2), nzchar(nm2))
    
    pid2 <- roster_view %>% filter(player_name == nm2) %>% slice(1) %>% pull(player_id)
    req(length(pid2) == 1)
    
    key_df <- vald_tests_long_ui %>%
      distinct(metric_key, source, metric_name, test_type) %>%
      mutate(system = source)
    
    force_metric_names <- c(
      "Jump Height (Imp-Mom) in Inches",
      "RSI-modified",
      "Force at Peak Power",
      "Force at Zero Velocity",
      "Eccentric Braking Impulse",
      "Concentric Impulse"
    )
    
    force_keys <- vapply(
      force_metric_names,
      function(nm) pick_metric_key_by_name("ForceDecks", nm, key_df),
      character(1)
    )
    force_keys <- force_keys[!is.na(force_keys)]
    
    df <- roster_percentiles_long %>%
      filter(player_id == pid2, metric_key %in% force_keys) %>%
      left_join(key_df %>% distinct(metric_key, metric_name), by = "metric_key") %>%
      mutate(
        axis = stringr::str_wrap(norm_metric(metric_name), width = 16),
        axis = factor(axis, levels = stringr::str_wrap(force_metric_names, width = 16))
      ) %>%
      transmute(player_name = nm2, axis = as.character(axis), percentile)
    
    validate(need(nrow(df) >= 3, "Not enough ForceDecks metrics for radar."))
    
    render_plotly_radar(
      df_long = df,
      selected_player = nm2,
      title = paste0("ForceDecks — ", nm2, " (position-group percentiles)")
    )
  })
  
  
  
  output$compare_nord_radar_plot <- renderPlotly({
    nm2 <- input$compare_player
    req(!is.null(nm2), nzchar(nm2))
    
    pid2 <- roster_view %>% filter(player_name == nm2) %>% slice(1) %>% pull(player_id)
    req(length(pid2) == 1)
    
    df <- roster_percentiles_long %>%
      filter(player_id == pid2, metric_key %in% radar_nord_metrics) %>%
      left_join(radar_nord_labels, by = "metric_key") %>%
      mutate(
        axis = stringr::str_wrap(radar_label, width = 16),
        axis = factor(axis, levels = stringr::str_wrap(radar_nord_labels$radar_label, width = 16))
      ) %>%
      transmute(player_name = nm2, axis = as.character(axis), percentile)
    
    validate(need(nrow(df) >= 3, "Not enough NordBord metrics for radar."))
    
    render_plotly_radar(
      df_long = df,
      selected_player = nm2,
      title = paste0("NordBord — ", nm2, " (position-group percentiles)")
    )
  })
  
  
  output$compare_catapult_radar_plot <- renderPlotly({
    nm2 <- input$compare_player
    req(!is.null(nm2), nzchar(nm2))
    
    pid2 <- roster_view %>% filter(player_name == nm2) %>% slice(1) %>% pull(player_id)
    req(length(pid2) == 1)
    
    df <- roster_percentiles_long %>%
      filter(player_id == pid2, metric_key %in% radar_catapult_metrics) %>%
      left_join(radar_catapult_labels, by = "metric_key") %>%
      mutate(axis = stringr::str_wrap(radar_label, width = 16)) %>%
      transmute(player_name = nm2, axis, percentile)
    
    validate(need(nrow(df) >= 3, "Not enough Catapult metrics for radar."))
    
    render_plotly_radar(
      df_long = df,
      selected_player = nm2,
      title = paste0("Catapult — ", nm2, " (position-group percentiles)")
    )
  })
  
  
  # ---------------------------
  # Correlations (X/Y separate source/test filters)
  # ---------------------------
  
  corr_base_df <- reactive({
    df <- vald_tests_long_ui
    
    # Restrict to whatever the roster explorer currently allows
    allow_keys <- roster_allowed_metric_keys()
    if (!is.null(allow_keys) && length(allow_keys) > 0) {
      df <- df %>% filter(metric_key %in% allow_keys)
    }
    
    df %>% filter(date <= as_of_date)
  })
  
  # ---- X test choices depend on X source ----
  observeEvent(input$corr_x_source, {
    df <- corr_base_df()
    if (!is.null(input$corr_x_source) && input$corr_x_source != "All") {
      df <- df %>% filter(source == input$corr_x_source)
    }
    
    tests <- sort(unique(na.omit(df$test_type)))
    updateSelectInput(session, "corr_x_test", choices = c("All", tests), selected = "All")
    
    # reset metric x
    updateSelectInput(session, "corr_x_metric", choices = character(0), selected = character(0))
  }, ignoreInit = FALSE)
  
  # ---- Y test choices depend on Y source ----
  observeEvent(input$corr_y_source, {
    df <- corr_base_df()
    if (!is.null(input$corr_y_source) && input$corr_y_source != "All") {
      df <- df %>% filter(source == input$corr_y_source)
    }
    
    tests <- sort(unique(na.omit(df$test_type)))
    updateSelectInput(session, "corr_y_test", choices = c("All", tests), selected = "All")
    
    # reset metric y + plot y
    updateSelectInput(session, "corr_y_metric", choices = character(0), selected = character(0))
    updateSelectInput(session, "corr_plot_y",  choices = character(0), selected = character(0))
  }, ignoreInit = FALSE)
  
  # ---- Populate Metric X choices based on X source/test ----
  observeEvent(
    list(input$corr_x_source, input$corr_x_test, input$pctl_system, input$pctl_test, input$pctl_metric),
    {
      df <- corr_base_df()
      
      if (!is.null(input$corr_x_source) && input$corr_x_source != "All") df <- df %>% filter(source == input$corr_x_source)
      if (!is.null(input$corr_x_test)   && input$corr_x_test   != "All") df <- df %>% filter(test_type == input$corr_x_test)
      
      metrics <- sort(unique(na.omit(df$metric_name)))
      metrics <- metrics[nzchar(metrics)]
      
      updateSelectInput(session, "corr_x_metric", choices = metrics, selected = metrics[1] %||% "")
    },
    ignoreInit = FALSE
  )
  
  # ---- Populate Metric Y choices based on Y source/test ----
  observeEvent(
    list(input$corr_y_source, input$corr_y_test, input$pctl_system, input$pctl_test, input$pctl_metric),
    {
      df <- corr_base_df()
      
      if (!is.null(input$corr_y_source) && input$corr_y_source != "All") df <- df %>% filter(source == input$corr_y_source)
      if (!is.null(input$corr_y_test)   && input$corr_y_test   != "All") df <- df %>% filter(test_type == input$corr_y_test)
      
      metrics <- sort(unique(na.omit(df$metric_name)))
      metrics <- metrics[nzchar(metrics)]
      
      updateSelectInput(session, "corr_y_metric", choices = metrics, selected = metrics[1] %||% "")
    },
    ignoreInit = FALSE
  )
  
  # ---- Select-all for Metric Y (respects Y source/test) ----
  observeEvent(input$corr_select_all_y, {
    df <- corr_base_df()
    if (!is.null(input$corr_y_source) && input$corr_y_source != "All") df <- df %>% filter(source == input$corr_y_source)
    if (!is.null(input$corr_y_test)   && input$corr_y_test   != "All") df <- df %>% filter(test_type == input$corr_y_test)
    
    metrics <- sort(unique(na.omit(df$metric_name)))
    metrics <- metrics[nzchar(metrics)]
    updateSelectInput(session, "corr_y_metric", selected = metrics)
  }, ignoreInit = TRUE)
  
  # ---- Scatterplot Y choices depend on selected Ys ----
  observeEvent(input$corr_y_metric, {
    ys <- input$corr_y_metric
    if (is.null(ys) || length(ys) == 0) return()
    updateSelectInput(session, "corr_plot_y", choices = ys, selected = ys[1])
  }, ignoreInit = TRUE)
  
  # ---- Compute correlations ----
  corr_result <- eventReactive(input$run_corr, {
    req(!is.null(input$corr_x_metric), nzchar(input$corr_x_metric))
    req(!is.null(input$corr_y_metric), length(input$corr_y_metric) >= 1)
    
    df <- corr_base_df()
    
    # X pool
    df_x_pool <- df
    if (!is.null(input$corr_x_source) && input$corr_x_source != "All") df_x_pool <- df_x_pool %>% filter(source == input$corr_x_source)
    if (!is.null(input$corr_x_test)   && input$corr_x_test   != "All") df_x_pool <- df_x_pool %>% filter(test_type == input$corr_x_test)
    
    # Y pool
    df_y_pool <- df
    if (!is.null(input$corr_y_source) && input$corr_y_source != "All") df_y_pool <- df_y_pool %>% filter(source == input$corr_y_source)
    if (!is.null(input$corr_y_test)   && input$corr_y_test   != "All") df_y_pool <- df_y_pool %>% filter(test_type == input$corr_y_test)
    
    # latest per player per metric_key (within each pool)
    latest_x <- df_x_pool %>%
      group_by(player_id, player_name, metric_key) %>%
      slice_max(date, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    latest_y <- df_y_pool %>%
      group_by(player_id, player_name, metric_key) %>%
      slice_max(date, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    # pick key for X metric_name (within X pool)
    x_key <- latest_x %>%
      filter(metric_name == input$corr_x_metric) %>%
      count(metric_key, sort = TRUE) %>%
      slice(1) %>%
      pull(metric_key)
    
    req(length(x_key) == 1)
    
    df_x <- latest_x %>%
      filter(metric_key == x_key) %>%
      transmute(player_id, player_name, x = suppressWarnings(as.numeric(metric_value)))
    
    y_names <- input$corr_y_metric
    
    summary <- lapply(y_names, function(y_nm) {
      y_key <- latest_y %>%
        filter(metric_name == y_nm) %>%
        count(metric_key, sort = TRUE) %>%
        slice(1) %>%
        pull(metric_key)
      
      if (length(y_key) != 1) {
        return(tibble::tibble(
          Metric_X = input$corr_x_metric,
          Metric_Y = y_nm,
          N = 0,
          Correlation = NA_real_,
          P_value = NA_real_
        ))
      }
      
      df_y <- latest_y %>%
        filter(metric_key == y_key) %>%
        transmute(player_id, player_name, y = suppressWarnings(as.numeric(metric_value)))
      
      joined <- inner_join(df_x, df_y, by = c("player_id", "player_name")) %>%
        filter(!is.na(x), !is.na(y))
      
      if (nrow(joined) < 3) {
        return(tibble::tibble(
          Metric_X = input$corr_x_metric,
          Metric_Y = y_nm,
          N = nrow(joined),
          Correlation = NA_real_,
          P_value = NA_real_
        ))
      }
      
      ct <- suppressWarnings(cor.test(joined$x, joined$y, method = "spearman"))
      
      tibble::tibble(
        Metric_X = input$corr_x_metric,
        Metric_Y = y_nm,
        N = nrow(joined),
        Correlation = unname(ct$estimate),
        P_value = ct$p.value
      )
    }) %>%
      bind_rows() %>%
      arrange(desc(abs(Correlation)))
    
    # scatter uses chosen plot Y (within selected Y list)
    y_plot_nm <- input$corr_plot_y %||% y_names[1]
    
    y_plot_key <- latest_y %>%
      filter(metric_name == y_plot_nm) %>%
      count(metric_key, sort = TRUE) %>%
      slice(1) %>%
      pull(metric_key)
    
    scatter <- tibble::tibble()
    if (length(y_plot_key) == 1) {
      df_y_plot <- latest_y %>%
        filter(metric_key == y_plot_key) %>%
        transmute(player_id, player_name, y = suppressWarnings(as.numeric(metric_value)))
      
      scatter <- inner_join(df_x, df_y_plot, by = c("player_id", "player_name")) %>%
        filter(!is.na(x), !is.na(y))
    }
    
    list(summary = summary, scatter = scatter, y_plot_nm = y_plot_nm)
  })
  
  output$corr_table <- renderDT({
    res <- corr_result()
    datatable(
      res$summary,
      rownames = FALSE,
      options = list(pageLength = 25, scrollX = TRUE)
    ) %>%
      formatRound(c("Correlation", "P_value"), 4)
  })
  
  output$corr_plot <- renderPlotly({
    res <- corr_result()
    df <- res$scatter
    if (nrow(df) == 0) return(NULL)
    
    highlight_nm <- input$corr_highlight_player
    
    p <- ggplot(df, aes(x = x, y = y, text = player_name)) +
      geom_point(size = 2, alpha = 0.7) +
      geom_vline(xintercept = median(df$x, na.rm = TRUE), linetype = "dotted") +
      geom_hline(yintercept = median(df$y, na.rm = TRUE), linetype = "dotted")
    
    # 🔴 highlighted player point (drawn on top)
    if (!is.null(highlight_nm) && highlight_nm != "None") {
      df_hi <- df %>% filter(player_name == highlight_nm)
      
      if (nrow(df_hi) == 1) {
        p <- p +
          geom_point(
            data = df_hi,
            aes(x = x, y = y),
            inherit.aes = FALSE,
            size = 4,
            color = "#DC2626"   # red-600
          )
      }
    }
    
    p <- p +
      theme_minimal(base_size = 12) +
      labs(
        x = input$corr_x_metric,
        y = res$y_plot_nm,
        title = paste0(
          "Latest snapshot scatter: ",
          res$y_plot_nm,
          " vs ",
          input$corr_x_metric
        )
      )
    
    ggplotly(p, tooltip = "text")
  })
  
}

shinyApp(ui, server)
