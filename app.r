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

source("metrics.R")

# ---------------------------
# Helpers
# ---------------------------

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
class_col <- if ("Class" %in% names(roster_view)) "Class" else NULL
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
        if (!is.null(group_col)) {
          selectInput("group_filter", "Group",
                      choices = c("All", sort(unique(na.omit(roster_view[[group_col]])))),
                      selected = "All", multiple = TRUE)
        },
        if (!is.null(pos_col)) {
          selectInput("pos_filter", "Position",
                      choices = c("All", sort(unique(na.omit(roster_view[[pos_col]])))),
                      selected = "All", multiple = TRUE)
        },
        if (!is.null(class_col)) {
          selectInput("class_filter", "Class Year",
                      choices = c("All", sort(unique(na.omit(roster_view[[class_col]])))),
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
        
        sliderInput(
          "min_pctl",
          "Min percentile",
          min = 0, max = 100, value = 0, step = 1
        ),
        
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
          width = 4,
          h4("Radar (MVP)"),
          plotOutput("radar_plot", height = 280),
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
                column(8, plotOutput("compare_radar_plot", height = 320))
              )
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
        selectInput(
          "corr_source", "Source",
          choices = c("All", sort(unique(na.omit(vald_tests_long_ui$source)))),
          selected = "All"
        ),
        
        selectInput(
          "corr_test", "Test",
          choices = "All",
          selected = "All"
        ),
        
        tags$hr(),
        
        selectInput(
          "corr_x_metric", "Metric X",
          choices = character(0)
        ),
        
        tags$hr(),
        
        fluidRow(
          column(
            8,
            selectInput(
              "corr_y_metric", "Metric Y (choose 1+)",
              choices = character(0),
              multiple = TRUE
            )
          ),
          column(
            4,
            actionButton("corr_select_all_y", "Select all")
          )
        ),
        
        selectInput(
          "corr_plot_y", "Scatterplot Y",
          choices = character(0)
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
  
  
  
  # selected player state (set by roster click)
  selected_player <- reactiveVal(roster_view$player_name[1] %||% "")
  
  observeEvent(selected_player(), {
    nm <- selected_player()
    players <- sort(unique(roster_view$player_name))
    pick <- players[players != nm][1] %||% nm
    updateSelectInput(session, "compare_player", choices = players, selected = pick)
  }, ignoreInit = TRUE)

  
  
  # ---------- Populate Performance/Trend filter dropdowns ----------
  player_pid <- reactive({
    nm <- selected_player()
    pid <- roster_view %>% filter(player_name == nm) %>% slice(1) %>% pull(player_id)
    req(length(pid) == 1)
    pid
  })
  
  perf_filtered_df <- reactive({
    df <- vald_tests_long_ui %>% filter(player_id == player_pid())
    
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
    sources <- sort(unique(na.omit(df$source)))
    
    updateSelectInput(session, "perf_source", choices = c("All", sources), selected = "All")
    updateSelectInput(session, "perf_test",   choices = "All", selected = "All")
    updateSelectInput(session, "perf_metric", choices = "All", selected = "All")
  }, ignoreInit = FALSE)
  
  observeEvent(input$perf_source, {
    df <- vald_tests_long_ui %>% filter(player_id == player_pid())
    
    if (!is.null(input$perf_source) && input$perf_source != "All") {
      df <- df %>% filter(source == input$perf_source)
    }
    
    tests <- sort(unique(na.omit(df$test_type)))
    updateSelectInput(session, "perf_test", choices = c("All", tests), selected = "All")
  }, ignoreInit = FALSE)
  
  observeEvent(c(input$perf_source, input$perf_test), {
    df <- vald_tests_long_ui %>% filter(player_id == player_pid())
    
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
    sources <- sort(unique(na.omit(df$source)))
    
    updateSelectInput(session, "trend_source", choices = c("All", sources), selected = "All")
    updateSelectInput(session, "trend_test",   choices = "All", selected = "All")
    updateSelectInput(session, "trend_metric", choices = "All", selected = "All")
  }, ignoreInit = FALSE)
  
  
  observeEvent(input$trend_source, {
    df <- vald_tests_long_ui %>% filter(player_id == player_pid())
    
    if (!is.null(input$trend_source) && input$trend_source != "All") {
      df <- df %>% filter(source == input$trend_source)
    }
    
    tests <- sort(unique(na.omit(df$test_type)))
    updateSelectInput(session, "trend_test", choices = c("All", tests), selected = "All")
  }, ignoreInit = FALSE)
  
  
  observeEvent(c(input$trend_source, input$trend_test), {
    df <- vald_tests_long_ui %>% filter(player_id == player_pid())
    
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
    
    # percentile slider filter (supports multi-selected metrics)
    mk <- input$pctl_metric
    
    if (!is.null(mk) && length(mk) > 0 && !("All" %in% mk)) {
      mk <- intersect(mk, keep_roster_metrics)
      
      if (length(mk) > 0) {
        
        ok_players <- roster_percentiles_long %>%
          filter(metric_key %in% mk) %>%
          select(player_id, metric_key, percentile) %>%
          tidyr::complete(player_id = unique(df$player_id), metric_key = mk,
                          fill = list(percentile = NA_real_)) %>%
          group_by(player_id) %>%
          summarise(
            ok = all(is.na(percentile) | percentile >= input$min_pctl),
            .groups = "drop"
          ) %>%
          filter(ok) %>%
          pull(player_id)
        
        df <- df %>% filter(player_id %in% ok_players)
      }
    }
    
    
    
    
    
    df
  })
  
  
  output$roster_table <- renderDT({
    df <- roster_filtered()

    mk_sys  <- input$pctl_system
    mk_test <- input$pctl_test
    mk_met  <- input$pctl_metric

    # start with all metrics
    metric_show <- keep_roster_metrics
    
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
    

    
    front <- c("player_name")
    if (!is.null(group_col)) front <- c(front, group_col)
    if (!is.null(pos_col))   front <- c(front, pos_col)
    if (!is.null(class_col)) front <- c(front, class_col)
    

    cols <- unique(c(front, metric_show))
    df_disp <- df %>% select(any_of(cols))

    # ---- CLEAN COLUMN NAMES FOR DISPLAY ----
    colnames(df_disp) <- vapply(
      colnames(df_disp),
      function(x) {
        if (grepl("\\|", x)) {
          sub("^.*\\|", "", x)   # keep text after final |
        } else {
          x
        }
      },
      character(1)
    )

    datatable(
      df_disp,
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        searchHighlight = TRUE
      )
    )
  })
  
  
  
  observeEvent(input$roster_table_rows_selected, {
    idx <- input$roster_table_rows_selected
    df <- roster_filtered()
    if (length(idx) == 1 && nrow(df) >= idx) {
      selected_player(df$player_name[idx])
      # user can click Player Card tab; selection persists
    }
  }, ignoreInit = TRUE)
  
  # ---------- Player banner ----------
  output$player_banner <- renderUI({
    nm <- selected_player()
    row <- roster_view %>% filter(player_name == nm) %>% slice(1)
    
    if (nrow(row) == 0) return(NULL)
    
    # optional metadata
    group_val <- if (!is.null(group_col)) row[[group_col]][1] else NA
    pos_val   <- if (!is.null(pos_col))   row[[pos_col]][1] else NA    
    class_val <- if (!is.null(class_col)) row[[class_col]][1] else NA
    headshot  <- if (!is.null(headshot_col)) row[[headshot_col]][1] else NA
    
    
    fluidRow(
      column(
        12,
        div(
          style = "padding:12px; border:1px solid #e5e7eb; border-radius:12px; margin-bottom:12px;",
          fluidRow(
            column(
              2,
              if (!is.null(headshot_col) && !is.na(headshot) && nzchar(headshot)) {
                tags$img(src = headshot, style="width:100%; border-radius:10px;")
              } else {
                div(style="width:100%; height:110px; border-radius:10px; background:#f3f4f6; display:flex; align-items:center; justify-content:center;",
                    span("No headshot"))
              }
            ),
            column(
              10,
              h3(
                nm,
                style = "border-bottom:4px solid #FFD100; padding-bottom:4px;"
              ),
              tags$div(
                style="color:#374151;",
                paste(
                  if (!is.na(group_val)) paste0("Group: ", group_val) else "",
                  if (!is.na(pos_val)) paste0(" | Pos: ", pos_val) else "",
                  if (!is.na(class_val)) paste0(" | Class: ", class_val) else "",
                  paste0(" | As-of: ", as.character(as_of_date))
                )
              )
            )
          )
        )
      )
    )
  })
  
  # ---------- Radar (MVP: NordBord avg percentile vs ForceDecks avg percentile) ----------
  
  output$radar_plot <- renderPlot({
    nm <- selected_player()
    pid <- roster_view %>% filter(player_name == nm) %>% slice(1) %>% pull(player_id)
    if (length(pid) == 0) return(NULL)
    
    df <- roster_percentiles_long %>%
      filter(player_id == pid, metric_key %in% radar_metrics) %>%
      left_join(radar_labels, by = "metric_key") %>%
      mutate(radar_label = factor(radar_label, levels = radar_labels$radar_label))
    
    if (nrow(df) < 3) {
      return(ggplot() + theme_void() + labs(title = "Not enough metrics for radar yet"))
    }
    
    ggplot(df, aes(x = radar_label, y = percentile, group = 1)) +
      geom_polygon(alpha = 0.25) +
      geom_point(size = 2) +
      coord_polar() +
      ylim(0, 100) +
      theme_minimal(base_size = 12) +
      theme(axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 9)) +
      labs(title = "Key metrics (roster percentiles)")
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
      mutate(label = paste0(metric_key, " (", round(percentile, 1), "th)")) %>%
      pull(label)
    
    # ---- Weaknesses (bottom end) ----
    weaknesses <- latest_pcts %>%
      filter(percentile <= 20) %>%            # ← adjust threshold if you want
      arrange(percentile) %>%
      slice_head(n = 5) %>%
      mutate(label = paste0(metric_key, " (", round(percentile, 1), "th)")) %>%
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
        Value  = metric_value,
        Units  = units,
        `Roster Percentile (as-of)` = RosterPercentile
      )
    
    datatable(df_out, rownames = FALSE,
              options = list(pageLength = 25, scrollX = TRUE),
              selection = "none") %>%
      formatRound(c("Value", "Roster Percentile (as-of)"), 2)
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
        title = paste(nm, "—", mk)
      ) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
  

  
  # comparisons
  
  output$compare_radar_plot <- renderPlot({
    nm2 <- input$compare_player
    if (is.null(nm2) || nm2 == "") return(NULL)
    
    pid2 <- roster_view %>%
      filter(player_name == nm2) %>%
      slice(1) %>%
      pull(player_id)
    
    if (length(pid2) == 0) return(NULL)
    
    df2 <- roster_percentiles_long %>%
      filter(player_id == pid2, metric_key %in% radar_metrics) %>%
      left_join(radar_labels, by = "metric_key") %>%
      mutate(radar_label = factor(radar_label, levels = radar_labels$radar_label))
    
    if (nrow(df2) < 3) {
      return(ggplot() + theme_void() + labs(title = "Not enough metrics for radar yet"))
    }
    
    ggplot(df2, aes(x = radar_label, y = percentile, group = 1)) +
      geom_polygon(alpha = 0.25) +
      geom_point(size = 2) +
      coord_polar() +
      ylim(0, 100) +
      theme_minimal(base_size = 12) +
      theme(
        axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9)
      ) +
      labs(title = paste("Radar —", nm2))
  })
  

  
  # ---------------------------
  # Correlations (filtered dropdowns + select-all + ggplot labels)
  # ---------------------------
  
  # Apply source/test filters
  # corr_filtered_df <- reactive({
  #   df <- vald_tests_long_ui
  #   
  #   if (!is.null(input$corr_source) && input$corr_source != "All") {
  #     df <- df %>% filter(source == input$corr_source)
  #   }
  #   if (!is.null(input$corr_test) && input$corr_test != "All") {
  #     df <- df %>% filter(test_type == input$corr_test)
  #   }
  #   
  #   df
  # })
  
  corr_filtered_df <- reactive({
    df <- vald_tests_long_ui
    
    if (!is.null(input$corr_source) && input$corr_source != "All") {
      df <- df %>% filter(source == input$corr_source)
    }
    if (!is.null(input$corr_test) && input$corr_test != "All") {
      df <- df %>% filter(test_type == input$corr_test)
    }
    
    df
  })
  
  
  # Update Test choices when Source changes
  # observeEvent(input$corr_source, {
  #   df <- corr_filtered_df()
  #   tests <- sort(unique(na.omit(df$test_type)))
  #   updateSelectInput(session, "corr_test", choices = c("All", tests), selected = "All")
  # }, ignoreInit = FALSE)
  
  observeEvent(input$corr_source, {
    df <- vald_tests_long_ui
    
    if (input$corr_source != "All") {
      df <- df %>% filter(source == input$corr_source)
    }
    
    tests <- sort(unique(na.omit(df$test_type)))
    
    updateSelectInput(
      session,
      "corr_test",
      choices = c("All", tests),
      selected = "All"   # 🔑 THIS IS THE KEY LINE
    )
  }, ignoreInit = FALSE)
  

  
  # Update Metric X / Metric Y choices when Source or Test changes
  observeEvent(c(input$corr_source, input$corr_test), {
    df <- corr_filtered_df()
    
    metrics <- sort(unique(na.omit(df$metric_name)))
    metrics <- metrics[nzchar(metrics)]
    
    updateSelectInput(session, "corr_x_metric", choices = metrics, selected = metrics[1] %||% "")
    updateSelectInput(session, "corr_y_metric", choices = metrics, selected = metrics[2] %||% metrics[1] %||% "")
  }, ignoreInit = FALSE)
  
  # Select-all button for Metric Y
  observeEvent(input$corr_select_all_y, {
    df <- corr_filtered_df()
    metrics <- sort(unique(na.omit(df$metric_name)))
    metrics <- metrics[nzchar(metrics)]
    updateSelectInput(session, "corr_y_metric", selected = metrics)
  }, ignoreInit = TRUE)
  
  # Sync scatterplot Y choices to currently-selected Y list
  observeEvent(input$corr_y_metric, {
    ys <- input$corr_y_metric
    if (is.null(ys) || length(ys) == 0) return()
    updateSelectInput(session, "corr_plot_y", choices = ys, selected = ys[1])
  }, ignoreInit = TRUE)
  
  # Compute correlations (Spearman only) using LATEST snapshot per player per metric_key
  corr_result <- eventReactive(input$run_corr, {
    req(!is.null(input$corr_x_metric), nzchar(input$corr_x_metric))
    req(!is.null(input$corr_y_metric), length(input$corr_y_metric) >= 1)
    
    df_base <- corr_filtered_df() %>%
      filter(date <= as_of_date) %>%
      mutate(metric_key = paste(source, test_type, metric_name, sep = "|")) %>%
      group_by(player_id, player_name, metric_key) %>%
      slice_max(date, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    # Metric X: pick the most common metric_key for that metric_name (important if All/All)
    x_candidates <- df_base %>% filter(metric_name == input$corr_x_metric)
    
    x_key <- x_candidates %>%
      count(metric_key, sort = TRUE) %>%
      slice(1) %>%
      pull(metric_key)
    
    req(length(x_key) == 1)
    
    df_x <- df_base %>%
      filter(metric_key == x_key) %>%
      select(player_id, player_name, x = metric_value)
    
    y_names <- input$corr_y_metric
    
    summary <- lapply(y_names, function(y_nm) {
      y_candidates <- df_base %>% filter(metric_name == y_nm)
      
      y_key <- y_candidates %>%
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
      
      df_y <- df_base %>%
        filter(metric_key == y_key) %>%
        select(player_id, player_name, y = metric_value)
      
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
    
    # Scatter: use chosen Y metric_name (must be within selected Y list)
    y_plot_nm <- input$corr_plot_y %||% y_names[1]
    
    y_plot_key <- df_base %>%
      filter(metric_name == y_plot_nm) %>%
      count(metric_key, sort = TRUE) %>%
      slice(1) %>%
      pull(metric_key)
    
    scatter <- tibble::tibble()
    if (length(y_plot_key) == 1) {
      df_y_plot <- df_base %>%
        filter(metric_key == y_plot_key) %>%
        select(player_id, player_name, y = metric_value)
      
      scatter <- inner_join(df_x, df_y_plot, by = c("player_id", "player_name")) %>%
        filter(!is.na(x), !is.na(y))
    }
    
    list(
      summary = summary,
      scatter = scatter,
      y_plot_nm = y_plot_nm
    )
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
    
    p <- ggplot(df, aes(x = x, y = y, text = player_name)) +
      geom_point(size = 2) +
      theme_minimal(base_size = 12) +
      labs(
        x = input$corr_x_metric,
        y = res$y_plot_nm,
        title = "Latest snapshot scatter (hover for player)"
      )
    
    ggplotly(p, tooltip = "text")
  })
  

}

shinyApp(ui, server)
