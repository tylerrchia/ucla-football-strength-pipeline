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
  slug <- player_photo_slug(player_name)
  if (is.na(slug) || !nzchar(slug)) return(list(url = NA_character_, alt_text = NA_character_))
  
  view <- match.arg(view)
  for (ext in exts) {
    fn <- paste0(slug, "_", view, ".", ext)
    if (file.exists(file.path("www", base_url_prefix, fn))) {
      return(list(url = paste0(base_url_prefix, "/", fn), alt_text = player_name))
    }
  }
  list(url = NA_character_, alt_text = NA_character_)
}

get_label <- function(metric_name) {
  if (metric_name == "rel_pwr") return("Relative Power")
  if (metric_name == "abs_pwr") return("Absolute Power")
  if (metric_name == "asym") return("Asymmetry")
  if (metric_name == "expl_def") return("Explosive Definition (ED)")
  if (metric_name == "rfd_30ms") return("RFD 30ms")
  if (metric_name == "rfd_100ms") return("RFD 100ms")
  if (metric_name == "rfd_200ms") return("RFD 200ms")
  if (metric_name == "adr_vel") return("Average Deceleration Rate (Velocity)")
  if (metric_name == "adr_force") return("Average Deceleration Rate (Force)")
  if (metric_name == "brake_force") return("Brake Force")
  if (metric_name == "peak_force") return("Peak Force")
  if (metric_name == "peak_vel") return("Peak Velocity")
  if (metric_name == "col_index") return("Collision Index")
  if (metric_name == "cvat_l") return("Countermovement (Left)")
  if (metric_name == "cvat_r") return("Countermovement (Right)")
  if (metric_name == "dj_l") return("Drop Jump (Left)")
  if (metric_name == "dj_r") return("Drop Jump (Right)")
  if (metric_name == "comp_score") return("Comp Score")
  if (metric_name == "grf_t") return("GRF Time")
  if (metric_name == "grf_imp") return("GRF Impulse")
  if (metric_name == "grf_rate") return("GRF Rate")
  if (metric_name == "height_l") return("Height (Left)")
  if (metric_name == "height_r") return("Height (Right)")
  if (metric_name == "con_time") return("Contraction Time")
  if (metric_name == "rfd") return("RFD")
  if (metric_name == "rsi") return("Reactive Strength Index")
  if (metric_name == "contact_time") return("Contact Time")
  if (metric_name == "flight_time") return("Flight Time")
  if (metric_name == "eccentric_impulse") return("Eccentric Impulse")
  if (metric_name == "conc_impulse") return("Concentric Impulse")
  if (metric_name == "reactive_str_idx") return("Reactive Strength Index")
  if (metric_name == "vel_at_peak_force") return("Velocity at Peak Force")
  if (metric_name == "peak_accel") return("Peak Acceleration")
  if (metric_name == "ecc_load") return("Eccentric Load")
  if (metric_name == "con_load") return("Concentric Load")
  if (metric_name == "total_load") return("Total Load")
  if (metric_name == "velocity") return("Velocity")
  if (metric_name == "strength_index") return("Strength Index")
  metric_name
}

color_for_z_score <- function(z) {
  if (is.na(z)) return("#999999")
  if (z > 2)    return("#065f46")
  if (z > 1)    return("#10b981")
  if (z > 0.5)  return("#a7f3d0")
  if (z < -2)   return("#7f1d1d")
  if (z < -1)   return("#dc2626")
  if (z < -0.5) return("#fca5a5")
  "#f3f4f6"
}

# UI
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    primary = "#1a56db",
    secondary = "#7c3aed",
    success = "#10b981",
    danger = "#dc2626",
    warning = "#f59e0b",
    info = "#0891b2"
  ),
  
  # Global CSS
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background-color: #f9fafb;
      }
      .navbar { background-color: #1a56db !important; }
      .navbar-brand, .nav-link { color: white !important; }
      .nav-link:hover { color: #dbeafe !important; }
      .card {
        border: 1px solid #e5e7eb;
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
      }
      .card-header {
        background-color: #f3f4f6;
        border-bottom: 1px solid #e5e7eb;
      }
      .badge {
        padding: 0.4em 0.6em;
      }
      .player-card {
        border: 2px solid #1a56db;
        border-radius: 8px;
        padding: 15px;
        background-color: white;
        margin-bottom: 15px;
      }
      .player-name {
        font-size: 1.25rem;
        font-weight: bold;
        color: #1a56db;
        margin-bottom: 10px;
      }
      .player-position {
        font-size: 0.9rem;
        color: #6b7280;
        margin-bottom: 10px;
      }
      .metric-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 10px;
        margin-top: 10px;
      }
      .metric-item {
        background-color: #f9fafb;
        padding: 8px;
        border-radius: 4px;
        border-left: 3px solid #1a56db;
      }
      .metric-label {
        font-size: 0.8rem;
        color: #6b7280;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      .metric-value {
        font-size: 1.1rem;
        font-weight: bold;
        color: #1f2937;
        margin-top: 4px;
      }
      .metric-zscore {
        font-size: 0.85rem;
        color: #6b7280;
        margin-top: 2px;
      }
      .headshot {
        max-width: 150px;
        height: auto;
        border-radius: 8px;
        border: 2px solid #1a56db;
        margin-bottom: 10px;
      }
      .photo {
        max-width: 100%;
        height: auto;
        border-radius: 8px;
        margin-bottom: 10px;
      }
      .roster-table {
        font-size: 0.9rem;
      }
      .roster-table tbody tr:hover {
        background-color: #f0f9ff;
      }
      .btn-primary {
        background-color: #1a56db;
        border-color: #1a56db;
      }
      .btn-primary:hover {
        background-color: #1e40af;
        border-color: #1e40af;
      }
      .nav-tabs .nav-link.active {
        border-color: #1a56db;
        color: #1a56db;
      }
      .nav-tabs .nav-link {
        color: #6b7280;
      }
      .nav-tabs .nav-link:hover {
        color: #1a56db;
      }
      h3 {
        color: #1f2937;
        margin-top: 20px;
        margin-bottom: 10px;
      }
    "))
  ),
  
  navbarPage(
    title = "VALD Roster Explorer",
    
    # Roster tab
    tabPanel(
      "Roster",
      fluidRow(
        column(12,
          h3("UCLA Football - Player Roster"),
          DTOutput("roster_table")
        )
      )
    ),
    
    # Player Card tab
    tabPanel(
      "Player Card",
      fluidRow(
        column(12,
          h3("Player Selection"),
          selectInput("player_select", "Select Player:", choices = c())
        )
      ),
      fluidRow(
        column(4,
          div(id = "player_card_left",
            uiOutput("player_card_left")
          )
        ),
        column(8,
          div(id = "player_card_right",
            uiOutput("player_card_right")
          )
        )
      )
    ),
    
    # Correlations tab
    tabPanel(
      "Correlations",
      fluidRow(
        column(12,
          h3("Metric Correlations")
        )
      ),
      fluidRow(
        column(3,
          selectInput("corr_y_metric", "Y-axis Metric:", choices = c())
        ),
        column(3,
          selectInput("corr_x_metric", "X-axis Metric:", choices = c())
        ),
        column(3,
          selectInput("corr_position", "Position Filter:", choices = c("All", "QB", "RB", "WR", "TE", "OL", "DL", "LB", "DB"))
        ),
        column(3,
          checkboxInput("corr_highlight_position", "Highlight Position")
        )
      ),
      fluidRow(
        column(12,
          plotlyOutput("corr_plot", height = "600px")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Build player choices
  observe({
    player_list <- sort(unique(df_latest$Player))
    updateSelectInput(session, "player_select", choices = player_list)
    updateSelectInput(session, "corr_y_metric", choices = sort(metric_cols))
    updateSelectInput(session, "corr_x_metric", choices = sort(metric_cols))
  })
  
  # Roster table
  output$roster_table <- renderDT({
    df_display <- df_latest %>%
      select(Player, Position, Class, Height, Weight, one_of(metric_cols)) %>%
      mutate(across(all_of(metric_cols), ~round(., 2)))
    
    datatable(
      df_display,
      options = list(
        pageLength = 15,
        dom = "ltip",
        columnDefs = list(
          list(targets = 2:ncol(df_display)-1, className = "dt-right")
        )
      ),
      rownames = FALSE,
      class = "roster-table"
    )
  })
  
  # Player card - Left column (info + headshot/photos)
  output$player_card_left <- renderUI({
    player <- input$player_select
    if (is.null(player) || !nzchar(player)) return(NULL)
    
    row <- df_latest %>% filter(Player == player)
    if (nrow(row) == 0) return(NULL)
    
    row <- row[1, ]
    headshot <- find_player_headshot(player)
    photo_front <- find_player_photo(player, "front")
    photo_side <- find_player_photo(player, "side")
    photo_back <- find_player_photo(player, "back")
    
    div(class = "player-card",
      div(class = "player-name", player),
      div(class = "player-position", paste(row$Position, "-", row$Class)),
      p(sprintf("Height: %s | Weight: %s lbs", row$Height, row$Weight)),
      
      if (!is.na(headshot)) {
        img(src = headshot, alt = player, class = "headshot")
      },
      
      if (!is.na(photo_front$url)) {
        h4("Front"),
        img(src = photo_front$url, alt = photo_front$alt_text, class = "photo")
      },
      
      if (!is.na(photo_side$url)) {
        h4("Side"),
        img(src = photo_side$url, alt = photo_side$alt_text, class = "photo")
      },
      
      if (!is.na(photo_back$url)) {
        h4("Back"),
        img(src = photo_back$url, alt = photo_back$alt_text, class = "photo")
      }
    )
  })
  
  # Player card - Right column (metrics)
  output$player_card_right <- renderUI({
    player <- input$player_select
    if (is.null(player) || !nzchar(player)) return(NULL)
    
    row <- df_latest %>% filter(Player == player)
    if (nrow(row) == 0) return(NULL)
    
    row <- row[1, ]
    
    metric_items <- lapply(metric_cols, function(col) {
      val <- row[[col]]
      if (is.na(val)) {
        z_score <- NA_real_
      } else {
        mean_val <- mean(df_latest[[col]], na.rm = TRUE)
        sd_val <- sd(df_latest[[col]], na.rm = TRUE)
        z_score <- if (sd_val == 0) 0 else (val - mean_val) / sd_val
      }
      
      bg_color <- color_for_z_score(z_score)
      
      div(class = "metric-item",
        style = paste0("border-left-color: ", bg_color, ";"),
        div(class = "metric-label", get_label(col)),
        div(class = "metric-value", if (is.na(val)) "N/A" else sprintf("%.2f", val)),
        if (!is.na(z_score)) {
          div(class = "metric-zscore", sprintf("Z: %.2f", z_score))
        }
      )
    })
    
    div(class = "metric-grid",
      metric_items
    )
  })
  
  # Correlations scatter plot
  output$corr_plot <- renderPlotly({
    y_metric <- input$corr_y_metric
    x_metric <- input$corr_x_metric
    
    if (is.null(y_metric) || is.null(x_metric) || !nzchar(y_metric) || !nzchar(x_metric)) {
      return(ggplotly(ggplot() + theme_minimal() + ggtitle("Select metrics to view")))
    }
    
    if (y_metric == x_metric) {
      return(ggplotly(ggplot() + theme_minimal() + ggtitle("Select different metrics")))
    }
    
    df_plot <- df_latest %>%
      filter(!is.na(.data[[y_metric]]), !is.na(.data[[x_metric]])) %>%
      mutate(x = .data[[x_metric]], y = .data[[y_metric]])
    
    if (nrow(df_plot) == 0) {
      return(ggplotly(ggplot() + theme_minimal() + ggtitle("No data available")))
    }
    
    res <- list(
      y_plot_nm = get_label(y_metric),
      x_plot_nm = get_label(x_metric)
    )
    
    # Compute correlation
    corr_val <- cor(df_plot$x, df_plot$y, use = "complete.obs")
    
    df_plot <- df_plot %>%
      mutate(
        text = sprintf("%s<br>%s: %.2f<br>%s: %.2f",
          Player, res$x_plot_nm, x, res$y_plot_nm, y)
      )
    
    p <- ggplot(df_plot, aes(x = x, y = y, text = text)) +
      geom_point(size = 3, alpha = 0.6, color = "#1a56db") +
      geom_smooth(method = "lm", se = TRUE, color = "#7c3aed", alpha = 0.2, fill = "#e9d5ff") +
      theme_minimal(base_size = 12) +
      labs(
        x = res$x_plot_nm,
        y = res$y_plot_nm,
        title = sprintf("Correlation: %.3f", corr_val)
      )
    
    # Highlight position if requested
    if (input$corr_highlight_position) {
      pos <- input$corr_position
      if (pos != "All") {
        df_hi <- df_plot %>% filter(Position == pos)
        if (nrow(df_hi) == 1) p <- p + geom_point(data = df_hi, aes(x = x, y = y), inherit.aes = FALSE, size = 4, color = "#DC2626")
      }
    }
    p <- p + theme_minimal(base_size = 12) +
      labs(x = input$corr_x_metric, y = res$y_plot_nm, title = paste0("Latest snapshot scatter: ", res$y_plot_nm, " vs ", input$corr_x_metric))
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)