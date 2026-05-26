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
  if (is.na(slug) || !nzchar(slug)) return(NA_character_)
  for (ext in exts) {
    fn <- paste0(slug, "_", view, ".", ext)
    if (file.exists(file.path("www", base_url_prefix, fn))) {
      return(paste0(base_url_prefix, "/", fn))
    }
  }
  NA_character_
}

# ---------------------------
# Derived metrics / filters
# ---------------------------

gen_power_score <- function(df, scale_0_100 = TRUE) {
  df$POWER_SCORE <- 0.25 * scale(df$REL_HIP_POWER, center=TRUE, scale=TRUE) +
                    0.25 * scale(df$REL_LEG_FORCE, center=TRUE, scale=TRUE) +
                    0.25 * scale(df$REL_LEG_BALANCE, center=TRUE, scale=TRUE) +
                    0.25 * scale(df$REL_LEG_SYMMETRY, center=TRUE, scale=TRUE)
  if (scale_0_100) df$POWER_SCORE <- 50 + 10*df$POWER_SCORE
  df
}

gen_athlete_score <- function(df, scale_0_100 = TRUE) {
  df$ATHLETE_SCORE <- 0.3 * scale(df$REL_SQUAT, center=TRUE, scale=TRUE) +
                      0.3 * scale(df$REL_COUNTERMOVEMENT_JUMP, center=TRUE, scale=TRUE) +
                      0.2 * scale(df$REL_SPRINT_10M, center=TRUE, scale=TRUE) +
                      0.2 * scale(df$REL_CHANGE_OF_DIRECTION, center=TRUE, scale=TRUE)
  if (scale_0_100) df$ATHLETE_SCORE <- 50 + 10*df$ATHLETE_SCORE
  df
}

gen_movement_score <- function(df, scale_0_100 = TRUE) {
  df$MOVEMENT_SCORE <- 0.25 * scale(df$REL_SQUAT, center=TRUE, scale=TRUE) +
                       0.25 * scale(df$REL_SQUAT_DEPTH_TIME, center=TRUE, scale=TRUE) +
                       0.25 * scale(df$REL_SINGLE_LEG_HOP_BALANCE, center=TRUE, scale=TRUE) +
                       0.25 * scale(df$REL_Y_BALANCE_COMPOSITE, center=TRUE, scale=TRUE)
  if (scale_0_100) df$MOVEMENT_SCORE <- 50 + 10*df$MOVEMENT_SCORE
  df
}

gen_stability_score <- function(df, scale_0_100 = TRUE) {
  df$STABILITY_SCORE <- 0.5 * scale(df$REL_LEG_BALANCE, center=TRUE, scale=TRUE) +
                        0.5 * scale(df$REL_Y_BALANCE_COMPOSITE, center=TRUE, scale=TRUE)
  if (scale_0_100) df$STABILITY_SCORE <- 50 + 10*df$STABILITY_SCORE
  df
}

gen_force_score <- function(df, scale_0_100 = TRUE) {
  df$FORCE_SCORE <- 0.5 * scale(df$REL_LEG_FORCE, center=TRUE, scale=TRUE) +
                    0.5 * scale(df$REL_SQUAT, center=TRUE, scale=TRUE)
  if (scale_0_100) df$FORCE_SCORE <- 50 + 10*df$FORCE_SCORE
  df
}

gen_speed_score <- function(df, scale_0_100 = TRUE) {
  df$SPEED_SCORE <- 0.5 * scale(df$REL_SPRINT_10M, center=TRUE, scale=TRUE) +
                    0.5 * scale(df$REL_CHANGE_OF_DIRECTION, center=TRUE, scale=TRUE)
  if (scale_0_100) df$SPEED_SCORE <- 50 + 10*df$SPEED_SCORE
  df
}

gen_reactive_score <- function(df, scale_0_100 = TRUE) {
  df$REACTIVE_SCORE <- scale(df$REL_COUNTERMOVEMENT_JUMP, center=TRUE, scale=TRUE)
  if (scale_0_100) df$REACTIVE_SCORE <- 50 + 10*df$REACTIVE_SCORE
  df
}

gen_power_endurance_score <- function(df, scale_0_100 = TRUE) {
  df$POWER_ENDURANCE_SCORE <- 0.5 * scale(df$REL_REPEATED_JUMP_INDEX, center=TRUE, scale=TRUE) +
                              0.5 * scale(df$REL_LEG_BALANCE, center=TRUE, scale=TRUE)
  if (scale_0_100) df$POWER_ENDURANCE_SCORE <- 50 + 10*df$POWER_ENDURANCE_SCORE
  df
}

# ---------------------------
# Data loading
# ---------------------------

# Load processed data
load("data/processed_vald_data.rda")  # loads `vald_data`

# Add composite scores
vald_data <- gen_power_score(vald_data)
vald_data <- gen_athlete_score(vald_data)
vald_data <- gen_movement_score(vald_data)
vald_data <- gen_stability_score(vald_data)
vald_data <- gen_force_score(vald_data)
vald_data <- gen_speed_score(vald_data)
vald_data <- gen_reactive_score(vald_data)
vald_data <- gen_power_endurance_score(vald_data)

# ---------------------------
# Shiny UI
# ---------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
      .navbar { background-color: #003d7a; }
      .navbar-default .navbar-brand { color: #ffffff !important; }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus { background-color: #f5f5f5; border-color: #ddd #ddd transparent; }
      .player-card { border: 1px solid #ddd; border-radius: 8px; padding: 15px; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      .metric-box { background-color: #f9f9f9; border-left: 4px solid #003d7a; padding: 10px; margin: 8px 0; border-radius: 4px; }
      .metric-label { font-weight: bold; color: #333; font-size: 0.9em; }
      .metric-value { font-size: 1.2em; color: #003d7a; font-weight: bold; }
      .scatter-info { background-color: #e8f4f8; padding: 10px; border-radius: 4px; margin: 10px 0; }
      .table-container { overflow-x: auto; }
      .player-photo { max-width: 100%; height: auto; border-radius: 8px; margin: 10px 0; }
    "))
  ),
  navbarPage(
    title = "UCLA Football VALD Strength Metrics",
    theme = bslib::bs_theme(version = 4, primary = "#003d7a"),
    tabPanel(
      "Roster Explorer",
      sidebarLayout(
        sidebarPanel(
          h3("Filters & Search"),
          textInput("player_search", "Player Name (partial match)", ""),
          selectInput("filter_position", "Position", c("All", sort(unique(na.omit(vald_data$POSITION))))),
          selectInput("filter_year", "Year/Class", c("All", sort(unique(na.omit(vald_data$YEAR))))),
          sliderInput("filter_height", "Height (in)", min=0, max=84, value=c(0,84)),
          sliderInput("filter_weight", "Weight (lbs)", min=0, max=350, value=c(0,350)),
          hr(),
          h4("Sort By"),
          selectInput("sort_metric", "Metric",
                      c("Name"="FULL_NAME",
                        "Power Score"="POWER_SCORE",
                        "Athlete Score"="ATHLETE_SCORE",
                        "Movement Score"="MOVEMENT_SCORE",
                        "Stability Score"="STABILITY_SCORE",
                        "Force Score"="FORCE_SCORE",
                        "Speed Score"="SPEED_SCORE",
                        "Reactive Score"="REACTIVE_SCORE",
                        "Power Endurance Score"="POWER_ENDURANCE_SCORE")),
          selectInput("sort_order", "Order", c("Ascending"="asc", "Descending"="desc")),
          width = 3
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Table View",
              div(class="table-container",
                  DT::dataTableOutput("roster_table")
              )
            ),
            tabPanel(
              "Individual Player",
              uiOutput("player_selector_ui"),
              uiOutput("player_detail_ui")
            )
          ),
          width = 9
        )
      )
    ),
    tabPanel(
      "Metric Correlations",
      sidebarLayout(
        sidebarPanel(
          selectInput("corr_metric_x", "X-Axis Metric",
                      c("Squat" = "REL_SQUAT",
                        "Countermovement Jump" = "REL_COUNTERMOVEMENT_JUMP",
                        "Hop Balance" = "REL_SINGLE_LEG_HOP_BALANCE",
                        "Y-Balance" = "REL_Y_BALANCE_COMPOSITE",
                        "Hip Power" = "REL_HIP_POWER",
                        "Leg Force" = "REL_LEG_FORCE",
                        "Leg Balance" = "REL_LEG_BALANCE",
                        "Leg Symmetry" = "REL_LEG_SYMMETRY",
                        "Sprint 10M" = "REL_SPRINT_10M",
                        "Change of Direction" = "REL_CHANGE_OF_DIRECTION",
                        "Repeated Jump Index" = "REL_REPEATED_JUMP_INDEX",
                        "Squat Depth Time" = "REL_SQUAT_DEPTH_TIME")),
          selectInput("corr_metric_y", "Y-Axis Metric",
                      c("Squat" = "REL_SQUAT",
                        "Countermovement Jump" = "REL_COUNTERMOVEMENT_JUMP",
                        "Hop Balance" = "REL_SINGLE_LEG_HOP_BALANCE",
                        "Y-Balance" = "REL_Y_BALANCE_COMPOSITE",
                        "Hip Power" = "REL_HIP_POWER",
                        "Leg Force" = "REL_LEG_FORCE",
                        "Leg Balance" = "REL_LEG_BALANCE",
                        "Leg Symmetry" = "REL_LEG_SYMMETRY",
                        "Sprint 10M" = "REL_SPRINT_10M",
                        "Change of Direction" = "REL_CHANGE_OF_DIRECTION",
                        "Repeated Jump Index" = "REL_REPEATED_JUMP_INDEX",
                        "Squat Depth Time" = "REL_SQUAT_DEPTH_TIME"),
                      selected = "REL_COUNTERMOVEMENT_JUMP"),
          selectInput("corr_position", "Position Filter", c("All", sort(unique(na.omit(vald_data$POSITION))))),
          hr(),
          h4("Correlation Info"),
          textOutput("correlation_value"),
          width = 3
        ),
        mainPanel(
          plotly::plotlyOutput("correlation_plot", height="600px"),
          width = 9
        )
      )
    ),
    tabPanel(
      "About",
      fluidRow(
        column(
          12,
          h2("UCLA Football VALD Analysis"),
          p("This dashboard provides an interactive exploration of VALD (Velocity-Based Automatic Load Detection) strength metrics collected from UCLA Football athletes."),
          h3("Available Metrics"),
          p("The following metrics are available for analysis:"),
          tags$ul(
            tags$li("Squat (Relative)"),
            tags$li("Countermovement Jump (Relative)"),
            tags$li("Single Leg Hop Balance (Relative)"),
            tags$li("Y-Balance Test Composite (Relative)"),
            tags$li("Hip Power (Relative)"),
            tags$li("Leg Force (Relative)"),
            tags$li("Leg Balance (Relative)"),
            tags$li("Leg Symmetry (Relative)"),
            tags$li("Sprint 10M (Relative)"),
            tags$li("Change of Direction (Relative)"),
            tags$li("Repeated Jump Index (Relative)"),
            tags$li("Squat Depth Time (Relative)")
          ),
          h3("Composite Scores"),
          p("The dashboard calculates the following composite scores based on selected metrics:"),
          tags$ul(
            tags$li("Power Score: 25% Hip Power + 25% Leg Force + 25% Leg Balance + 25% Leg Symmetry"),
            tags$li("Athlete Score: 30% Squat + 30% Countermovement Jump + 20% Sprint 10M + 20% Change of Direction"),
            tags$li("Movement Score: 25% Squat + 25% Squat Depth Time + 25% Single Leg Hop Balance + 25% Y-Balance"),
            tags$li("Stability Score: 50% Leg Balance + 50% Y-Balance"),
            tags$li("Force Score: 50% Leg Force + 50% Squat"),
            tags$li("Speed Score: 50% Sprint 10M + 50% Change of Direction"),
            tags$li("Reactive Score: 100% Countermovement Jump"),
            tags$li("Power Endurance Score: 50% Repeated Jump Index + 50% Leg Balance")
          ),
          h3("Data Source"),
          p("Data is sourced from VALD testing of UCLA Football athletes during the 2023-2024 season.")
        )
      )
    )
  )
)

# ---------------------------
# Shiny Server
# ---------------------------

server <- function(input, output, session) {

  # Reactive data filtering
  filtered_data <- reactive({
    df <- vald_data
    
    # Text search
    if (nzchar(input$player_search)) {
      pattern <- tolower(input$player_search)
      df <- df[grepl(pattern, tolower(df$FULL_NAME), ignore.case=TRUE), ]
    }
    
    # Position filter
    if (input$filter_position != "All") {
      df <- df[df$POSITION == input$filter_position, ]
    }
    
    # Year filter
    if (input$filter_year != "All") {
      df <- df[df$YEAR == input$filter_year, ]
    }
    
    # Height filter
    df <- df[df$HEIGHT >= input$filter_height[1] & df$HEIGHT <= input$filter_height[2], ]
    
    # Weight filter
    df <- df[df$WEIGHT >= input$filter_weight[1] & df$WEIGHT <= input$filter_weight[2], ]
    
    df
  })

  # Sort and display roster table
  output$roster_table <- DT::renderDataTable({
    df <- filtered_data()
    
    # Sort
    sort_col <- input$sort_metric
    if (nrow(df) > 0) {
      if (input$sort_order == "asc") {
        df <- df[order(df[[sort_col]], na.last=TRUE), ]
      } else {
        df <- df[order(df[[sort_col]], na.last=TRUE, decreasing=TRUE), ]
      }
    }
    
    # Select columns for display
    display_cols <- c("FULL_NAME", "POSITION", "YEAR", "HEIGHT", "WEIGHT",
                      "POWER_SCORE", "ATHLETE_SCORE", "MOVEMENT_SCORE", "STABILITY_SCORE",
                      "FORCE_SCORE", "SPEED_SCORE", "REACTIVE_SCORE", "POWER_ENDURANCE_SCORE")
    
    display_df <- df[, display_cols, drop=FALSE]
    
    # Format numeric columns to 1 decimal
    for (col in display_cols) {
      if (is.numeric(display_df[[col]])) {
        display_df[[col]] <- round(display_df[[col]], 1)
      }
    }
    
    DT::datatable(
      display_df,
      colnames = c("Name", "Position", "Year", "Height", "Weight",
                   "Power", "Athlete", "Movement", "Stability",
                   "Force", "Speed", "Reactive", "Power Endurance"),
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })

  # Player detail UI
  output$player_selector_ui <- renderUI({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(p("No players match the current filters."))
    }
    selectInput("selected_player", "Select Player", choices = df$FULL_NAME)
  })

  output$player_detail_ui <- renderUI({
    if (is.null(input$selected_player) || !nzchar(input$selected_player)) {
      return(div())
    }
    
    player <- vald_data[vald_data$FULL_NAME == input$selected_player, ]
    if (nrow(player) == 0) {
      return(p("Player not found."))
    }
    
    player <- player[1, ]
    
    # Build player card
    card_content <- list(
      div(class="player-card",
          fluidRow(
            column(
              4,
              img(src = find_player_headshot(player$FULL_NAME), style="max-width:100%; border-radius:8px;"),
              img(src = find_player_photo(player$FULL_NAME, "front"), class="player-photo")
            ),
            column(
              8,
              h2(player$FULL_NAME),
              div(class="metric-box",
                  span(class="metric-label", "Position: "),
                  span(class="metric-value", player$POSITION)
              ),
              div(class="metric-box",
                  span(class="metric-label", "Year: "),
                  span(class="metric-value", player$YEAR)
              ),
              div(class="metric-box",
                  span(class="metric-label", "Height: "),
                  span(class="metric-value", paste0(player$HEIGHT, " in"))
              ),
              div(class="metric-box",
                  span(class="metric-label", "Weight: "),
                  span(class="metric-value", paste0(player$WEIGHT, " lbs"))
              )
            )
          ),
          fluidRow(
            column(
              6,
              h3("Composite Scores"),
              div(class="metric-box",
                  span(class="metric-label", "Power Score: "),
                  span(class="metric-value", round(player$POWER_SCORE, 1))
              ),
              div(class="metric-box",
                  span(class="metric-label", "Athlete Score: "),
                  span(class="metric-value", round(player$ATHLETE_SCORE, 1))
              ),
              div(class="metric-box",
                  span(class="metric-label", "Movement Score: "),
                  span(class="metric-value", round(player$MOVEMENT_SCORE, 1))
              ),
              div(class="metric-box",
                  span(class="metric-label", "Stability Score: "),
                  span(class="metric-value", round(player$STABILITY_SCORE, 1))
              ),
              div(class="metric-box",
                  span(class="metric-label", "Force Score: "),
                  span(class="metric-value", round(player$FORCE_SCORE, 1))
              ),
              div(class="metric-box",
                  span(class="metric-label", "Speed Score: "),
                  span(class="metric-value", round(player$SPEED_SCORE, 1))
              ),
              div(class="metric-box",
                  span(class="metric-label", "Reactive Score: "),
                  span(class="metric-value", round(player$REACTIVE_SCORE, 1))
              ),
              div(class="metric-box",
                  span(class="metric-label", "Power Endurance Score: "),
                  span(class="metric-value", round(player$POWER_ENDURANCE_SCORE, 1))
              )
            ),
            column(
              6,
              h3("Raw Metrics"),
              div(class="metric-box",
                  span(class="metric-label", "Squat: "),
                  span(class="metric-value", round(player$REL_SQUAT, 2))
              ),
              div(class="metric-box",
                  span(class="metric-label", "Countermovement Jump: "),
                  span(class="metric-value", round(player$REL_COUNTERMOVEMENT_JUMP, 2))
              ),
              div(class="metric-box",
                  span(class="metric-label", "Hop Balance: "),
                  span(class="metric-value", round(player$REL_SINGLE_LEG_HOP_BALANCE, 2))
              ),
              div(class="metric-box",
                  span(class="metric-label", "Y-Balance: "),
                  span(class="metric-value", round(player$REL_Y_BALANCE_COMPOSITE, 2))
              ),
              div(class="metric-box",
                  span(class="metric-label", "Hip Power: "),
                  span(class="metric-value", round(player$REL_HIP_POWER, 2))
              ),
              div(class="metric-box",
                  span(class="metric-label", "Leg Force: "),
                  span(class="metric-value", round(player$REL_LEG_FORCE, 2))
              ),
              div(class="metric-box",
                  span(class="metric-label", "Leg Balance: "),
                  span(class="metric-value", round(player$REL_LEG_BALANCE, 2))
              ),
              div(class="metric-box",
                  span(class="metric-label", "Leg Symmetry: "),
                  span(class="metric-value", round(player$REL_LEG_SYMMETRY, 2))
              ),
              div(class="metric-box",
                  span(class="metric-label", "Sprint 10M: "),
                  span(class="metric-value", round(player$REL_SPRINT_10M, 2))
              ),
              div(class="metric-box",
                  span(class="metric-label", "Change of Direction: "),
                  span(class="metric-value", round(player$REL_CHANGE_OF_DIRECTION, 2))
              ),
              div(class="metric-box",
                  span(class="metric-label", "Repeated Jump Index: "),
                  span(class="metric-value", round(player$REL_REPEATED_JUMP_INDEX, 2))
              ),
              div(class="metric-box",
                  span(class="metric-label", "Squat Depth Time: "),
                  span(class="metric-value", round(player$REL_SQUAT_DEPTH_TIME, 2))
              )
            )
          )
      )
    )
    
    card_content
  })

  # Correlation plot
  output$correlation_plot <- plotly::renderPlotly({
    x_metric <- input$corr_metric_x
    y_metric <- input$corr_metric_y
    
    df <- vald_data
    
    # Position filter
    if (input$corr_position != "All") {
      df <- df[df$POSITION == input$corr_position, ]
    }
    
    # Remove NAs
    df <- df[!is.na(df[[x_metric]]) & !is.na(df[[y_metric]]), ]
    
    if (nrow(df) == 0) {
      return(plotly::plot_ly() %>% plotly::add_text(text="No data available for selected filters"))
    }
    
    # Calculate correlation
    corr <- cor(df[[x_metric]], df[[y_metric]], use="complete.obs")
    
    # Get metric names
    x_name <- names(grep(x_metric, c("Squat" = "REL_SQUAT",
                                     "Countermovement Jump" = "REL_COUNTERMOVEMENT_JUMP",
                                     "Hop Balance" = "REL_SINGLE_LEG_HOP_BALANCE",
                                     "Y-Balance" = "REL_Y_BALANCE_COMPOSITE",
                                     "Hip Power" = "REL_HIP_POWER",
                                     "Leg Force" = "REL_LEG_FORCE",
                                     "Leg Balance" = "REL_LEG_BALANCE",
                                     "Leg Symmetry" = "REL_LEG_SYMMETRY",
                                     "Sprint 10M" = "REL_SPRINT_10M",
                                     "Change of Direction" = "REL_CHANGE_OF_DIRECTION",
                                     "Repeated Jump Index" = "REL_REPEATED_JUMP_INDEX",
                                     "Squat Depth Time" = "REL_SQUAT_DEPTH_TIME"), value=TRUE))
    y_name <- names(grep(y_metric, c("Squat" = "REL_SQUAT",
                                     "Countermovement Jump" = "REL_COUNTERMOVEMENT_JUMP",
                                     "Hop Balance" = "REL_SINGLE_LEG_HOP_BALANCE",
                                     "Y-Balance" = "REL_Y_BALANCE_COMPOSITE",
                                     "Hip Power" = "REL_HIP_POWER",
                                     "Leg Force" = "REL_LEG_FORCE",
                                     "Leg Balance" = "REL_LEG_BALANCE",
                                     "Leg Symmetry" = "REL_LEG_SYMMETRY",
                                     "Sprint 10M" = "REL_SPRINT_10M",
                                     "Change of Direction" = "REL_CHANGE_OF_DIRECTION",
                                     "Repeated Jump Index" = "REL_REPEATED_JUMP_INDEX",
                                     "Squat Depth Time" = "REL_SQUAT_DEPTH_TIME"), value=TRUE))
    
    # Create scatter plot with trendline
    p <- plotly::plot_ly(df, x = ~get(x_metric), y = ~get(y_metric),
                         type = 'scatter', mode = 'markers',
                         marker = list(size = 8, color = '#003d7a', opacity = 0.7),
                         text = ~FULL_NAME,
                         hovertemplate = '<b>%{text}</b><br>X: %{x:.2f}<br>Y: %{y:.2f}<extra></extra>')
    
    # Add trendline
    z <- lm(df[[y_metric]] ~ df[[x_metric]])
    x_range <- range(df[[x_metric]], na.rm=TRUE)
    y_pred <- predict(z, newdata = data.frame(x = x_range))
    
    p <- p %>% plotly::add_trace(x = x_range, y = y_pred,
                                 mode = 'lines',
                                 line = list(color = '#ff6b35', width = 2),
                                 hoverinfo = 'none',
                                 showlegend = TRUE,
                                 name = 'Trendline')
    
    p <- p %>% plotly::layout(
      title = paste0("Correlation: ", x_metric, " vs ", y_metric, " (r = ", round(corr, 3), ")"),
      xaxis = list(title = x_metric),
      yaxis = list(title = y_metric),
      hovermode = 'closest'
    )
    
    p
  })

  output$correlation_value <- renderText({
    x_metric <- input$corr_metric_x
    y_metric <- input$corr_metric_y
    
    df <- vald_data
    
    if (input$corr_position != "All") {
      df <- df[df$POSITION == input$corr_position, ]
    }
    
    df <- df[!is.na(df[[x_metric]]) & !is.na(df[[y_metric]]), ]
    
    if (nrow(df) == 0) {
      return("No data available")
    }
    
    corr <- cor(df[[x_metric]], df[[y_metric]], use="complete.obs")
    paste0("Pearson Correlation: ", round(corr, 3))
  })
}

# ---------------------------
# Run the Shiny app
# ---------------------------

shinyApp(ui, server)