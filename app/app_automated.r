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
  if (is.na(slug) || !nzchar(slug)) return(list(NA_character_, NA_character_))
  
  view <- match.arg(view)
  for (ext in exts) {
    fn <- paste0(slug, "_", view, ".", ext)
    if (file.exists(file.path("www", "PHOTOS", fn))) {
      return(paste0("PHOTOS/", fn))
    }
  }
  NA_character_
}

player_photo_slugs <- function(player_name) {
  slug <- player_photo_slug(player_name)
  if (is.na(slug) || !nzchar(slug)) return(NA_character_)
  slug
}

find_player_photos <- function(player_name, views = c("front","side","back"),
                               exts = c("jpg","jpeg","png","webp","heif","heic"),
                               base_url_prefix = "pics") {
  slug <- player_photo_slugs(player_name)
  if (is.na(slug) || !nzchar(slug)) return(c(NA_character_, NA_character_, NA_character_))
  
  out <- c(NA_character_, NA_character_, NA_character_)
  names(out) <- c("front", "side", "back")
  for (i in seq_along(views)) {
    view <- views[i]
    for (ext in exts) {
      fn <- paste0(slug, "_", view, ".", ext)
      if (file.exists(file.path("www", "PHOTOS", fn))) {
        out[view] <- paste0("PHOTOS/", fn)
        break
      }
    }
  }
  out
}

get_photo <- function(img, htmltools = T) {
  if (is.na(img) || !nzchar(img)) {
    img_tag <- tags$img(src="blank.jpg", height="100%")
  } else {
    img_tag <- tags$img(src = img, height="100%")
  }
  img_tag
}

get_headshot <- function(player_name, htmltools = T) {
  img <- find_player_headshot(player_name)
  if (is.na(img) || !nzchar(img)) {
    img_tag <- tags$img(src="blank.jpg", height="100%")
  } else {
    img_tag <- tags$img(src = img, height="100%")
  }
  img_tag
}

get_photos <- function(player_name) {
  photos <- find_player_photos(player_name)
  
  front_tag <- if (!is.na(photos["front"])) tags$img(src = photos["front"], height="100%") else tags$img(src="blank.jpg", height="100%")
  side_tag  <- if (!is.na(photos["side"])) tags$img(src = photos["side"], height="100%") else tags$img(src="blank.jpg", height="100%")
  back_tag  <- if (!is.na(photos["back"])) tags$img(src = photos["back"], height="100%") else tags$img(src="blank.jpg", height="100%")
  
  list(front = front_tag, side = side_tag, back = back_tag)
}

get_metric_display_name <- function(metric_short) {
  map <- list(
    "Weight" = "BW",
    "BF%" = "BF%",
    "Lean Mass" = "LM",
    "Grip" = "GripStr",
    "RSI" = "RSI",
    "Jump Power" = "Pwr",
    "Jump Height" = "CMJ Height",
    "RSI-mod" = "RSI-mod",
    "Fly 10-15" = "Fly 10-15",
    "Monopod ROM" = "ROM",
    "ISO L Strength" = "ISO L",
    "ISO R Strength" = "ISO R",
    "ISO Prone" = "ISO Prone"
  )
  
  # Create reverse lookup
  names_to_short <- setNames(names(map), unlist(map))
  
  if (metric_short %in% unlist(map)) {
    return(names_to_short[[metric_short]])
  }
  return(metric_short)
}

metric_short_to_display <- function(metric_short) {
  map <- list(
    "BW" = "Weight",
    "BF%" = "BF%",
    "LM" = "Lean Mass",
    "GripStr" = "Grip",
    "RSI" = "RSI",
    "Pwr" = "Jump Power",
    "CMJ Height" = "Jump Height",
    "RSI-mod" = "RSI-mod",
    "Fly 10-15" = "Fly 10-15",
    "ROM" = "Monopod ROM",
    "ISO L" = "ISO L Strength",
    "ISO R" = "ISO R Strength",
    "ISO Prone" = "ISO Prone"
  )
  
  if (metric_short %in% names(map)) {
    return(map[[metric_short]])
  }
  return(metric_short)
}

# ---------------------------
# UI
# ---------------------------

ui <- navbarPage(
  title = "UCLA Football Strength & Conditioning",
  id = "navbar",
  theme = bslib::bs_theme(version = 5, preset = "bootstrap"),
  
  # Tab 1: Athlete Profiles
  tabPanel(
    title = "Athlete Profiles",
    value = "athlete_profiles",
    
    fluidRow(
      column(
        width = 3,
        h4("Filters"),
        selectInput(
          "filter_position",
          "Position",
          choices = c("All", sort(unique(vald_data$position)))
        ),
        selectInput(
          "filter_class",
          "Class",
          choices = c("All", sort(unique(vald_data$class)))
        )
      ),
      column(
        width = 9,
        h4("Athletes"),
        DT::dataTableOutput("athlete_table")
      )
    )
  ),
  
  # Tab 2: Player Card
  tabPanel(
    title = "Player Card",
    value = "player_card",
    
    fluidRow(
      column(
        width = 3,
        textInput(
          "search_player",
          "Search Player",
          placeholder = "Enter player name..."
        ),
        actionButton("submit_player_search", "Search")
      ),
      column(
        width = 9,
        h4(textOutput("selected_player_name"))
      )
    ),
    
    fluidRow(
      column(
        width = 3,
        h5("Photo"),
        uiOutput("player_photo")
      ),
      column(
        width = 9,
        h5("Metrics"),
        DT::dataTableOutput("player_metrics_table")
      )
    )
  ),
  
  # Tab 3: Correlations
  tabPanel(
    title = "Correlations",
    value = "correlations",
    
    fluidRow(
      column(
        width = 4,
        selectInput(
          "corr_metric_x",
          "X-axis Metric",
          choices = c(
            "Weight" = "BW",
            "BF%" = "BF%",
            "Lean Mass" = "LM",
            "Grip" = "GripStr",
            "RSI" = "RSI",
            "Jump Power" = "Pwr",
            "Jump Height" = "CMJ Height",
            "RSI-mod" = "RSI-mod",
            "Fly 10-15" = "Fly 10-15",
            "Monopod ROM" = "ROM",
            "ISO L Strength" = "ISO L",
            "ISO R Strength" = "ISO R",
            "ISO Prone" = "ISO Prone"
          ),
          selected = "BW"
        )
      ),
      column(
        width = 4,
        selectInput(
          "corr_metric_y",
          "Y-axis Metric",
          choices = c(
            "Weight" = "BW",
            "BF%" = "BF%",
            "Lean Mass" = "LM",
            "Grip" = "GripStr",
            "RSI" = "RSI",
            "Jump Power" = "Pwr",
            "Jump Height" = "CMJ Height",
            "RSI-mod" = "RSI-mod",
            "Fly 10-15" = "Fly 10-15",
            "Monopod ROM" = "ROM",
            "ISO L Strength" = "ISO L",
            "ISO R Strength" = "ISO R",
            "ISO Prone" = "ISO Prone"
          ),
          selected = "RSI"
        )
      ),
      column(
        width = 4,
        selectInput(
          "corr_filter_position",
          "Position",
          choices = c("All", sort(unique(vald_data$position)))
        )
      )
    ),
    
    fluidRow(
      plotlyOutput("corr_plot", height = "600px")
    )
  ),
  
  # Tab 4: Trends
  tabPanel(
    title = "Trends",
    value = "trends",
    
    fluidRow(
      column(
        width = 3,
        selectInput(
          "trend_metric",
          "Metric",
          choices = c(
            "Weight" = "BW",
            "BF%" = "BF%",
            "Lean Mass" = "LM",
            "Grip" = "GripStr",
            "RSI" = "RSI",
            "Jump Power" = "Pwr",
            "Jump Height" = "CMJ Height",
            "RSI-mod" = "RSI-mod",
            "Fly 10-15" = "Fly 10-15",
            "Monopod ROM" = "ROM",
            "ISO L Strength" = "ISO L",
            "ISO R Strength" = "ISO R",
            "ISO Prone" = "ISO Prone"
          ),
          selected = "BW"
        )
      ),
      column(
        width = 3,
        selectInput(
          "trend_position",
          "Position",
          choices = c("All", sort(unique(vald_data$position)))
        )
      ),
      column(
        width = 6,
        checkboxGroupInput(
          "trend_class",
          "Classes",
          choices = sort(unique(vald_data$class)),
          selected = sort(unique(vald_data$class)),
          inline = TRUE
        )
      )
    ),
    
    fluidRow(
      plotlyOutput("trend_plot", height = "600px")
    ),
    
    fluidRow(
      column(
        width = 12,
        h5("Summary Statistics"),
        tableOutput("trend_summary_table")
      )
    )
  )
)

# ---------------------------
# Server
# ---------------------------

server <- function(input, output, session) {
  
  # Athlete Profiles Tab
  output$athlete_table <- DT::renderDataTable({
    df <- vald_data
    
    if (input$filter_position != "All") {
      df <- df %>% filter(position == input$filter_position)
    }
    if (input$filter_class != "All") {
      df <- df %>% filter(class == input$filter_class)
    }
    
    # Select columns for display
    df_display <- df %>%
      select(Player, position, class, BW, `BF%`, LM, GripStr, RSI, Pwr, `CMJ Height`, `RSI-mod`, `Fly 10-15`, ROM, `ISO L`, `ISO R`, `ISO Prone`) %>%
      arrange(Player)
    
    datatable(
      df_display,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-right', targets = 3:15)
        )
      )
    )
  })
  
  # Player Card Tab
  observeEvent(input$submit_player_search, {
    player_name <- input$search_player
    if (player_name %in% vald_data$Player) {
      # Update selected player
      updateTextInput(session, "selected_player", value = player_name)
    } else {
      showNotification("Player not found", type = "error")
    }
  })
  
  selected_player <- reactive({
    if (!is.null(input$search_player) && input$search_player != "") {
      input$search_player
    } else {
      NA_character_
    }
  })
  
  output$selected_player_name <- renderText({
    if (is.na(selected_player())) {
      "No player selected"
    } else {
      selected_player()
    }
  })
  
  output$player_photo <- renderUI({
    if (is.na(selected_player())) {
      return(NULL)
    }
    
    player_name <- selected_player()
    photos <- find_player_photos(player_name)
    
    div(
      style = "display: flex; gap: 10px;",
      div(
        h6("Front"),
        if (!is.na(photos["front"])) tags$img(src = photos["front"], height = "200px") else tags$img(src="blank.jpg", height="200px")
      ),
      div(
        h6("Side"),
        if (!is.na(photos["side"])) tags$img(src = photos["side"], height = "200px") else tags$img(src="blank.jpg", height="200px")
      ),
      div(
        h6("Back"),
        if (!is.na(photos["back"])) tags$img(src = photos["back"], height = "200px") else tags$img(src="blank.jpg", height="200px")
      )
    )
  })
  
  output$player_metrics_table <- DT::renderDataTable({
    if (is.na(selected_player())) {
      return(NULL)
    }
    
    player_name <- selected_player()
    player_data <- vald_data %>% filter(Player == player_name)
    
    if (nrow(player_data) == 0) {
      return(NULL)
    }
    
    # Reshape data for display
    metrics <- c("BW", "BF%", "LM", "GripStr", "RSI", "Pwr", "CMJ Height", "RSI-mod", "Fly 10-15", "ROM", "ISO L", "ISO R", "ISO Prone")
    
    metric_display <- sapply(metrics, function(m) {
      if (m %in% names(player_data)) {
        val <- player_data[[m]]
        if (is.numeric(val)) {
          round(val, 2)
        } else {
          val
        }
      } else {
        NA
      }
    })
    
    df_display <- data.frame(
      Metric = sapply(metrics, metric_short_to_display),
      Value = as.character(metric_display)
    )
    
    datatable(
      df_display,
      options = list(
        pageLength = 20,
        dom = 't'
      ),
      rownames = FALSE
    )
  })
  
  # Correlations Tab
  output$corr_plot <- renderPlotly({
    df <- vald_data
    
    if (input$corr_filter_position != "All") {
      df <- df %>% filter(position == input$corr_filter_position)
    }
    
    metric_x <- input$corr_metric_x
    metric_y <- input$corr_metric_y
    
    if (!(metric_x %in% names(df)) || !(metric_y %in% names(df))) {
      return(NULL)
    }
    
    # Remove NAs
    df_clean <- df %>%
      select(Player, position, all_of(c(metric_x, metric_y))) %>%
      filter(!is.na(.[[metric_x]]) & !is.na(.[[metric_y]]))
    
    if (nrow(df_clean) == 0) {
      return(NULL)
    }
    
    # Create scatter plot
    plot_data <- df_clean
    plot_data$x <- plot_data[[metric_x]]
    plot_data$y <- plot_data[[metric_y]]
    
    # Calculate correlation
    corr <- cor(plot_data$x, plot_data$y, use = "complete.obs")
    
    # Fit linear regression line
    fit <- lm(y ~ x, data = plot_data)
    
    # Create plot
    p <- plot_ly(plot_data) %>%
      add_trace(
        x = ~x,
        y = ~y,
        mode = 'markers',
        type = 'scatter',
        marker = list(size = 8, color = 'steelblue', opacity = 0.7),
        text = ~Player,
        hovertemplate = '<b>%{text}</b><br>' %>
          paste0(metric_short_to_display(metric_x), ': %{x}<br>') %>
          paste0(metric_short_to_display(metric_y), ': %{y}<extra></extra>')
      ) %>%
      add_trace(
        x = ~x,
        y = ~fitted(fit),
        mode = 'lines',
        type = 'scatter',
        line = list(color = 'red', dash = 'dash'),
        name = paste0('r = ', round(corr, 3)),
        hoverinfo = 'skip'
      ) %>%
      layout(
        title = paste0('Correlation: ', metric_short_to_display(metric_x), ' vs ', metric_short_to_display(metric_y)),
        xaxis = list(title = metric_short_to_display(metric_x)),
        yaxis = list(title = metric_short_to_display(metric_y)),
        hovermode = 'closest',
        showlegend = TRUE
      )
    
    p
  })
  
  # Trends Tab
  output$trend_plot <- renderPlotly({
    df <- vald_data
    
    if (input$trend_position != "All") {
      df <- df %>% filter(position == input$trend_position)
    }
    
    if (length(input$trend_class) > 0) {
      df <- df %>% filter(class %in% input$trend_class)
    }
    
    metric <- input$trend_metric
    
    if (!(metric %in% names(df))) {
      return(NULL)
    }
    
    # Check if we have any data
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    # Remove NAs for the selected metric
    df_clean <- df %>%
      filter(!is.na(.[[metric]])) %>%
      select(Player, class, all_of(metric)) %>%
      rename(value = all_of(metric))
    
    if (nrow(df_clean) == 0) {
      return(NULL)
    }
    
    # Create box plot
    p <- plot_ly(
      df_clean,
      x = ~class,
      y = ~value,
      type = 'box',
      color = ~class
    ) %>%
      add_trace(
        x = ~class,
        y = ~value,
        type = 'scatter',
        mode = 'markers',
        marker = list(size = 5, color = 'steelblue', opacity = 0.5),
        text = ~Player,
        hovertemplate = '<b>%{text}</b><br>Class: %{x}<br>Value: %{y}<extra></extra>',
        showlegend = FALSE
      ) %>%
      layout(
        title = paste0(metric_short_to_display(metric), ' by Class'),
        xaxis = list(title = 'Class'),
        yaxis = list(title = metric_short_to_display(metric)),
        boxmode = 'group',
        hovermode = 'closest',
        showlegend = FALSE,
        margin = list(b = 100)
      )
    
    p
  })
  
  output$trend_summary_table <- renderTable({
    df <- vald_data
    
    if (input$trend_position != "All") {
      df <- df %>% filter(position == input$trend_position)
    }
    
    if (length(input$trend_class) > 0) {
      df <- df %>% filter(class %in% input$trend_class)
    }
    
    metric <- input$trend_metric
    
    if (!(metric %in% names(df))) {
      return(NULL)
    }
    
    # Remove NAs for the selected metric
    df_clean <- df %>%
      filter(!is.na(.[[metric]])) %>%
      select(class, all_of(metric)) %>%
      rename(value = all_of(metric))
    
    if (nrow(df_clean) == 0) {
      return(NULL)
    }
    
    # Calculate summary statistics by class
    summary_stats <- df_clean %>%
      group_by(class) %>%
      summarise(
        N = n(),
        Mean = mean(value, na.rm = TRUE),
        Median = median(value, na.rm = TRUE),
        SD = sd(value, na.rm = TRUE),
        Min = min(value, na.rm = TRUE),
        Max = max(value, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        across(where(is.numeric) & !c(N), ~round(., 2))
      )
    
    summary_stats
  })
}

# ---------------------------
# Run App
# ---------------------------

shinyApp(ui, server)