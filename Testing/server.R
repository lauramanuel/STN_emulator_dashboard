library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(viridis)
library(sf)
library(leaflet)

server <- function(input, output, session) {
  
  # -----------------------------
  # Load Data
  # -----------------------------
  ts_data <- read_csv("data/timeseries.csv", show_col_types = FALSE)
  route_data <- read_csv("data/route_summary.csv", show_col_types = FALSE)
  val_data <- read_csv("data/validation.csv", show_col_types = FALSE)
  ent_data <- read_csv("data/entrainment_7day.csv", show_col_types = FALSE)
  
  ts_data$datetime <- as.POSIXct(ts_data$datetime)
  ts_data$node <- as.character(ts_data$node)
  
  # -----------------------------
  # Create Placeholder Forecast Data
  # -----------------------------
  # This is only for app layout testing.
  # Replace this later with real forecast emulator outputs.
  forecast_ts_data <- bind_rows(
    ts_data %>% filter(scenario == "baseline") %>% mutate(scenario = "baseline"),
    ts_data %>% filter(scenario == "baseline") %>% mutate(scenario = "A", entrainment = pmin(1, entrainment * 1.10), survival = pmax(0, survival * 0.98)),
    ts_data %>% filter(scenario == "baseline") %>% mutate(scenario = "B", entrainment = pmin(1, entrainment * 1.25), survival = pmax(0, survival * 0.96)),
    ts_data %>% filter(scenario == "baseline") %>% mutate(scenario = "C", entrainment = pmin(1, entrainment * 0.90), survival = pmax(0, survival * 1.01)),
    ts_data %>% filter(scenario == "baseline") %>% mutate(scenario = "D", entrainment = pmin(1, entrainment * 1.40), survival = pmax(0, survival * 0.94))
  )
  
  # -----------------------------
  # Load Spatial Data
  # -----------------------------
  delta_boundary <- st_read("data/shapefiles/Bay_Delta_Poly_New.shp", quiet = TRUE)
  delta_channels <- st_read("data/shapefiles/hydro_delta_marsh.shp", quiet = TRUE)
  nodes_sf <- st_read("data/shapefiles/nodes.shp", quiet = TRUE)
  
  delta_boundary <- st_transform(delta_boundary, 4326)
  delta_channels <- st_transform(delta_channels, 4326)
  nodes_sf <- st_transform(nodes_sf, 4326)
  
  nodes_sf$node <- as.character(nodes_sf$node)
  
  # -----------------------------
  # Initialize Inputs
  # -----------------------------
  updateDateRangeInput(
    session,
    "dates",
    start = min(ts_data$datetime, na.rm = TRUE),
    end = max(ts_data$datetime, na.rm = TRUE)
  )
  
  updateSelectInput(
    session,
    "node",
    choices = sort(unique(ts_data$node)),
    selected = sort(unique(ts_data$node))[1]
  )
  
  # -----------------------------
  # Scenario Control
  # -----------------------------
  output$scenario_control <- renderUI({
    
    req(input$tabs)
    
    if (grepl("^forecast7", input$tabs)) {
      selectInput(
        "scenario",
        "Scenario:",
        choices = c("baseline", "A", "B", "C", "D"),
        selected = "baseline"
      )
    } else {
      tagList(
        selectInput(
          "scenario",
          "Scenario:",
          choices = "baseline",
          selected = "baseline"
        ),
        tags$small(
          "Current condition pages use baseline scenario.",
          style = "color:#666;"
        )
      )
    }
  })
  
  # -----------------------------
  # Helper: Determine Active Data Source
  # -----------------------------
  active_data <- reactive({
    
    req(input$tabs)
    
    if (grepl("^forecast7", input$tabs)) {
      forecast_ts_data
    } else {
      ts_data %>%
        filter(scenario == "baseline")
    }
  })
  
  # -----------------------------
  # Helper: Filtered Time Series Data
  # -----------------------------
  filtered_ts <- reactive({
    
    df <- active_data()
    
    if (!is.null(input$dates) && length(input$dates) == 2) {
      df <- df %>%
        filter(
          datetime >= as.POSIXct(input$dates[1]),
          datetime <= as.POSIXct(input$dates[2]) + 86399
        )
    }
    
    if (!is.null(input$scenario)) {
      df <- df %>%
        filter(scenario == input$scenario)
    }
    
    if (!is.null(input$node) && input$tabs %in% c(
      "current7_ptm7",
      "current7_ptm30",
      "current7_ecoptm",
      "current30_ptm7",
      "current30_ptm30",
      "current30_ecoptm",
      "forecast7_ptm7",
      "forecast7_ptm30",
      "forecast7_ecoptm"
    )) {
      df <- df %>%
        filter(node == input$node)
    }
    
    df
  })
  
  # -----------------------------
  # Helper: Event Horizon Data
  # -----------------------------
  event_ts <- reactive({
    
    df <- active_data()
    
    if (!is.null(input$dates) && length(input$dates) == 2) {
      df <- df %>%
        filter(
          datetime >= as.POSIXct(input$dates[1]),
          datetime <= as.POSIXct(input$dates[2]) + 86399
        )
    }
    
    if (!is.null(input$scenario)) {
      df <- df %>%
        filter(scenario == input$scenario)
    }
    
    df
  })
  
  map_data <- reactive({
    
    node_values <- event_ts() %>%
      group_by(node) %>%
      summarise(
        entrainment = mean(entrainment, na.rm = TRUE),
        survival = mean(survival, na.rm = TRUE),
        flow = mean(flow, na.rm = TRUE),
        .groups = "drop"
      )
    
    nodes_sf %>%
      left_join(node_values, by = "node")
  })
  
  # -----------------------------
  # Plot Helpers
  # -----------------------------
  make_timeseries_plot <- function(metric = "entrainment", title = "PTM Entrainment Time Series") {
    
    df <- filtered_ts()
    
    validate(
      need(nrow(df) > 0, "No data available for the selected filters.")
    )
    
    ggplot(df, aes(x = datetime, y = .data[[metric]], color = node)) +
      geom_line(linewidth = 1) +
      scale_color_viridis(discrete = TRUE) +
      labs(
        title = title,
        x = "Date / Time",
        y = metric,
        color = "Node"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        legend.position = "bottom"
      )
  }
  
  make_summary_table <- function(metric = "entrainment") {
    
    filtered_ts() %>%
      summarise(
        node = first(node),
        scenario = first(scenario),
        start_datetime = min(datetime, na.rm = TRUE),
        end_datetime = max(datetime, na.rm = TRUE),
        mean_flow = round(mean(flow, na.rm = TRUE), 2),
        mean_survival = round(mean(survival, na.rm = TRUE), 3),
        mean_entrainment = round(mean(entrainment, na.rm = TRUE), 3),
        max_entrainment = round(max(entrainment, na.rm = TRUE), 3)
      )
  }
  
  make_ecoptm_table <- function() {
    
    filtered_ts() %>%
      group_by(node, scenario) %>%
      summarise(
        mean_flow = round(mean(flow, na.rm = TRUE), 2),
        mean_survival = round(mean(survival, na.rm = TRUE), 3),
        mean_entrainment = round(mean(entrainment, na.rm = TRUE), 3),
        risk_class = case_when(
          mean_entrainment >= 0.15 ~ "High",
          mean_entrainment >= 0.10 ~ "Moderate",
          TRUE ~ "Low"
        ),
        .groups = "drop"
      )
  }
  
  make_scatter_plot <- function(title = "Event Horizon Scatter Plot") {
    
    df <- event_ts()
    
    validate(
      need(nrow(df) > 0, "No data available for the selected filters.")
    )
    
    ggplot(df, aes(x = flow, y = entrainment, color = survival)) +
      geom_point(size = 2, alpha = 0.75) +
      scale_color_viridis(option = "viridis") +
      labs(
        title = title,
        x = "Flow",
        y = "Entrainment",
        color = "Survival"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.position = "bottom"
      )
  }
  
  make_event_map <- function() {
    
    df <- map_data()
    
    pal <- colorNumeric(
      palette = "viridis",
      domain = c(0, 1),
      na.color = "grey"
    )
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      addPolygons(
        data = delta_boundary,
        color = "black",
        weight = 2,
        fillOpacity = 0.05,
        group = "Boundary"
      ) %>%
      
      addPolylines(
        data = delta_channels,
        color = "#2b8cbe",
        weight = 1,
        opacity = 0.6,
        group = "Channels"
      ) %>%
      
      addCircleMarkers(
        data = df,
        radius = ~ifelse(is.na(entrainment), 4, 5 + entrainment * 25),
        color = ~pal(entrainment),
        fillOpacity = 0.9,
        stroke = FALSE,
        group = "Nodes",
        label = ~paste0(
          "Node: ", node,
          "<br>Entrainment: ", round(entrainment, 3),
          "<br>Survival: ", round(survival, 3),
          "<br>Flow: ", round(flow, 1)
        )
      ) %>%
      
      addLegend(
        "bottomright",
        pal = pal,
        values = c(0, 1),
        title = "Entrainment",
        opacity = 1
      ) %>%
      
      addLayersControl(
        overlayGroups = c("Boundary", "Channels", "Nodes"),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  # -----------------------------
  # Current 7d Average Flow Outputs
  # -----------------------------
  output$current7_ptm7_plot <- renderPlot({
    make_timeseries_plot("entrainment", "Current 7d Average Flow - PTM 7d Entrainment")
  })
  
  output$current7_ptm7_summary <- renderTable({
    make_summary_table("entrainment")
  })
  
  output$current7_ptm30_plot <- renderPlot({
    make_timeseries_plot("entrainment", "Current 7d Average Flow - PTM 30d Entrainment")
  })
  
  output$current7_ptm30_summary <- renderTable({
    make_summary_table("entrainment")
  })
  
  output$current7_ecoptm_table <- renderTable({
    make_ecoptm_table()
  })
  
  output$current7_event_map <- renderLeaflet({
    make_event_map()
  })
  
  output$current7_event_scatter <- renderPlot({
    make_scatter_plot("Current 7d Average Flow - Event Horizon")
  })
  
  # -----------------------------
  # Current 30d Average Flow Outputs
  # -----------------------------
  output$current30_ptm7_plot <- renderPlot({
    make_timeseries_plot("entrainment", "Current 30d Average Flow - PTM 7d Entrainment")
  })
  
  output$current30_ptm7_summary <- renderTable({
    make_summary_table("entrainment")
  })
  
  output$current30_ptm30_plot <- renderPlot({
    make_timeseries_plot("entrainment", "Current 30d Average Flow - PTM 30d Entrainment")
  })
  
  output$current30_ptm30_summary <- renderTable({
    make_summary_table("entrainment")
  })
  
  output$current30_ecoptm_table <- renderTable({
    make_ecoptm_table()
  })
  
  output$current30_event_map <- renderLeaflet({
    make_event_map()
  })
  
  output$current30_event_scatter <- renderPlot({
    make_scatter_plot("Current 30d Average Flow - Event Horizon")
  })
  
  # -----------------------------
  # Forecast 7d Average Flow Outputs
  # -----------------------------
  output$forecast7_ptm7_plot <- renderPlot({
    make_timeseries_plot("entrainment", "Forecast 7d Average Flow - PTM 7d Entrainment")
  })
  
  output$forecast7_ptm7_summary <- renderTable({
    make_summary_table("entrainment")
  })
  
  output$forecast7_ptm30_plot <- renderPlot({
    make_timeseries_plot("entrainment", "Forecast 7d Average Flow - PTM 30d Entrainment")
  })
  
  output$forecast7_ptm30_summary <- renderTable({
    make_summary_table("entrainment")
  })
  
  output$forecast7_ecoptm_table <- renderTable({
    make_ecoptm_table()
  })
  
  output$forecast7_event_map <- renderLeaflet({
    make_event_map()
  })
  
  output$forecast7_event_scatter <- renderPlot({
    make_scatter_plot("Forecast 7d Average Flow - Event Horizon")
  })
}