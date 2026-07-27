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
  ts_data <- read_csv("data/timeseries.csv")
  route_data <- read_csv("data/route_summary.csv")
  val_data <- read_csv("data/validation.csv")
  ent_data <- read_csv("data/entrainment_7day.csv")
  
  ts_data$datetime <- as.POSIXct(ts_data$datetime)
  
  # Load shapefiles
  delta_boundary <- st_read("data/shapefiles/Bay_Delta_Poly_New.shp", quiet = TRUE)
  delta_channels <- st_read("data/shapefiles/hydro_delta_marsh.shp", quiet = TRUE)
  nodes_sf <- st_read("data/shapefiles/nodes.shp", quiet = TRUE)

  # Transform to WGS84 for leaflet
  delta_boundary <- st_transform(delta_boundary, 4326)
  delta_channels <- st_transform(delta_channels, 4326)
  nodes_sf <- st_transform(nodes_sf, 4326)
  
  # ✅ FIX: ensure matching data types
  nodes_sf$node <- as.character(nodes_sf$node)
  ts_data$node <- as.character(ts_data$node)
  
  # -----------------------------
  # Initialize Inputs
  # -----------------------------
  updateSelectInput(session, "scenario",
                    choices = unique(ts_data$scenario))
  
  updateSelectInput(session, "node",
                    choices = unique(ts_data$node))
  
  # -----------------------------
  # Reactive Data
  # -----------------------------
  map_data <- reactive({
    
    node_values <- filtered_ts() %>%
      group_by(node) %>%
      summarise(
        entrainment = mean(entrainment, na.rm = TRUE)
      )
    
    nodes_sf %>%
      left_join(node_values, by = "node")
  })
  filtered_ts <- reactive({
    ts_data %>%
      filter(scenario == input$scenario,
             node == input$node)
  })
  
  filtered_val <- reactive({
    val_data %>%
      filter(scenario == input$scenario)
  })
  
  filtered_ent <- reactive({
    ent_data %>%
      filter(scenario == input$scenario)
  })
  
  # -----------------------------
  # Plots
  # -----------------------------
  
  output$survival_plot <- renderPlot({
    ggplot(filtered_ts(), aes(x = datetime, y = survival, color = node)) +
      geom_line(linewidth = 1) +
      scale_color_viridis(discrete = TRUE) +
      labs(title = "Survival Over Time",
           x = "Time", y = "Survival") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13)
      )
  })
  
  output$entrainment_plot <- renderPlot({
    ggplot(filtered_ts(), aes(x = datetime, y = entrainment, color = node)) +
      geom_line(linewidth = 1) +
      scale_color_viridis(discrete = TRUE) +
      labs(title = "Entrainment Over Time",
           x = "Time", y = "Entrainment") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13)
      )
  })
  
  output$validation_plot <- renderPlot({
    ggplot(filtered_val(), aes(x = ptm_value, y = emulator_value, color = scenario)) +
      geom_point(size = 2) +
      geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
      scale_color_viridis(discrete = TRUE) +
      labs(title = "PTM vs Emulator",
           x = "PTM",
           y = "Emulator") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13)
      )
  })
  
  
  output$entrainment_7day_plot <- renderPlot({
    ggplot(filtered_ent(), aes(x = day, y = entrainment, color = location)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_color_viridis(discrete = TRUE) +
      labs(title = "7-Day Entrainment",
           x = "Day",
           y = "Entrainment") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13)
      )
  })
  
  output$route_plot <- renderPlot({
    route_data %>%
      filter(scenario == input$scenario) %>%
      ggplot(aes(x = route, y = probability, fill = route)) +
      geom_col() +
      scale_fill_viridis(discrete = TRUE) +
      labs(title = "Routing Probability",
           x = "Route",
           y = "Probability") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        legend.position = "none"
      )
  })
  output$map <- renderLeaflet({
    
    df <- map_data()
    
    pal <- colorNumeric(
      "viridis",
      domain = c(0, 1),   # ✅ fallback range
      na.color = "grey"
    )
    
    leaflet() %>%
      
      # Basemap
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # -----------------------------
    # Delta Boundary
    # -----------------------------
    addPolygons(
      data = delta_boundary,
      color = "black",
      weight = 2,
      fillOpacity = 0.05,
      group = "Boundary"
    ) %>%
      
      # -----------------------------
    # Channel Network
    # -----------------------------
    addPolylines(
      data = delta_channels,
      color = "#2b8cbe",
      weight = 1,
      opacity = 0.6,
      group = "Channels"
    ) %>%
      
      # -----------------------------
    # Nodes (with entrainment)
    # -----------------------------
    addCircleMarkers(
      data = df,
      radius = ~5 + entrainment * 25,
      color = ~pal(entrainment),
      fillOpacity = 0.9,
      stroke = FALSE,
      group = "Nodes",
      
      label = ~paste0(
        "Node: ", node,
        "<br>Entrainment: ", round(entrainment, 3)
      )
    ) %>%
      
      # -----------------------------
    # Legend
    # -----------------------------
    addLegend(
      "bottomright",
      pal = pal,
      values = df$entrainment[!is.na(df$entrainment)],
      title = "Entrainment"
    ) %>%
      
      # -----------------------------
    # Layer Control (VERY NICE FEATURE)
    # -----------------------------
    addLayersControl(
      overlayGroups = c("Boundary", "Channels", "Nodes"),
      options = layersControlOptions(collapsed = FALSE)
    )
  })
}
