library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(viridis)
library(sf)
library(leaflet)
library(lwgeom)

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
  eh_baseline <- read_csv(
    "../STN_EMULATOR/EH_baseline.csv",
    show_col_types = FALSE
  )
  global_eh_min <- min(
    c(
      eh_baseline$Horizon_15,
      eh_baseline$Horizon_30,
      eh_baseline$Horizon_80
    ),
    na.rm = TRUE
  )
  
  global_eh_max <- max(
    c(
      eh_baseline$Horizon_15,
      eh_baseline$Horizon_30,
      eh_baseline$Horizon_80
    ),
    na.rm = TRUE
  )
  eventhorizon7 <- read_csv(
    "../STN_EMULATOR/Output/20260224/eventhorizon7/eventhorizon.csv",
    show_col_types = FALSE
  )
  
  eventhorizon30 <- read_csv(
    "../STN_EMULATOR/Output/20260224/eventhorizon30/eventhorizon.csv",
    show_col_types = FALSE
  )

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
  delta_boundary <- st_read("../STN_EMULATOR/shapefiles/Bay_Delta_Poly_New.shp", quiet = TRUE)
  delta_channels <- st_read("../STN_EMULATOR/shapefiles/hydro_delta_marsh.shp", quiet = TRUE)
  nodes_sf <- st_read("../STN_EMULATOR/shapefiles/nodes.shp", quiet = TRUE)
  river_centerline <- st_read("../STN_EMULATOR/shapefiles/CCF_OldRiver_CL.shp", quiet = TRUE)
  river_length_m <- as.numeric(st_length(river_centerline))
  river_length_miles <- river_length_m / 1609.344
  print(river_length_miles)
  delta_boundary <- st_transform(delta_boundary, 4326)
  delta_channels <- st_transform(delta_channels, 4326)
  river_centerline <- st_transform(river_centerline, 4326)
  nodes_sf <- st_transform(nodes_sf, 4326)
  
  nodes_sf$node <- as.character(nodes_sf$node)
  
  st_crs(delta_boundary)
  
  st_crs(river_centerline)
  st_geometry_type(river_centerline)
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
  # Helper: end date helper
  # -----------------------------
  selected_event_date <- reactive({
    
    req(input$dates)
    
    as.numeric(
      format(
        as.Date(input$dates[2]),
        "%Y%m%d"
      )
    )
    
  })
 
  # -----------------------------
  # Helper: event horizon geometry
  # -----------------------------
  make_event_geometry <- function(event_horizon_miles,
                                  centerline,
                                  total_length_miles){
    
    # Convert miles to fraction of total river length
    fraction <- event_horizon_miles / total_length_miles
    
    # Keep fraction between 0 and 1
    fraction <- max(0, min(1, fraction))
    
    # High entrainment reach
    high_line <- lwgeom::st_linesubstring(
      centerline,
      from = 0,
      to = fraction
    )
    
    # Low entrainment reach
    low_line <- lwgeom::st_linesubstring(
      centerline,
      from = fraction,
      to = 1
    )
    
    # Event Horizon point
    eh_point <- lwgeom::st_linesubstring(
      centerline,
      from = fraction,
      to = fraction
    )
    
    return(list(
      high_line = high_line,
      low_line  = low_line,
      eh_point  = eh_point,
      fraction  = fraction
    ))
  }
  make_eh_transect <- function(
    centerline,
    fraction,
    length_m = 30000
  ) {
    
    coords <- st_coordinates(centerline)
    
    idx <- max(
      2,
      round(fraction * nrow(coords))
    )
    
    p1 <- coords[idx - 1, c("X","Y")]
    p2 <- coords[idx, c("X","Y")]
    
    dx <- p2[1] - p1[1]
    dy <- p2[2] - p1[2]
    
    seg_length <- sqrt(dx^2 + dy^2)
    
    dx <- dx / seg_length
    dy <- dy / seg_length
    
    # Perpendicular vector
    
    px <- -dy
    py <- dx
    
    midpoint <- p2
    
    start <- c(
      midpoint[1] - 50000,
      midpoint[2]
    )
    
    end <- c(
      midpoint[1] + 50000,
      midpoint[2]
    )
    
    st_sfc(
      st_linestring(
        rbind(start, end)
      ),
      crs = st_crs(centerline)
    )
    
  }
  # -----------------------------
  # Helper: event horizon distance reactive
  # -----------------------------
  event_horizon_distance <- reactive({
    
    target_date <- selected_event_date()
    
    horizon_file <- if (
      input$tabs %in% c(
        "current30_event"
      )
    ) {
      eventhorizon30
    } else {
      eventhorizon7
    }
    
    row <- horizon_file %>%
      filter(
        date == target_date,
        RISK == as.numeric(input$eh_risk)
      )
    
    req(nrow(row) > 0)
    
    row$event_horizon_distance[1]
  })
  
  eh_geom <- reactive({
    
    make_event_geometry(
      event_horizon_distance(),
      river_centerline,
      river_length_miles
    )
  })
  
  # -----------------------------
  # Helper: zone helper
  # -----------------------------
  
  event_zones <- reactive({
    
    geom <- eh_geom()
    
    # Create Event Horizon transect
    transect <- make_eh_transect(
      river_centerline,
      geom$fraction
    )
    
    # Split Delta polygon
    split_result <- lwgeom::st_split(
      delta_boundary,
      transect
    )
    
    # Extract resulting polygons
    parts <- st_collection_extract(
      split_result,
      "POLYGON"
    )
    
    parts <- st_as_sf(parts)
    
    # -----------------------------
    # Diagnostic Output
    # -----------------------------
    cat("\n")
    cat("Number of split polygons:", nrow(parts), "\n")
    
    # Calculate areas
    parts$area <- as.numeric(st_area(parts))
    
    print(
      parts$area
    )
    
    # Sort by area
    parts <- parts %>%
      arrange(desc(area))
    
    # -----------------------------
    # Keep largest polygons only
    # -----------------------------
    # Usually the first two are the
    # north/south Delta pieces
    
    if (nrow(parts) >= 2) {
      
      major_parts <- parts[1:2, ]
      
    } else {
      
      major_parts <- parts
      
    }
    
    # -----------------------------
    # Determine north vs south
    # using centroid Y coordinate
    # -----------------------------
    
    centroids <- st_centroid(
      major_parts
    )
    
    major_parts$cy <- st_coordinates(
      centroids
    )[,2]
    
    low_zone <- major_parts %>%
      filter(
        cy == max(cy)
      )
    
    high_zone <- major_parts %>%
      filter(
        cy == min(cy)
      )
    
    list(
      high_zone = high_zone,
      low_zone = low_zone,
      transect = transect,
      polygons = parts
    )
    
  })
  # -----------------------------
  # Helper: end horizon point helper
  # -----------------------------
  get_event_point <- function(
    risk_value,
    horizon = "7"
  ) {
    
    target_date <- selected_event_date()
    
    df <- if (horizon == "7") {
      eventhorizon7
    } else {
      eventhorizon30
    }
    
    df %>%
      filter(
        date == target_date,
        RISK == risk_value
      )
    
  }
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
  # Helper: Event Horizon Plot 
  # -----------------------------
  get_eh_background <- function(risk_value) {
    
    color_col <- paste0(
      "Horizon_",
      risk_value
    )
    
    eh_baseline %>%
      transmute(
        EXP = CCF + TPP,
        VER = VNS,
        predicted_event_horizon_distance =
          .data[[color_col]]
      ) %>%
      filter(
        !is.na(predicted_event_horizon_distance)
      ) %>%
      slice_sample(
        n = 10000
      )
    
  }
  make_event_horizon_scatter <- function(
    risk_value,
    title,
    horizon = "7"
  ) {    
    background_df <- get_eh_background(
      risk_value
    )
    
    point_df <- get_event_point(
      risk_value,
      horizon
    )
    
    ggplot(
      background_df,
      aes(
        x = EXP,
        y = VER,
        color = predicted_event_horizon_distance
      )
    ) +
      
      geom_point(
        size = 2,
        alpha = 0.35
      ) +
      
      geom_point(
        data = point_df,
        aes(
          x = EXP,
          y = VER
        ),
        inherit.aes = FALSE,
        shape = 21,
        size = 7,
        fill = "red",
        color = "white",
        stroke = 2
      ) +
      
      scale_color_viridis_c(
        option = "viridis",
        direction = 1,
        limits = c(
          global_eh_min,
          global_eh_max
        ),
        name = "Predicted Event\nHorizon (miles)"
      ) +
      
      labs(
        title = title,
        x = "Export (cfs)"
      ) +
      
      scale_y_log10(
        name = "Vernalis (cfs)",
        breaks = c(
          1000,
          2000,
          5000,
          10000,
          20000
        ),
        labels = scales::comma
      ) +
      
      theme_bw()
  }
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
    print("EVENT TS CALLED")
    
    print(input$tabs)
    
    print(input$scenario)
    
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
        data = event_zones()$low_zone,
        fillColor = "#a9d4e6",
        fillOpacity = 0.5,
        color = "#427799",
        weight = 1,
        group = "Low Zone"
      ) %>%
      
      addPolygons(
        data = event_zones()$high_zone,
        fillColor = "#e8b5b5",
        fillOpacity = 0.6,
        color = "#a74a4a",
        weight = 1,
        group = "High Zone"
      ) %>%
      
      addPolylines(
        data = eh_geom()$high_line,
        color = "red",
        weight = 7
      ) %>%
      
      addPolylines(
        data = eh_geom()$low_line,
        color = "grey40",
        weight = 4
      ) %>%
      
      addCircleMarkers(
        data = eh_geom()$eh_point,
        radius = 8,
        color = "red",
        fillOpacity = 1
      )%>%
    
      addPolylines(
        data = delta_channels,
        color = "#2b8cbe",
        weight = 1,
        opacity = 0.6,
        group = "Channels"
      ) %>%
      
      addPolylines(
        data = event_zones()$transect,
        color = "black",
        weight = 4,
        opacity = 0.8,
        group = "Event Horizon"
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
      )%>%
      addLabelOnlyMarkers(
        lng = -121.55,
        lat = 38.15,
        label = "High Entrainment Zone",
        labelOptions = labelOptions(
          noHide = TRUE,
          textOnly = TRUE
        )
      ) %>%
      
      fitBounds(
        lng1 = -122.15,
        lat1 = 37.75,
        lng2 = -121.15,
        lat2 = 38.85
      )%>%
      
      addLabelOnlyMarkers(
        lng = -121.65,
        lat = 38.55,
        label = "Low Entrainment Zone",
        labelOptions = labelOptions(
          noHide = TRUE,
          textOnly = TRUE
        )
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
  
  output$current7_event_scatter15 <- renderPlot({
    
    make_event_horizon_scatter(
      15,
      "Current 7d Average Flow - Event Horizon - 15% Risk",
      horizon = "7"
    )
    
  })
  
  output$current7_event_scatter30 <- renderPlot({
    
    make_event_horizon_scatter(
      30,
      "Current 7d Average Flow - Event Horizon - 30% Risk",
      horizon = "7"
    )
    
  })
  
  output$current7_event_scatter80 <- renderPlot({
    
    make_event_horizon_scatter(
      80,
      "Current 7d Average Flow - Event Horizon - 80% Risk",
      horizon = "7"
    )
    
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
  
  output$current30_event_scatter15 <- renderPlot({
    make_event_horizon_scatter(
      15,
      "Current 7d Average Flow - Event Horizon - 15% Risk",
      horizon = "30"
    )
  })
  
  output$current30_event_scatter30 <- renderPlot({
    make_event_horizon_scatter(
      30,
      "Current 7d Average Flow - Event Horizon - 30% Risk",
      horizon = "30"
    )
  })
  
  output$current30_event_scatter80 <- renderPlot({
    make_event_horizon_scatter(
      80,
      "Current 7d Average Flow - Event Horizon - 80% Risk",
      horizon = "30"
    )
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
  
  output$forecast7_event_scatter15 <- renderPlot({
    make_event_horizon_scatter(
      15,
      "Forecast 7d Average Flow - Event Horizon - 15% Risk",
      horizon = "7"
    )
  })
  
  output$forecast7_event_scatter30 <- renderPlot({
    make_event_horizon_scatter(
      30,
      "Forecast 7d Average Flow - Event Horizon - 30% Risk",
      horizon = "7"
    )
  })
  
  output$forecast7_event_scatter80 <- renderPlot({
    make_event_horizon_scatter(
      80,
      "Forecast 7d Average Flow - Event Horizon - 80% Risk",
      horizon = "7"
    )
  })
}
