library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(viridis)
library(sf)
library(leaflet)
library(lwgeom)
<<<<<<< HEAD
library(gstat)
library(terra)
library(sp)
=======
library(gt)
>>>>>>> 5f2f0be (update About page)

server <- function(input, output, session) {
  find_master_file <- function(run_folder) {
    
    candidate_files <- list.files(
      path = file.path(RUN_BASE_DIR, run_folder),
      pattern = "^All_PTM_ECOPTM_Event_Horizon_Results.*\\.xlsx$",
      full.names = TRUE
    )
    
    req(length(candidate_files) > 0)
    
    candidate_files[1]
    
  }
  # =====================================================
  # Available Model Run Dates
  # =====================================================
  # Each model run lands in its own dated subfolder, e.g.
  #   ../STN_EMULATOR/Output/20260623/All_PTM_ECOPTM_Event_Horizon_Results.xlsx
  #   ../STN_EMULATOR/Output/20260224/All_PTM_ECOPTM_Event_Horizon_Results.xlsx
  # The folder name (YYYYMMDD) becomes a selectable "Model Run Date" in
  # the sidebar; whichever one is selected determines which xlsx file
  # drives the entire app.
  # -----------------------------------------------------
  RUN_BASE_DIR   <- "../STN_EMULATOR/Output"
  RUN_FILENAME   <- "All_PTM_ECOPTM_Event_Horizon_Results.xlsx"
  
  list_available_runs <- function(base_dir = RUN_BASE_DIR) {
    
    run_dirs <- list.dirs(base_dir, full.names = FALSE, recursive = FALSE)
    run_dirs <- run_dirs[grepl("^[0-9]{8}$", run_dirs)]
    
    if (length(run_dirs) == 0) {
      stop("No dated run folders (YYYYMMDD) found under ", base_dir)
    }
    
    data.frame(
      folder   = run_dirs,
      run_date = as.Date(run_dirs, format = "%Y%m%d"),
      stringsAsFactors = FALSE
    ) %>%
      arrange(desc(run_date))
  }
  
  available_runs <- list_available_runs()
  
  # Sorts scenario labels numerically where possible ("-5000" before
  # "-3500"), with non-numeric labels like "baseline" sorted last.
  sort_scenarios <- function(x) {
    numeric_scenarios <- suppressWarnings(as.numeric(x))
    x[order(is.na(numeric_scenarios), numeric_scenarios, x)]
  }
  
  updateSelectInput(
    session,
    "run_date",
    choices = setNames(
      available_runs$folder,
      format(available_runs$run_date, "%b %d, %Y")
    ),
    selected = available_runs$folder[1]
  )
  
  # =====================================================
  # Load Master Results File (reactive on the selected run date)
  # =====================================================
  master_data <- reactive({
    
    req(input$run_date)
    
    master_path <- find_master_file(
      input$run_date
    )
    req(file.exists(master_path))
    
    # -----------------------------
    # Combined model results (long format):
    # one row per PTM_scenario x Forecast_scenario x Model x
    # DSM2_Node (if applicable) x Risk_Level_Percent (if applicable)
    # -----------------------------
    combined <- read_excel(master_path, sheet = "Combined_Results") %>%
      mutate(
        DSM2_Node         = as.character(DSM2_Node),
        Forecast_scenario = as.character(Scenario),
        Start_Date        = as.Date(Start_Date),
        End_Date          = as.Date(End_Date)
      )
    
    # -----------------------------
    # Node metadata (location name, region, lat/lon) --
    # this replaces the old nodes.shp join, since the master
    # file now carries coordinates directly.
    # -----------------------------
    node_meta <- read_excel(master_path, sheet = "PTM_Results") %>%
      mutate(DSM2_Node = as.character(DSM2_Node)) %>%
      filter(!is.na(DSM2_Node)) %>%
      distinct(DSM2_Node, .keep_all = TRUE) %>%
      select(DSM2_Node, Location, Region, X, Y)
    
    # -----------------------------
    # Scenario date windows (for the informational "Data Window" display)
    # -----------------------------
    scenario_inputs <- read_excel(master_path, sheet = "Scenario_Inputs") %>%
      mutate(
        Start_Date = as.Date(Start_Date),
        End_Date   = as.Date(End_Date)
      )
    
    # -----------------------------
    # Forecast scenario choices, auto-detected from the file
    # (baseline, -3500, -5000, ... whatever exists)
    # -----------------------------
    forecast_scenario_choices <- combined %>%
      filter(Emulator_Scenario == "Forecast average") %>%
      pull(Forecast_scenario) %>%
      unique() %>%
      sort_scenarios()
    
    list(
      combined                  = combined,
      node_meta                 = node_meta,
      scenario_inputs           = scenario_inputs,
      forecast_scenario_choices = forecast_scenario_choices
    )
  })
  
  # -----------------------------
  # Refresh the Node selector whenever the master data changes
  # (a different run may have a different node set)
  # -----------------------------
  observeEvent(master_data(), {
    
    md <- master_data()
    
    all_nodes <- md$combined %>%
      filter(!is.na(DSM2_Node)) %>%
      distinct(DSM2_Node) %>%
      left_join(md$node_meta, by = "DSM2_Node") %>%
      arrange(as.numeric(DSM2_Node))
    
    updateSelectInput(
      session,
      "node",
      choices = setNames(
        all_nodes$DSM2_Node,
        paste0(all_nodes$DSM2_Node, " \u2013 ", all_nodes$Location)
      ),
      selected = all_nodes$DSM2_Node[1]
    )
  })
  
  # -----------------------------
  # Event Horizon background "cloud" -- the master file only has
  # one EXP/VER point per scenario, so we keep the old baseline
  # distribution file to give the scatter plots visual context.
  # -----------------------------
  eh_baseline <- read_csv(
    "../STN_EMULATOR/EH_baseline.csv",
    show_col_types = FALSE
  )
  
  global_eh_min <- min(
    c(eh_baseline$Horizon_25, eh_baseline$Horizon_50, eh_baseline$Horizon_75),
    na.rm = TRUE
  )
  
  global_eh_max <- max(
    c(eh_baseline$Horizon_25, eh_baseline$Horizon_50, eh_baseline$Horizon_75),
    na.rm = TRUE
  )
  
  # -----------------------------
  # Load Spatial Data (boundary / channel polygons still come
  # from shapefiles -- the xlsx has no polygon geometry)
  # -----------------------------
  delta_boundary <- st_read("../STN_EMULATOR/shapefiles/Bay_Delta_Poly_New.shp", quiet = TRUE)
  delta_channels <- st_read("../STN_EMULATOR/shapefiles/hydro_delta_marsh.shp", quiet = TRUE)
  river_centerline <- st_read("../STN_EMULATOR/shapefiles/CCF_OldRiver_CL.shp", quiet = TRUE)
  river_length_m <- as.numeric(st_length(river_centerline))
  river_length_miles <- river_length_m / 1609.344
  
  delta_boundary <- st_transform(delta_boundary, 26910)
  delta_channels <- st_transform(delta_channels, 26910)
  river_centerline <- st_transform(river_centerline, 26910)
  
  # =====================================================
  # Initialize Inputs
  # =====================================================
  
  # -----------------------------
  # Data Window display (replaces the old dateRangeInput filter --
  # each scenario is already a fixed averaging window, so there's
  # nothing left to filter by date; this just tells you what window
  # is currently active)
  # -----------------------------
  tab_to_ptm_scenario <- function(tabname) {
    if (grepl("^current7", tabname)) {
      "Historic 7-day average"
    } else if (grepl("^current30", tabname)) {
      "Historic 30-day average"
    } else if (grepl("^forecast7", tabname)) {
      "Forecast average"
    } else {
      NA_character_
    }
  }
  
  output$date_window <- renderUI({
    
    req(input$tabs)
    
    md <- master_data()
    
    ptm_scenario <- tab_to_ptm_scenario(input$tabs)
    req(ptm_scenario)
    
    win <- md$scenario_inputs %>% filter(Scenario == ptm_scenario)
    req(nrow(win) > 0)
    
    tags$div(
      tags$label("Data Window:", class = "control-label"),
      tags$p(
        paste0(
          format(win$Start_Date[1], "%b %d, %Y"), " – ",
          format(win$End_Date[1], "%b %d, %Y"),
          " (", win$Number_of_Days[1], "-day average)"
        ),
        style = "margin-top:-8px; color:#334; font-size:13px;"
      )
    )
  })
  
  # -----------------------------
  # Scenario Control
  # -----------------------------
  output$scenario_control <- renderUI({
    
    req(input$tabs)
    
    md <- master_data()
    
    if (grepl("^forecast7", input$tabs)) {
      selectInput(
        "scenario",
        "Scenario:",
        choices = md$forecast_scenario_choices,
        selected = md$forecast_scenario_choices[1]
      )
    } else {
      tagList(
        selectInput(
          "scenario",
          "Scenario:",
          choices = "Historic",
          selected = "Historic"
        ),
        tags$small(
          "Current condition pages use Historic scenario.",
          style = "color:#666;"
        )
      )
    }
  })
  
  # =====================================================
  # Core reactive: rows from Combined_Model_Results for the
  # currently active tab + scenario
  # =====================================================
  active_results <- reactive({
    
    req(input$tabs)
    
    md <- master_data()
    
    ptm_scenario <- tab_to_ptm_scenario(input$tabs)
    req(ptm_scenario)
    
    scen <- if (grepl("^forecast7", input$tabs)) {
      req(input$scenario)
      input$scenario
    } else {
      "Historic"
    }
    
    md$combined %>%
      filter(Emulator_Scenario == ptm_scenario, Scenario == scen)
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
    eh_point,
    centerline
  ) {
    
    midpoint <- st_coordinates(
      eh_point
    )[1,]
    
    start <- c(
      midpoint[1] - 2,
      midpoint[2]
    )
    
    end <- c(
      midpoint[1] + 2,
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
  # Event Horizon rows in Combined_Model_Results already carry the
  # EXP/VER inputs and Prediction_Final (miles) for the active
  # scenario + risk level -- no date lookup needed anymore.
  # -----------------------------
  event_horizon_distance <- reactive({
    
    req(input$eh_risk)
    
    row <- active_results() %>%
      filter(Model == "Event Horizon", Risk_Level_Percent == as.numeric(input$eh_risk))
    
    req(nrow(row) > 0)
    
    row$Prediction_Final[1]
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
    
    nodes <- map_data_analysis()
    
    threshold <- as.numeric(
      input$eh_risk
    )
    
    req(nrow(nodes) > 3)
    print(st_crs(nodes))
    
    print(st_crs(delta_boundary))
    # --------------------------------
    # Build interpolation grid
    # --------------------------------
    
    bbox <- st_bbox(delta_boundary)
    
    grid <- expand.grid(
      x = seq(
        bbox["xmin"],
        bbox["xmax"],
        length.out = 150
      ),
      y = seq(
        bbox["ymin"],
        bbox["ymax"],
        length.out = 150
      )
    )
    
    grid <- st_as_sf(
      grid,
      coords = c("x","y"),
      crs = st_crs(nodes)
    )
    sp_grid <- as(grid, "Spatial")
    sp_nodes <- as(nodes, "Spatial")
    # --------------------------------
    # IDW interpolation
    # --------------------------------
    
    
    names(sp_nodes)[
      names(sp_nodes) == "entrainment"
    ] <- "z"
    
    idw_surface <- gstat::idw(
      z ~ 1,
      sp_nodes,
      newdata = sp_grid
    )
    
    pred <- st_as_sf(idw_surface)
    
    pred <- pred %>%
      rename(
        entrainment = var1.pred
      )
    
    # --------------------------------
    # High-risk contour
    # --------------------------------
    
    high_pts <- pred %>%
      filter(
        entrainment >= threshold
      )
    
    req(
      nrow(high_pts) > 10
    )
    high_pts <- pred %>%
      filter(
        entrainment >= threshold
      )
    high_pts <- high_pts %>%
      st_intersection(delta_boundary)
    high_zone <- high_pts %>%
      st_union() %>%
      sf::st_concave_hull(
        ratio = 0.5
      )
    
    # Clip to Delta
    
    high_zone <- st_intersection(
      st_as_sf(high_zone),
      delta_boundary
    )
    
    low_zone <- st_difference(
      delta_boundary,
      st_union(high_zone)
    )
    
    list(
      high_zone = st_transform(
        high_zone,
        4326
      ),
      low_zone = st_transform(
        low_zone,
        4326
      )
    )
    
  })
  
  # -----------------------------
  # Helper: Event Horizon point for the scatter plots
  # -----------------------------
  get_event_point <- function(risk_value) {
    
    active_results() %>%
      filter(Model == "Event Horizon", Risk_Level_Percent == risk_value)
  }
  
  # -----------------------------
  # Helper: Event Horizon background "cloud"
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
    title
  ) {    
    background_df <- get_eh_background(
      risk_value
    )
    
    point_df <- get_event_point(
      risk_value
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
  # Helper: node-level entrainment data for the map
  # (7-day model on 7-day tabs / forecast tabs, 30-day
  # model on the 30d Average Flow event horizon tab)
  # -----------------------------
  map_entrainment_data <- reactive({
    
    req(input$tabs)
    
    req(input$event_ptm_model)
    
    model <- input$event_ptm_model
    
    active_results() %>%
      filter(
        Model == model,
        !is.na(DSM2_Node)
      ) %>%
      group_by(
        DSM2_Node
      ) %>%
      summarise(
        entrainment = mean(
          Prediction_Final,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      left_join(
        master_data()$node_meta,
        by = "DSM2_Node"
      )
  })
  
  map_data_analysis <- reactive({
    df <- map_entrainment_data()
    st_as_sf(
      df,
      coords = c("X","Y"),
      crs = 4326,
      remove = FALSE
    ) %>%
      st_transform(26910)
    
  })
  
  map_data_display <- reactive({
    
    st_transform(
      map_data_analysis(),
      4326
    )
    
  })
  
  # -----------------------------
  # Plot Helpers
  # -----------------------------
  
  # Bar chart of entrainment by node. The master file provides one
  # value per node per averaging window (not a daily time series),
  # so this replaces the old line-chart-over-time.
  make_entrainment_bar_plot <- function(model_name, title) {
    
    df <- active_results() %>%
      filter(
        Model == model_name,
        !is.na(DSM2_Node)
      ) %>%
      group_by(
        DSM2_Node,
        Output_Metric,
        Output_Unit
      ) %>%
      summarise(
        Prediction_Final = mean(
          Prediction_Final,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      left_join(
        master_data()$node_meta,
        by = "DSM2_Node"
      )
    
    validate(
      need(nrow(df) > 0, "No data available for the selected filters.")
    )
    
    df <- df %>%
      mutate(
        node_label = paste0(DSM2_Node, " - ", Location),
        is_selected = !is.null(input$node) & DSM2_Node == input$node
      ) %>%
      arrange(Prediction_Final)
    
    df$node_label <- factor(
      df$node_label,
      levels = unique(df$node_label)
    )
    
    ggplot(df, aes(x = node_label, y = Prediction_Final, fill = is_selected)) +
      geom_col() +
      scale_fill_manual(
        values = c(`TRUE` = "#0a7e8c", `FALSE` = "#a9d4e6"),
        guide = "none"
      ) +
      coord_flip() +
      labs(
        title = title,
        x = "Node",
        y = paste0(df$Output_Metric[1], " (", df$Output_Unit[1], ")")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          size = 16,
          face = "bold"
        ),
        
        axis.text.y = element_text(
          size = 14
        ),
        
        axis.text.x = element_text(
          size = 10
        ),
        
        axis.title = element_text(
          size = 13
        ),
        plot.margin = margin(
          t = 10,
          r = 10,
          b = 10,
          l = 120
        )
      )
  }
  
  # Detail table for whichever node is selected in the sidebar
  make_summary_table <- function(model_name) {
    
    df <- active_results() %>%
      filter(Model == model_name, !is.na(DSM2_Node)) %>%
      left_join(master_data()$node_meta, by = "DSM2_Node")
    
    validate(
      need(nrow(df) > 0, "No data available for the selected filters.")
    )
    
    df <- df %>%
      mutate(rank = rank(-Prediction_Final, ties.method = "min"))
    
    sel <- df %>% filter(DSM2_Node == input$node)
    if (nrow(sel) == 0) sel <- df[1, ]
    
    data.frame(
      Node              = sel$DSM2_Node,
      Location          = sel$Location,
      Region            = sel$Region,
      Scenario          = sel$Emulator_Scenario,
      Forecast_Scenario = sel$Scenario,
      Window            = paste(format(sel$Start_Date), "to", format(sel$End_Date)),
      Metric            = sel$Output_Metric,
      Value             = as.integer(round(sel$Prediction_Final, 0)),
      Unit              = sel$Output_Unit,
      Rank              = paste0(sel$rank, " of ", nrow(df))
    )
  }
  
  # ECO PTM outputs are Delta-wide (not per node)
  make_ecoptm_table <- function() {
    
    active_results() %>%
      filter(Model %in% c("ECO-PTM Survival", "ECO-PTM Interior Routing")) %>%
      transmute(
        Scenario          = Emulator_Scenario,
        Forecast_Scenario = Scenario,
        Window            = paste(format(Start_Date), "to", format(End_Date)),
        Metric            = Output_Metric,
        Value             = as.integer(round(Prediction_Final, 0)),
        Unit              = Output_Unit,
        Exports               = as.integer(round(EXP, 0)),
        Vernailis               = as.integer(round(VER, 0))
      )
  }
  
  make_event_map <- function() {
    
    df <- map_data_display()
    eh_display <- list(
      high_line = st_transform(
        eh_geom()$high_line,
        4326
      ),
      low_line = st_transform(
        eh_geom()$low_line,
        4326
      ),
      eh_point = st_transform(
        eh_geom()$eh_point,
        4326
      )
    )
    pal <- colorNumeric(
      palette = "viridis",
      domain = range(df$entrainment, na.rm = TRUE),
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
        data = eh_display$high_line,
        color = "red",
        weight = 6,
        opacity = 1
      ) %>%
      
      addPolylines(
        data = eh_display$low_line,
        color = "grey40",
        weight = 3,
        opacity = 0.8
      ) %>%
      
      
      addCircleMarkers(
        data = eh_display$eh_point,
        radius = 9,
        color = "red",
        weight = 3,
        fillColor = "red",
        fillOpacity = 1,
        label = paste0(
          "Event Horizon: ",
          round(
            event_horizon_distance(),
            1
          ),
          " miles"
        )
      )%>%
      
      addPolylines(
        data = delta_channels,
        color = "#2b8cbe",
        weight = 1,
        opacity = 0.6,
        group = "Channels"
      ) %>%
      
      
      addCircleMarkers(
        data = df,
        radius = ~ifelse(is.na(entrainment), 3, 4 + entrainment / 100 * 10),
        color = ~pal(entrainment),
        fillOpacity = 0.9,
        stroke = FALSE,
        group = "Nodes",
        label = ~paste0(
          "Node: ", DSM2_Node,
          "<br>Location: ", Location,
          "<br>Region: ", Region,
          "<br>Entrainment: ", round(entrainment, 1), "%"
        )
      ) %>%
      
      addLegend(
        "bottomright",
        pal = pal,
        values = df$entrainment,
        title = "Entrainment (%)",
        opacity = 1
      ) %>%
      addLegend(
        position = "topright",
        colors = c("#e8b5b5", "#a9d4e6"),
        labels = c(
          "High Entrainment Zone",
          "Low Entrainment Zone"
        ),
        title = "Risk Zones",
        opacity = 0.7
      )%>%
      addLayersControl(
        overlayGroups = c("Boundary", "Channels", "Nodes"),
        options = layersControlOptions(collapsed = FALSE)
      )%>%
      
      
      fitBounds(
        lng1 = -122.15,
        lat1 = 37.75,
        lng2 = -121.15,
        lat2 = 38.85
      )
    
  }
  
  # -----------------------------
  # Current 7d Average Flow Outputs
  # -----------------------------
  output$current7_ptm7_plot <- renderPlot({
    make_entrainment_bar_plot("PTM 7-Day Entrainment", "Current 7d Average Flow - PTM 7d Entrainment")
  })
  
  output$current7_ptm7_summary <- renderTable({
    make_summary_table("PTM 7-Day Entrainment")
  })
  
  output$current7_ptm30_plot <- renderPlot({
    make_entrainment_bar_plot("PTM 30-Day Entrainment", "Current 7d Average Flow - PTM 30d Entrainment")
  })
  
  output$current7_ptm30_summary <- renderTable({
    make_summary_table("PTM 30-Day Entrainment")
  })
  
  output$current7_ecoptm_table <- renderTable({
    make_ecoptm_table()
  })
  
  output$current7_event_map <- renderLeaflet({
    make_event_map()
  })
  
  output$current7_event_scatter25 <- renderPlot({
    make_event_horizon_scatter(25, "Current 7d Average Flow - Event Horizon - 25% Risk")
  })
  
  output$current7_event_scatter50 <- renderPlot({
    make_event_horizon_scatter(50, "Current 7d Average Flow - Event Horizon - 50% Risk")
  })
  
  output$current7_event_scatter75 <- renderPlot({
    make_event_horizon_scatter(75, "Current 7d Average Flow - Event Horizon - 75% Risk")
  })
  
  # -----------------------------
  # Current 30d Average Flow Outputs
  # -----------------------------
  output$current30_ptm7_plot <- renderPlot({
    make_entrainment_bar_plot("PTM 7-Day Entrainment", "Current 30d Average Flow - PTM 7d Entrainment")
  })
  
  output$current30_ptm7_summary <- renderTable({
    make_summary_table("PTM 7-Day Entrainment")
  })
  
  output$current30_ptm30_plot <- renderPlot({
    make_entrainment_bar_plot("PTM 30-Day Entrainment", "Current 30d Average Flow - PTM 30d Entrainment")
  })
  
  output$current30_ptm30_summary <- renderTable({
    make_summary_table("PTM 30-Day Entrainment")
  })
  
  output$current30_ecoptm_table <- renderTable({
    make_ecoptm_table()
  })
  
  output$current30_event_map <- renderLeaflet({
    make_event_map()
  })
  
  output$current30_event_scatter25 <- renderPlot({
    make_event_horizon_scatter(25, "Current 30d Average Flow - Event Horizon - 25% Risk")
  })
  
  output$current30_event_scatter50 <- renderPlot({
    make_event_horizon_scatter(50, "Current 30d Average Flow - Event Horizon - 50% Risk")
  })
  
  output$current30_event_scatter75 <- renderPlot({
    make_event_horizon_scatter(75, "Current 30d Average Flow - Event Horizon - 75% Risk")
  })
  
  # -----------------------------
  # Forecast 7d Average Flow Outputs
  # -----------------------------
  output$forecast7_ptm7_plot <- renderPlot({
    make_entrainment_bar_plot("PTM 7-Day Entrainment", "Forecast 7d Average Flow - PTM 7d Entrainment")
  })
  
  output$forecast7_ptm7_summary <- renderTable({
    make_summary_table("PTM 7-Day Entrainment")
  })
  
  output$forecast7_ptm30_plot <- renderPlot({
    make_entrainment_bar_plot("PTM 30-Day Entrainment", "Forecast 7d Average Flow - PTM 30d Entrainment")
  })
  
  output$forecast7_ptm30_summary <- renderTable({
    make_summary_table("PTM 30-Day Entrainment")
  })
  
  output$forecast7_ecoptm_table <- renderTable({
    make_ecoptm_table()
  })
  
  output$forecast7_event_map <- renderLeaflet({
    make_event_map()
  })
  
  output$forecast7_event_scatter25 <- renderPlot({
    make_event_horizon_scatter(25, "Forecast 7d Average Flow - Event Horizon - 25% Risk")
  })
  
  output$forecast7_event_scatter50 <- renderPlot({
    make_event_horizon_scatter(50, "Forecast 7d Average Flow - Event Horizon - 50% Risk")
  })
  
  output$forecast7_event_scatter75 <- renderPlot({
    make_event_horizon_scatter(75, "Forecast 7d Average Flow - Event Horizon - 75% Risk")
  })
  
  output$about_info_table <- render_gt({
    data <- data.frame(
      Name = c("Adam Witt", "Puneet Khatavkar", "Jo Anna Beck", "Josh Israel"),
      Role = c("Activity Lead","Quality review", "Contracting Officers Representative", "Reclamation Activity Oversight"),
      Email = c("adam.witt@stantec.com", "", "jbeck@usbr.gov", "jaisrael@usbr.gov")
    )
    about_info_table <- gt(data)
    about_info_table |>
      tab_row_group(
        label = html("<strong><em>Bureau of Reclamation,</strong><br>Bay-Delta Office</em>"), 
        rows = 3:4
      )|>
      tab_row_group(
        label = html("<strong><em>Stantec Inc.,</strong><br>Sacramento C St. Office</em>"), 
        rows = 1:2
      )|>
      cols_align(
        align = "center",
        columns = 2:3
      )|>
      tab_style(
        style = cell_text(align = "center", size = "medium"),
        locations = cells_row_groups()
      )|>
      tab_style(
        style = cell_text(align = "center", size = "medium"),
        locations = cells_row_groups()
      )|>
      tab_style(
        style = cell_text(weight = "bold", align = "center", size = "large"),
        locations = cells_column_labels(columns = 1:3)
      )
  })
  
}
