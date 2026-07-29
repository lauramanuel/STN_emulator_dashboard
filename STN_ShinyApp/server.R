
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(viridis)
library(sf)
library(leaflet)
library(lwgeom)
library(gstat)
library(sp)
library(plotly)
library(lightgbm)
library(xgboost)

MODEL_DIR <- "STN_EMULATOR/models"
OUTPUT_DIR <- "STN_EMULATOR/Output"
SHAPE_DIR <- "STN_EMULATOR/shapefiles"

ptm7_model <- lgb.load(file.path(MODEL_DIR, "PTM_Entrainment7d_lightgbm.txt"))
ptm30_model <- lgb.load(file.path(MODEL_DIR, "PTM_Entrainment30d_lightgbm.txt"))
eco_survival_model <- lgb.load(file.path(MODEL_DIR, "ECOPTM_survival_lightgbm.txt"))
eco_interior_model <- lgb.load(file.path(MODEL_DIR, "ECOPTM_interior_lightgbm.txt"))
event_horizon_model <- xgb.load(file.path(MODEL_DIR, "xgb_event_horizon.json"))

bound_percent <- function(x) pmax(0, pmin(100, x))

read_lgb_features <- function(model_file) {
  header <- readLines(model_file, n = 60, warn = FALSE)
  feature_line <- grep("^feature_names=", header, value = TRUE)
  if (length(feature_line) != 1) {
    stop("Could not read LightGBM feature names from: ", model_file)
  }
  strsplit(sub("^feature_names=", "", feature_line), "\\s+")[[1]]
}

eco_survival_features <- read_lgb_features(
  file.path(MODEL_DIR, "ECOPTM_survival_lightgbm.txt")
)
eco_interior_features <- read_lgb_features(
  file.path(MODEL_DIR, "ECOPTM_interior_lightgbm.txt")
)

server <- function(input, output, session) {

  find_latest_master <- function() {
    files <- list.files(
      OUTPUT_DIR,
      pattern = "^All_PTM_ECOPTM_Event_Horizon_Results.*\\.xlsx$",
      full.names = TRUE,
      recursive = TRUE
    )
    validate(need(length(files) > 0, "No master results workbook was found."))
    files[which.max(file.info(files)$mtime)]
  }

  reference_data <- reactive({
    master_path <- find_latest_master()

    combined <- read_excel(master_path, sheet = "Combined_Results") %>%
      mutate(DSM2_Node = as.character(DSM2_Node))

    node_meta <- read_excel(master_path, sheet = "PTM_Results") %>%
      mutate(DSM2_Node = as.character(DSM2_Node)) %>%
      filter(!is.na(DSM2_Node)) %>%
      distinct(DSM2_Node, .keep_all = TRUE) %>%
      select(DSM2_Node, Location, Region, X, Y)

    nodes_7d <- combined %>%
      filter(Model == "PTM 7-Day Entrainment", !is.na(DSM2_Node)) %>%
      distinct(DSM2_Node) %>%
      arrange(as.numeric(DSM2_Node)) %>%
      pull(DSM2_Node)

    nodes_30d <- combined %>%
      filter(Model == "PTM 30-Day Entrainment", !is.na(DSM2_Node)) %>%
      distinct(DSM2_Node) %>%
      arrange(as.numeric(DSM2_Node)) %>%
      pull(DSM2_Node)

    list(
      node_meta = node_meta,
      nodes_7d = nodes_7d,
      nodes_30d = nodes_30d
    )
  })

  eh_baseline <- read_csv(
    file.path("STN_EMULATOR", "EH_baseline.csv"),
    show_col_types = FALSE
  )

  delta_boundary <- st_read(
    file.path(SHAPE_DIR, "Bay_Delta_Poly_New.shp"),
    quiet = TRUE
  ) %>% st_transform(26910)

  delta_channels <- st_read(
    file.path(SHAPE_DIR, "hydro_delta_marsh.shp"),
    quiet = TRUE
  ) %>% st_transform(26910)

  river_centerline <- st_read(
    file.path(SHAPE_DIR, "CCF_OldRiver_CL.shp"),
    quiet = TRUE
  ) %>% st_transform(26910)

  river_length_miles <- as.numeric(st_length(river_centerline)) / 1609.344

  saved_runs <- reactiveVal(list())

  add_saved_run <- function(
    name,
    condition,
    family,
    data
  ) {
    
    current <- saved_runs()
    
    run_number <-
      length(current) + 1
    
    time_stamp <- format(
      Sys.time(),
      "%H:%M:%S"
    )
    
    key <- paste0(
      run_number,
      ". ",
      condition,
      " | ",
      name,
      " | ",
      family,
      " | ",
      time_stamp
    )
    
    data$Saved_Run_ID <-
      key
    
    current[[key]] <-
      data
    
    saved_runs(
      current
    )
    
    key
  }
  

  make_ptm_results <- function(condition, scenario_name, exp, ver, sac, east, xgeo) {
    ref <- reference_data()

    make_one <- function(model, model_name, nodes) {
      model_input <- data.frame(
        DSM2_Node = as.numeric(nodes),
        EXP = exp,
        VER = ver,
        SAC = sac,
        EAST = east,
        XGEO = xgeo
      )

      raw <- predict(model, as.matrix(model_input))

      data.frame(
        Condition = condition,
        Scenario_Name = scenario_name,
        Model = model_name,
        DSM2_Node = as.character(nodes),
        Prediction_Raw = raw,
        Prediction_Final = bound_percent(raw),
        Output_Unit = "Percent",
        EXP = exp,
        VER = ver,
        SAC = sac,
        EAST = east,
        XGEO = xgeo,
        stringsAsFactors = FALSE
      ) %>%
        left_join(ref$node_meta, by = "DSM2_Node")
    }

    bind_rows(
      make_one(ptm7_model, "PTM 7-Day Entrainment", ref$nodes_7d),
      make_one(ptm30_model, "PTM 30-Day Entrainment", ref$nodes_30d)
    )
  }

  build_eco_input <- function(sac, yol, moke, dcc, required_features) {
    values <- list(
      SAC = sac,
      FPT = sac,
      YOLO = yol,
      YOL = yol,
      MOKE = moke,
      MOK = moke,
      DCC = dcc
    )

    missing <- setdiff(required_features, names(values))
    if (length(missing) > 0) {
      stop("Missing ECO-PTM feature mapping for: ", paste(missing, collapse = ", "))
    }

    out <- as.data.frame(
      lapply(required_features, function(x) values[[x]]),
      check.names = FALSE
    )
    names(out) <- required_features
    out
  }

  make_eco_results <- function(condition, scenario_name, sac, yol, moke, dcc) {
    survival_input <- build_eco_input(
      sac, yol, moke, dcc, eco_survival_features
    )
    interior_input <- build_eco_input(
      sac, yol, moke, dcc, eco_interior_features
    )

    survival_raw <- predict(eco_survival_model, as.matrix(survival_input))
    interior_raw <- predict(eco_interior_model, as.matrix(interior_input))

    data.frame(
      Condition = condition,
      Scenario_Name = scenario_name,
      Model = c("ECO-PTM Survival", "ECO-PTM Interior Routing"),
      Prediction_Raw = c(survival_raw, interior_raw),
      Prediction_Final = bound_percent(c(survival_raw, interior_raw)),
      Output_Unit = "Percent",
      SAC = sac,
      YOL = yol,
      MOKE = moke,
      DCC = dcc,
      stringsAsFactors = FALSE
    )
  }

  make_eh_result <- function(condition, scenario_name, exp, ver, east, xgeo, risk) {
    model_input <- data.frame(
      VER = ver,
      EXP = exp,
      EAST = east,
      XGEO = xgeo,
      Risk = risk
    )

    raw <- predict(event_horizon_model, as.matrix(model_input))

    data.frame(
      Condition = condition,
      Scenario_Name = scenario_name,
      Model = "Event Horizon",
      Risk_Level_Percent = risk,
      Prediction_Raw = raw,
      Prediction_Final = pmax(0, raw),
      Output_Unit = "River miles",
      EXP = exp,
      VER = ver,
      EAST = east,
      XGEO = xgeo,
      stringsAsFactors = FALSE
    )
  }

  make_ptm_bar <- function(df, title) {
    validate(need(nrow(df) > 0, "Run the model to display results."))

    plot_df <- df %>%
      mutate(
        node_label = ifelse(
          is.na(Location),
          DSM2_Node,
          paste0(DSM2_Node, " - ", Location)
        )
      ) %>%
      arrange(Prediction_Final)

    plot_df$node_label <- factor(
      plot_df$node_label,
      levels = plot_df$node_label
    )

    ggplot(plot_df, aes(node_label, Prediction_Final)) +
      geom_col() +
      coord_flip() +
      labs(
        title = title,
        x = "DSM2 Node",
        y = "Predicted Entrainment (%)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 11),
        plot.margin = margin(10, 10, 10, 100)
      )
  }

  make_entrainment_zones <- function(nodes, threshold) {
    validate(need(nrow(nodes) > 3, "Not enough node results to create risk zones."))

    bbox <- st_bbox(delta_boundary)
    grid <- expand.grid(
      x = seq(bbox["xmin"], bbox["xmax"], length.out = 150),
      y = seq(bbox["ymin"], bbox["ymax"], length.out = 150)
    ) %>%
      st_as_sf(coords = c("x", "y"), crs = st_crs(nodes))

    sp_grid <- as(grid, "Spatial")
    sp_nodes <- as(nodes, "Spatial")
    names(sp_nodes)[names(sp_nodes) == "entrainment"] <- "z"

    pred <- gstat::idw(z ~ 1, sp_nodes, newdata = sp_grid) %>%
      st_as_sf() %>%
      rename(entrainment = var1.pred)

    high_pts <- pred %>% filter(entrainment >= threshold)

    validate(
      need(
        nrow(high_pts) > 10,
        paste0("Not enough points exceed the ", threshold, "% threshold.")
      )
    )

    high_zone <- high_pts %>%
      st_intersection(delta_boundary) %>%
      st_union() %>%
      sf::st_concave_hull(ratio = 0.5) %>%
      st_as_sf() %>%
      st_intersection(delta_boundary)

    low_zone <- st_difference(delta_boundary, st_union(high_zone))

    list(
      high = st_transform(high_zone, 4326),
      low = st_transform(low_zone, 4326)
    )
  }

  make_ptm_map <- function(df, threshold) {
    validate(need(nrow(df) > 0, "Run the model to display results."))

    sf_nodes <- df %>%
      filter(!is.na(X), !is.na(Y)) %>%
      transmute(
        DSM2_Node,
        Location,
        Region,
        entrainment = Prediction_Final,
        X,
        Y
      ) %>%
      st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE) %>%
      st_transform(26910)

    zones <- make_entrainment_zones(sf_nodes, threshold)
    display_nodes <- st_transform(sf_nodes, 4326)

    pal <- colorNumeric(
      "viridis",
      domain = range(display_nodes$entrainment, na.rm = TRUE)
    )

    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = st_transform(
          delta_boundary,
          4326
        ),
        fillColor = "#eef7f9",
        fillOpacity = 0.2,
        color = "#0a7e8c",
        weight = 1,
        group = "Delta Boundary"
      ) %>%
      
      addPolylines(
        data = st_transform(
          delta_channels,
          4326
        ),
        color = "#2b8cbe",
        weight = 1,
        opacity = 0.6,
        group = "Channels"
      ) %>%
      
      addPolygons(
        data = zones$low,
        fillColor = "#a9d4e6",
        fillOpacity = 0.45,
        color = "#427799",
        weight = 1,
        group = "Low Risk Zone"
      ) %>%
      
      addPolygons(
        data = zones$high,
        fillColor = "#e8b5b5",
        fillOpacity = 0.6,
        color = "#a74a4a",
        weight = 1,
        group = "High Risk Zone"
      ) %>%
      
      addCircleMarkers(
        data = display_nodes,
        radius = ~4 + entrainment / 100 * 10,
        color = ~pal(entrainment),
        fillOpacity = 0.9,
        stroke = FALSE,
        label = ~paste0(
          "Node: ", DSM2_Node,
          "<br>Location: ", Location,
          "<br>Region: ", Region,
          "<br>Entrainment: ", round(entrainment, 1), "%"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = display_nodes$entrainment,
        title = "Entrainment (%)",
        opacity = 1
      ) %>%
      
      addLegend(
        position = "topright",
        colors = c(
          "#e8b5b5",
          "#a9d4e6"
        ),
        labels = c(
          paste0(
            "High Risk Zone: ≥ ",
            threshold,
            "%"
          ),
          paste0(
            "Low Risk Zone: < ",
            threshold,
            "%"
          )
        ),
        title = "Entrainment Risk Zones",
        opacity = 0.8
      ) %>%
      
      addLayersControl(
        overlayGroups = c(
          "Delta Boundary",
          "Channels",
          "Low Risk Zone",
          "High Risk Zone",
          "Nodes"
        ),
        options = layersControlOptions(
          collapsed = FALSE
        )
      ) %>%
      
      fitBounds(
        -122.15, 37.75, -121.15, 38.85)
  }

  make_event_geometry <- function(distance_miles) {
    fraction <- max(0, min(1, distance_miles / river_length_miles))
    list(
      high_line = lwgeom::st_linesubstring(river_centerline, 0, fraction),
      point = lwgeom::st_linesubstring(river_centerline, fraction, fraction)
    )
  }

  make_eh_map <- function(df) {
    validate(need(nrow(df) > 0, "Run the model to display results."))
    geom <- make_event_geometry(df$Prediction_Final[1])

    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = st_transform(delta_boundary, 4326),
        fillColor = "#eef7f9",
        fillOpacity = 0.25,
        color = "#0a7e8c",
        weight = 1
      ) %>%
      addPolylines(
        data = st_transform(delta_channels, 4326),
        color = "#2b8cbe",
        weight = 1,
        opacity = 0.6
      ) %>%
      addPolylines(
        data = st_transform(geom$high_line, 4326),
        color = "red",
        weight = 6
      ) %>%
      addCircleMarkers(
        data = st_transform(geom$point, 4326),
        radius = 9,
        color = "red",
        fillColor = "red",
        fillOpacity = 1,
        label = paste0(
          "Event Horizon: ",
          round(df$Prediction_Final[1], 1),
          " miles"
        )
      ) %>%
      fitBounds(-122.15, 37.75, -121.15, 38.85)
  }

  make_eh_scatter <- function(df) {
    
    validate(
      need(
        nrow(df) > 0,
        "Run the Event Horizon model to display results."
      )
    )
    
    risk <- as.numeric(
      df$Risk_Level_Percent[1]
    )
    
    horizon_col <- paste0(
      "Horizon_",
      risk
    )
    
    validate(
      need(
        horizon_col %in% names(eh_baseline),
        paste0(
          "Historical Event Horizon background data are not available for ",
          risk,
          "% risk. Historical background data currently support 25%, 50%, and 75% risk."
        )
      )
    )
    
    background <- eh_baseline %>%
      transmute(
        
        EXP =
          CCF + TPP,
        
        VER =
          VNS,
        
        Historical_Event_Horizon =
          .data[[horizon_col]]
        
      ) %>%
      filter(
        !is.na(EXP),
        !is.na(VER),
        !is.na(Historical_Event_Horizon),
        EXP > 0,
        VER > 0
      )
    
    if (nrow(background) > 10000) {
      
      set.seed(123)
      
      background <- background %>%
        slice_sample(
          n = 10000
        )
    }
    
    scenario_point <- df %>%
      filter(
        EXP > 0,
        VER > 0
      )
    
    p <- ggplot(
      
      background,
      
      aes(
        x = EXP,
        y = VER,
        color = Historical_Event_Horizon,
        
        text = paste0(
          "Historical Point",
          "<br>Export: ",
          round(
            EXP,
            0
          ),
          " cfs",
          "<br>Vernalis: ",
          round(
            VER,
            0
          ),
          " cfs",
          "<br>Event Horizon: ",
          round(
            Historical_Event_Horizon,
            1
          ),
          " miles"
        )
      )
      
    ) +
      
      geom_point(
        size = 2,
        alpha = 0.35
      ) +
      
      geom_point(
        
        data = scenario_point,
        
        aes(
          x = EXP,
          y = VER,
          
          text = paste0(
            "Selected Scenario",
            "<br>Name: ",
            Scenario_Name,
            "<br>Condition: ",
            Condition,
            "<br>Export: ",
            round(
              EXP,
              0
            ),
            " cfs",
            "<br>Vernalis: ",
            round(
              VER,
              0
            ),
            " cfs",
            "<br>Event Horizon: ",
            round(
              Prediction_Final,
              1
            ),
            " miles"
          )
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
        name = "Historical Event Horizon\nDistance (miles)"
      ) +
      
      scale_x_continuous(
        labels = scales::comma
      ) +
      
      scale_y_log10(
        labels = scales::comma,
        breaks = c(
          1000,
          2000,
          5000,
          10000,
          20000
        )
      ) +
      
      labs(
        title = paste0(
          "Historical Event Horizon Conditions — ",
          risk,
          "% Risk"
        ),
        subtitle =
          "The selected model run is highlighted in red.",
        x = "Combined Export, EXP (cfs)",
        y = "Vernalis Flow, VER (cfs)"
      ) +
      
      theme_bw()
    
    ggplotly(
      p,
      tooltip = "text"
    ) %>%
      
      config(
        displaylogo = FALSE
      )
  }

  register_condition <- function(prefix, condition_label) {

    ptm_result <- eventReactive(
      input[[paste0("run_", prefix, "_ptm")]],
      {
        req(input[[paste0(prefix, "_input_method")]] == "single")

        result <- make_ptm_results(
          condition = condition_label,
          scenario_name = input[[paste0(prefix, "_ptm_name")]],
          exp = input[[paste0(prefix, "_ptm_exp")]],
          ver = input[[paste0(prefix, "_ptm_ver")]],
          sac = input[[paste0(prefix, "_ptm_sac")]],
          east = input[[paste0(prefix, "_ptm_east")]],
          xgeo = input[[paste0(prefix, "_ptm_xgeo")]]
        )

        add_saved_run(
          input[[paste0(prefix, "_ptm_name")]],
          condition_label,
          "PTM",
          result
        )

        result
      }
    )

    eco_result <- eventReactive(
      input[[paste0("run_", prefix, "_eco")]],
      {
        req(input[[paste0(prefix, "_input_method")]] == "single")

        result <- make_eco_results(
          condition = condition_label,
          scenario_name = input[[paste0(prefix, "_eco_name")]],
          sac = input[[paste0(prefix, "_eco_sac")]],
          yol = input[[paste0(prefix, "_eco_yol")]],
          moke = input[[paste0(prefix, "_eco_moke")]],
          dcc = as.numeric(input[[paste0(prefix, "_eco_dcc")]])
        )

        add_saved_run(
          input[[paste0(prefix, "_eco_name")]],
          condition_label,
          "ECO-PTM",
          result
        )

        result
      }
    )

    eh_result <- eventReactive(
      input[[paste0("run_", prefix, "_eh")]],
      {
        req(input[[paste0(prefix, "_input_method")]] == "single")

        result <- make_eh_result(
          condition = condition_label,
          scenario_name = input[[paste0(prefix, "_eh_name")]],
          exp = input[[paste0(prefix, "_eh_exp")]],
          ver = input[[paste0(prefix, "_eh_ver")]],
          east = input[[paste0(prefix, "_eh_east")]],
          xgeo = input[[paste0(prefix, "_eh_xgeo")]],
          risk = as.numeric(input[[paste0(prefix, "_eh_risk")]])
        )

        add_saved_run(
          input[[paste0(prefix, "_eh_name")]],
          condition_label,
          "Event Horizon",
          result
        )

        result
      }
    )

    output[[paste0(prefix, "_ptm7_plot")]] <- renderPlot({
      df <- ptm_result() %>% filter(Model == "PTM 7-Day Entrainment")
      make_ptm_bar(df, paste(condition_label, "— PTM 7-Day Entrainment"))
    })

    output[[paste0(prefix, "_ptm30_plot")]] <- renderPlot({
      df <- ptm_result() %>% filter(Model == "PTM 30-Day Entrainment")
      make_ptm_bar(df, paste(condition_label, "— PTM 30-Day Entrainment"))
    })

    output[[paste0(prefix, "_ptm7_map")]] <- renderLeaflet({
      df <- ptm_result() %>% filter(Model == "PTM 7-Day Entrainment")
      make_ptm_map(
        df,
        as.numeric(input[[paste0(prefix, "_ptm_threshold")]])
      )
    })

    output[[paste0(prefix, "_ptm30_map")]] <- renderLeaflet({
      df <- ptm_result() %>% filter(Model == "PTM 30-Day Entrainment")
      make_ptm_map(
        df,
        as.numeric(input[[paste0(prefix, "_ptm_threshold")]])
      )
    })

    output[[paste0(prefix, "_ptm7_table")]] <- renderTable({
      ptm_result() %>%
        filter(Model == "PTM 7-Day Entrainment") %>%
        transmute(
          DSM2_Node,
          Location,
          Region,
          Prediction_Percent = round(Prediction_Final, 2)
        ) %>%
        arrange(desc(Prediction_Percent))
    })

    output[[paste0(prefix, "_ptm30_table")]] <- renderTable({
      ptm_result() %>%
        filter(Model == "PTM 30-Day Entrainment") %>%
        transmute(
          DSM2_Node,
          Location,
          Region,
          Prediction_Percent = round(Prediction_Final, 2)
        ) %>%
        arrange(desc(Prediction_Percent))
    })

    output[[paste0(prefix, "_eco_table")]] <- renderTable({
      eco_result() %>%
        transmute(
          Model,
          Prediction_Percent = round(Prediction_Final, 2),
          SAC,
          YOL,
          MOKE,
          DCC
        )
    })

    output[[paste0(prefix, "_eh_table")]] <- renderTable({
      eh_result() %>%
        transmute(
          Scenario_Name,
          Risk_Level_Percent,
          Event_Horizon_Miles = round(Prediction_Final, 2),
          EXP,
          VER,
          EAST,
          XGEO
        )
    })

    output[[paste0(prefix, "_eh_map")]] <- renderLeaflet({
      make_eh_map(eh_result())
    })

    output[[paste0(prefix, "_eh_scatter")]] <- renderPlotly({
      make_eh_scatter(eh_result())
    })

    output[[paste0("download_", prefix, "_ptm")]] <- downloadHandler(
      filename = function() {
        paste0(prefix, "_PTM_results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write_csv(ptm_result(), file)
      }
    )

    output[[paste0("download_", prefix, "_eco")]] <- downloadHandler(
      filename = function() {
        paste0(prefix, "_ECO_PTM_results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write_csv(eco_result(), file)
      }
    )

    output[[paste0("download_", prefix, "_eh")]] <- downloadHandler(
      filename = function() {
        paste0(prefix, "_Event_Horizon_result_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write_csv(eh_result(), file)
      }
    )
  }

  register_condition("current", "Current Conditions")
  register_condition("forecast", "Forecast Conditions")

  output$comparison_run_selector <- renderUI({
    
    available <- names(
      saved_runs()
    )
    
    if (length(available) == 0) {
      
      return(
        tags$div(
          class = "alert alert-info",
          "Run at least two scenarios before using Scenario Comparison."
        )
      )
    }
    
    checkboxGroupInput(
      inputId = "comparison_runs",
      label = "Select Two or More Runs:",
      choices = available,
      selected = available
    )
  })

  comparison_data <- reactive({
    req(input$comparison_runs)
    runs <- saved_runs()

    selected <- lapply(input$comparison_runs, function(name) runs[[name]])
    bind_rows(selected) %>%
      filter(Model == input$comparison_model)
  })

  output$comparison_table <- renderTable({
    comparison_data()
  })

  output$comparison_plot <- renderPlot({
    df <- comparison_data()
    validate(need(nrow(df) > 0, "Select compatible runs to compare."))

    if (
      input$comparison_model %in%
      c(
        "PTM 7-Day Entrainment",
        "PTM 30-Day Entrainment"
      )
    ) {
      
      plot_df <- df %>%
        mutate(
          
          Run_Label = ifelse(
            is.na(Saved_Run_ID),
            paste(
              Condition,
              Scenario_Name,
              sep = " — "
            ),
            Saved_Run_ID
          ),
          
          DSM2_Node = factor(
            DSM2_Node,
            levels = unique(
              DSM2_Node[
                order(
                  as.numeric(
                    DSM2_Node
                  )
                )
              ]
            )
          )
        )
      
      ggplot(
        plot_df,
        aes(
          x = DSM2_Node,
          y = Prediction_Final,
          fill = Run_Label
        )
      ) +
        
        geom_col(
          position = position_dodge(
            preserve = "single"
          )
        ) +
        
        coord_flip() +
        
        labs(
          title = input$comparison_model,
          x = "DSM2 Node",
          y = "Predicted Entrainment (%)",
          fill = "Scenario Run"
        ) +
        
        theme_minimal()
      
    }
    else {
      
      plot_df <- df %>%
        mutate(
          
          Run_Label = ifelse(
            is.na(Saved_Run_ID),
            paste(
              Condition,
              Scenario_Name,
              sep = " — "
            ),
            Saved_Run_ID
          )
        )
      
      ggplot(
        plot_df,
        aes(
          x = Run_Label,
          y = Prediction_Final,
          fill = Run_Label
        )
      ) +
        
        geom_col(
          width = 0.7
        ) +
        
        labs(
          title = input$comparison_model,
          x = "Scenario Run",
          
          y = ifelse(
            input$comparison_model ==
              "Event Horizon",
            "Event Horizon Distance (River Miles)",
            "Prediction (%)"
          ),
          
          fill = "Scenario Run"
        ) +
        
        theme_minimal() +
        
        theme(
          axis.text.x = element_text(
            angle = 30,
            hjust = 1
          )
        )
      
    }
  })

  output$download_comparison <- downloadHandler(
    filename = function() {
      paste0("scenario_comparison_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(comparison_data(), file)
    }
  )
}
