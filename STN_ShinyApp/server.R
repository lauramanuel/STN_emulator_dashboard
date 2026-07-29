
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
    
    input_method_label <- if (
      "Input_Method" %in% names(data) &&
      any(data$Input_Method == "folder", na.rm = TRUE)
    ) {
      "Archive Folder"
    } else if (
      "Input_Method" %in% names(data) &&
      any(data$Input_Method == "upload", na.rm = TRUE)
    ) {
      "Uploaded File"
    } else {
      "Single Values"
    }

    key <- paste0(
      run_number,
      ". ",
      condition,
      " | ",
      name,
      " | Input: ",
      input_method_label,
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
        Input_Method = "single",
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
      Input_Method = "single",
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
      Input_Method = "single",
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
        DSM2_Node_Num = suppressWarnings(as.numeric(DSM2_Node)),
        node_label = ifelse(
          is.na(Location) | Location == "",
          DSM2_Node,
          paste0(DSM2_Node, " — ", Location)
        ),
        value_label = sprintf("%.2f%%", Prediction_Final),
        hover_text = paste0(
          "<b>DSM2 Node:</b> ", DSM2_Node,
          "<br><b>Location:</b> ", Location,
          "<br><b>Region:</b> ", Region,
          "<br><b>Predicted entrainment:</b> ",
          sprintf("%.2f%%", Prediction_Final)
        )
      ) %>%
      arrange(DSM2_Node_Num, DSM2_Node)

    # Reverse only for display so the smallest node remains at the top.
    plot_df$node_label <- factor(
      plot_df$node_label,
      levels = rev(plot_df$node_label)
    )

    x_max <- max(
      105,
      ceiling(max(plot_df$Prediction_Final, na.rm = TRUE) + 12)
    )

    plot_ly(
      data = plot_df,
      x = ~Prediction_Final,
      y = ~node_label,
      type = "bar",
      orientation = "h",
      text = ~value_label,
      textposition = "outside",
      textfont = list(size = 14, color = "#1F1F1F"),
      hovertext = ~hover_text,
      hoverinfo = "text",
      marker = list(
        color = "#0072B2",
        line = list(color = "#003B5C", width = 0.8)
      ),
      cliponaxis = FALSE
    ) %>%
      layout(
        title = list(
          text = paste0(
            "<b>", title, "</b>",
            "<br><span style='font-size:13px;'>",
            "Fixed numeric DSM2-node order; values are percent entrainment.",
            "</span>"
          ),
          x = 0.02,
          xanchor = "left",
          font = list(size = 20, color = "#1F1F1F")
        ),
        xaxis = list(
          title = list(
            text = "<b>Predicted Entrainment (%)</b>",
            font = list(size = 16, color = "#1F1F1F")
          ),
          range = c(0, x_max),
          tickfont = list(size = 14, color = "#1F1F1F"),
          gridcolor = "#D9D9D9",
          zerolinecolor = "#666666",
          showline = TRUE,
          linecolor = "#666666"
        ),
        yaxis = list(
          title = "",
          tickfont = list(size = 13, color = "#1F1F1F"),
          automargin = TRUE,
          showgrid = FALSE
        ),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        font = list(
          family = "Arial, Segoe UI, sans-serif",
          size = 14,
          color = "#1F1F1F"
        ),
        margin = list(l = 300, r = 95, t = 100, b = 75),
        showlegend = FALSE,
        annotations = list(
          list(
            text = "Note: Predictions are emulator outputs rounded to two decimal places.",
            x = 0,
            y = -0.12,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            xanchor = "left",
            font = list(size = 12, color = "#404040")
          )
        )
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "lasso2d", "select2d", "autoScale2d",
          "toggleSpikelines"
        ),
        toImageButtonOptions = list(
          format = "png",
          filename = gsub("[^A-Za-z0-9_-]+", "_", title),
          height = 1000,
          width = 1600,
          scale = 2
        )
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

    popup_html <- ~paste0(
      "<div style='font-size:16px;line-height:1.55;min-width:300px;'>",
      "<div style='font-size:20px;font-weight:700;color:#075f6d;margin-bottom:8px;'>DSM2 Node ", DSM2_Node, "</div>",
      "<div><b>Node:</b> ", DSM2_Node, "</div>",
      "<div><b>Location:</b> ", Location, "</div>",
      "<div><b>Region:</b> ", Region, "</div>",
      "<div style='font-size:18px;font-weight:800;margin-top:8px;color:#8b1e1e;'>Entrainment: ",
      sprintf("%.2f", entrainment), "%</div>",
      "</div>"
    )

    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = st_transform(delta_boundary, 4326),
        fillColor = "#eef7f9", fillOpacity = 0.2,
        color = "#0a7e8c", weight = 1,
        group = "Delta Boundary"
      ) %>%
      addPolylines(
        data = st_transform(delta_channels, 4326),
        color = "#2b8cbe", weight = 1, opacity = 0.6,
        group = "Channels"
      ) %>%
      addPolygons(
        data = zones$low,
        fillColor = "#a9d4e6", fillOpacity = 0.45,
        color = "#427799", weight = 1,
        group = "Low Risk Zone"
      ) %>%
      addPolygons(
        data = zones$high,
        fillColor = "#e8b5b5", fillOpacity = 0.6,
        color = "#a74a4a", weight = 1,
        group = "High Risk Zone"
      ) %>%
      addCircleMarkers(
        data = display_nodes,
        radius = ~4 + entrainment / 100 * 10,
        color = ~pal(entrainment),
        fillColor = ~pal(entrainment),
        fillOpacity = 0.9,
        stroke = TRUE,
        weight = 1,
        popup = popup_html,
        group = "Nodes"
      ) %>%
      addLabelOnlyMarkers(
        data = display_nodes,
        label = ~DSM2_Node,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = FALSE,
          offset = c(0, -10),
          className = "node-permanent-label"
        ),
        group = "Node Labels"
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
        colors = c("#e8b5b5", "#a9d4e6"),
        labels = c(
          paste0("High Risk Zone: ≥ ", threshold, "%"),
          paste0("Low Risk Zone: < ", threshold, "%")
        ),
        title = "Entrainment Risk Zones",
        opacity = 0.8
      ) %>%
      addLayersControl(
        overlayGroups = c(
          "Delta Boundary", "Channels", "Low Risk Zone",
          "High Risk Zone", "Nodes", "Node Labels"
        ),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      fitBounds(-122.15, 37.75, -121.15, 38.85)
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

  make_ptm_png_plot <- function(df, threshold, title) {

    if (is.null(df) || nrow(df) == 0) {
      stop("No PTM results are available. Run the PTM models before downloading.")
    }

    threshold <- as.numeric(threshold)

    if (is.na(threshold)) {
      threshold <- 25
    }

    nodes_utm <- df %>%
      filter(
        !is.na(X),
        !is.na(Y),
        !is.na(Prediction_Final)
      ) %>%
      transmute(
        DSM2_Node = as.character(DSM2_Node),
        Location,
        Region,
        entrainment = as.numeric(Prediction_Final),
        X = as.numeric(X),
        Y = as.numeric(Y)
      ) %>%
      st_as_sf(
        coords = c("X", "Y"),
        crs = 4326,
        remove = FALSE
      ) %>%
      st_transform(26910)

    if (nrow(nodes_utm) == 0) {
      stop("PTM node coordinates are missing, so the PNG map cannot be created.")
    }

    # Create plain coordinate columns for reliable node labels.
    node_xy <- st_coordinates(nodes_utm)

    node_labels <- nodes_utm %>%
      st_drop_geometry() %>%
      mutate(
        map_x = node_xy[, 1],
        map_y = node_xy[, 2]
      )

    # Risk polygons are added when interpolation succeeds. The PNG still
    # downloads with nodes and boundaries if a particular threshold cannot
    # produce a valid interpolated polygon.
    zones <- tryCatch(
      make_entrainment_zones(nodes_utm, threshold),
      error = function(e) NULL,
      shiny.silent.error = function(e) NULL
    )

    p <- ggplot() +
      geom_sf(
        data = st_make_valid(delta_boundary),
        fill = "#F7FBFC",
        color = "#2B6573",
        linewidth = 0.55
      ) +
      geom_sf(
        data = st_make_valid(delta_channels),
        color = "#6BAED6",
        linewidth = 0.28,
        alpha = 0.65
      )

    if (!is.null(zones)) {

      low_zone <- st_transform(st_make_valid(zones$low), 26910) %>%
        mutate(Risk_Zone = paste0("Low risk: < ", threshold, "%"))

      high_zone <- st_transform(st_make_valid(zones$high), 26910) %>%
        mutate(Risk_Zone = paste0("High risk: ≥ ", threshold, "%"))

      zone_values <- stats::setNames(
        c("#A9D4E6", "#E8B5B5"),
        c(
          paste0("Low risk: < ", threshold, "%"),
          paste0("High risk: ≥ ", threshold, "%")
        )
      )

      p <- p +
        geom_sf(
          data = low_zone,
          aes(fill = Risk_Zone),
          color = "#427799",
          alpha = 0.48,
          linewidth = 0.4
        ) +
        geom_sf(
          data = high_zone,
          aes(fill = Risk_Zone),
          color = "#9B2C2C",
          alpha = 0.62,
          linewidth = 0.4
        ) +
        scale_fill_manual(
          name = "Entrainment risk zones",
          values = zone_values,
          drop = FALSE
        )
    }

    p +
      geom_sf(
        data = nodes_utm,
        aes(color = entrainment),
        size = 3.4
      ) +
      geom_text(
        data = node_labels,
        aes(
          x = map_x,
          y = map_y,
          label = DSM2_Node
        ),
        nudge_y = 2800,
        size = 3.7,
        fontface = "bold",
        color = "#1F1F1F",
        check_overlap = TRUE
      ) +
      scale_color_viridis_c(
        option = "D",
        name = "Predicted\nentrainment (%)",
        limits = c(0, 100),
        breaks = c(0, 25, 50, 75, 100)
      ) +
      coord_sf(datum = NA) +
      labs(
        title = title,
        subtitle = paste0(
          "PTM predictions for all supported DSM2 nodes; risk threshold = ",
          threshold,
          "%"
        ),
        caption = if (is.null(zones)) {
          paste0(
            "Node values are shown. Interpolated risk zones could not be ",
            "generated for this result and threshold."
          )
        } else {
          paste0(
            "Red shading indicates entrainment ≥ ",
            threshold,
            "%; blue shading indicates entrainment < ",
            threshold,
            "%."
          )
        }
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(
          size = 19,
          face = "bold",
          color = "#1F1F1F"
        ),
        plot.subtitle = element_text(
          size = 13,
          color = "#333333"
        ),
        plot.caption = element_text(
          size = 11,
          hjust = 0,
          color = "#404040"
        ),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        plot.background = element_rect(
          fill = "white",
          color = NA
        ),
        panel.background = element_rect(
          fill = "white",
          color = NA
        )
      )
  }

  make_eh_png_plot <- function(df, title) {
    validate(need(nrow(df) > 0, "Run the model before downloading the map."))

    geom <- make_event_geometry(df$Prediction_Final[1])
    eh_line <- st_transform(geom$high_line, 26910)
    eh_point <- st_transform(geom$point, 26910)

    ggplot() +
      geom_sf(
        data = delta_boundary,
        fill = "#F7FBFC",
        color = "#2B6573",
        linewidth = 0.5
      ) +
      geom_sf(
        data = delta_channels,
        color = "#6BAED6",
        linewidth = 0.3,
        alpha = 0.70
      ) +
      geom_sf(
        data = eh_line,
        color = "#B2182B",
        linewidth = 2.2
      ) +
      geom_sf(
        data = eh_point,
        color = "#B2182B",
        fill = "#B2182B",
        shape = 21,
        size = 4.5
      ) +
      coord_sf(datum = NA) +
      labs(
        title = title,
        subtitle = paste0(
          "Predicted Event Horizon: ",
          sprintf("%.2f", df$Prediction_Final[1]),
          " river miles at ",
          df$Risk_Level_Percent[1],
          "% risk"
        ),
        caption = "The red reach represents the predicted upstream Event Horizon distance."
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 19, face = "bold", color = "#1F1F1F"),
        plot.subtitle = element_text(size = 13, color = "#333333"),
        plot.caption = element_text(size = 11, hjust = 0, color = "#404040"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
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

    output[[paste0(prefix, "_ptm7_plot")]] <- renderPlotly({
      df <- ptm_result() %>% filter(Model == "PTM 7-Day Entrainment")
      make_ptm_bar(df, paste(condition_label, "— PTM 7-Day Entrainment"))
    })

    output[[paste0(prefix, "_ptm30_plot")]] <- renderPlotly({
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

    output[[paste0("download_", prefix, "_ptm7_map")]] <- downloadHandler(
      filename = function() {
        paste0(
          prefix,
          "_PTM_7day_map_",
          format(Sys.Date(), "%Y-%m-%d"),
          ".png"
        )
      },
      contentType = "image/png",
      content = function(file) {

        df <- ptm_result() %>%
          filter(Model == "PTM 7-Day Entrainment")

        p <- make_ptm_png_plot(
          df = df,
          threshold = input[[paste0(prefix, "_ptm_threshold")]],
          title = paste(
            condition_label,
            "— PTM 7-Day Entrainment Risk Map"
          )
        )

        grDevices::png(
          filename = file,
          width = 3900,
          height = 2700,
          units = "px",
          res = 300,
          bg = "white",
          type = "cairo"
        )

        on.exit(
          grDevices::dev.off(),
          add = TRUE
        )

        print(p)
      }
    )

    output[[paste0("download_", prefix, "_ptm30_map")]] <- downloadHandler(
      filename = function() {
        paste0(
          prefix,
          "_PTM_30day_map_",
          format(Sys.Date(), "%Y-%m-%d"),
          ".png"
        )
      },
      contentType = "image/png",
      content = function(file) {

        df <- ptm_result() %>%
          filter(Model == "PTM 30-Day Entrainment")

        p <- make_ptm_png_plot(
          df = df,
          threshold = input[[paste0(prefix, "_ptm_threshold")]],
          title = paste(
            condition_label,
            "— PTM 30-Day Entrainment Risk Map"
          )
        )

        grDevices::png(
          filename = file,
          width = 3900,
          height = 2700,
          units = "px",
          res = 300,
          bg = "white",
          type = "cairo"
        )

        on.exit(
          grDevices::dev.off(),
          add = TRUE
        )

        print(p)
      }
    )

    output[[paste0(prefix, "_ptm7_table")]] <- renderTable({
      ptm_result() %>%
        filter(Model == "PTM 7-Day Entrainment") %>%
        transmute(
          DSM2_Node,
          Location,
          Region,
          Prediction_Percent = round(Prediction_Final, 2)
        ) %>%
        arrange(suppressWarnings(as.numeric(DSM2_Node)), DSM2_Node)
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
        arrange(suppressWarnings(as.numeric(DSM2_Node)), DSM2_Node)
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

    output[[paste0("download_", prefix, "_eh_map")]] <- downloadHandler(
      filename = function() paste0(prefix, "_Event_Horizon_map_", Sys.Date(), ".png"),
      contentType = "image/png",
      content = function(file) {
        p <- make_eh_png_plot(
          eh_result(),
          paste(condition_label, "— Event Horizon Map")
        )
        ggsave(
          filename = file,
          plot = p,
          device = "png",
          width = 13,
          height = 9,
          units = "in",
          dpi = 300,
          bg = "white"
        )
      }
    )

    output[[paste0(prefix, "_eh_scatter")]] <- renderPlotly({
      make_eh_scatter(eh_result())
    })

    output[[paste0("download_", prefix, "_ptm")]] <- downloadHandler(
      filename = function() {
        paste0(prefix, "_PTM_results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write_csv(ptm_result() %>% mutate(across(where(is.numeric), ~round(.x, 2))), file)
      }
    )

    output[[paste0("download_", prefix, "_eco")]] <- downloadHandler(
      filename = function() {
        paste0(prefix, "_ECO_PTM_results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write_csv(eco_result() %>% mutate(across(where(is.numeric), ~round(.x, 2))), file)
      }
    )

    output[[paste0("download_", prefix, "_eh")]] <- downloadHandler(
      filename = function() {
        paste0(prefix, "_Event_Horizon_result_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write_csv(eh_result() %>% mutate(across(where(is.numeric), ~round(.x, 2))), file)
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

  output$comparison_plot <- renderPlotly({
    df <- comparison_data()
    validate(need(nrow(df) > 0, "Select compatible runs to compare."))

    plot_df <- df %>%
      mutate(
        Run_Label = ifelse(
          is.na(Saved_Run_ID),
          paste(Condition, Scenario_Name, sep = " — "),
          Saved_Run_ID
        ),
        hover_text = paste0(
          "<b>Run:</b> ", Run_Label,
          "<br><b>Model:</b> ", Model,
          ifelse(
            !is.na(DSM2_Node),
            paste0("<br><b>DSM2 Node:</b> ", DSM2_Node),
            ""
          ),
          "<br><b>Prediction:</b> ",
          sprintf("%.2f", Prediction_Final),
          " ", Output_Unit
        )
      )

    if (
      input$comparison_model %in%
      c("PTM 7-Day Entrainment", "PTM 30-Day Entrainment")
    ) {
      plot_df <- plot_df %>%
        mutate(
          DSM2_Node_Num = suppressWarnings(as.numeric(DSM2_Node)),
          DSM2_Node = factor(
            DSM2_Node,
            levels = rev(
              unique(
                DSM2_Node[
                  order(DSM2_Node_Num, DSM2_Node)
                ]
              )
            )
          )
        )

      p <- plot_ly(
        plot_df,
        x = ~Prediction_Final,
        y = ~DSM2_Node,
        color = ~Run_Label,
        type = "bar",
        orientation = "h",
        text = ~sprintf("%.2f", Prediction_Final),
        textposition = "auto",
        hovertext = ~hover_text,
        hoverinfo = "text"
      ) %>%
        layout(
          barmode = "group",
          xaxis = list(
            title = "<b>Predicted Entrainment (%)</b>",
            tickfont = list(size = 13)
          ),
          yaxis = list(
            title = "<b>DSM2 Node</b>",
            tickfont = list(size = 13),
            automargin = TRUE
          )
        )
    } else {
      p <- plot_ly(
        plot_df,
        x = ~Run_Label,
        y = ~Prediction_Final,
        color = ~Run_Label,
        type = "bar",
        text = ~sprintf("%.2f", Prediction_Final),
        textposition = "outside",
        hovertext = ~hover_text,
        hoverinfo = "text"
      ) %>%
        layout(
          xaxis = list(
            title = "<b>Scenario Run</b>",
            tickangle = -25,
            automargin = TRUE,
            tickfont = list(size = 12)
          ),
          yaxis = list(
            title = if (
              input$comparison_model == "Event Horizon"
            ) {
              "<b>Event Horizon Distance (River Miles)</b>"
            } else {
              "<b>Prediction (%)</b>"
            },
            tickfont = list(size = 13)
          ),
          showlegend = FALSE
        )
    }

    p %>%
      layout(
        title = list(
          text = paste0("<b>", input$comparison_model, " Comparison</b>"),
          x = 0.02,
          xanchor = "left"
        ),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        font = list(
          family = "Arial, Segoe UI, sans-serif",
          size = 14,
          color = "#1F1F1F"
        ),
        margin = list(l = 100, r = 60, t = 80, b = 120)
      ) %>%
      config(
        displaylogo = FALSE,
        toImageButtonOptions = list(
          format = "png",
          filename = "scenario_comparison",
          width = 1600,
          height = 1000,
          scale = 2
        )
      )
  })

  output$download_comparison <- downloadHandler(
    filename = function() {
      paste0("scenario_comparison_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(comparison_data() %>% mutate(across(where(is.numeric), ~round(.x, 2))), file)
    }
  )


  archive_runs <- reactive({
    runs <- saved_runs()
    if (length(runs) == 0) return(list())
    runs[vapply(runs, function(x) {
      "Input_Method" %in% names(x) && any(x$Input_Method == "folder", na.rm = TRUE)
    }, logical(1))]
  })

  output$omri_archive_available <- reactive({
    length(archive_runs()) > 0
  })
  outputOptions(output, "omri_archive_available", suspendWhenHidden = FALSE)

  output$omri_comparison_status <- renderUI({
    if (length(archive_runs()) == 0) {
      tags$div(
        class = "alert alert-info",
        tags$b("OMRI comparison is not active yet."),
        tags$br(),
        "This section becomes available only after one or more runs are created using the Read from Archive Folder input method."
      )
    } else {
      tags$div(class = "alert alert-success", "Archive-folder runs are available for OMRI comparison.")
    }
  })

  output$omri_run_selector <- renderUI({
    available <- names(archive_runs())
    checkboxGroupInput("omri_comparison_runs", "Select OMRI Runs:", choices = available, selected = available)
  })

  omri_comparison_data <- reactive({
    req(input$omri_comparison_runs)
    selected <- lapply(input$omri_comparison_runs, function(name) archive_runs()[[name]])
    bind_rows(selected) %>% filter(Model == input$omri_comparison_model)
  })

  output$omri_comparison_table <- renderTable({
    omri_comparison_data()
  })

  output$omri_comparison_plot <- renderPlotly({
    df <- omri_comparison_data()
    validate(need(nrow(df) > 0, "Select compatible OMRI archive runs to compare."))

    plot_df <- df %>%
      mutate(
        Run_Label = ifelse(
          is.na(Saved_Run_ID),
          paste(Condition, Scenario_Name, sep = " — "),
          Saved_Run_ID
        ),
        hover_text = paste0(
          "<b>OMRI Run:</b> ", Run_Label,
          "<br><b>Model:</b> ", Model,
          ifelse(
            !is.na(DSM2_Node),
            paste0("<br><b>DSM2 Node:</b> ", DSM2_Node),
            ""
          ),
          "<br><b>Prediction:</b> ",
          sprintf("%.2f", Prediction_Final),
          " ", Output_Unit
        )
      )

    plot_ly(
      plot_df,
      x = ~Run_Label,
      y = ~Prediction_Final,
      color = ~Run_Label,
      type = "bar",
      text = ~sprintf("%.2f", Prediction_Final),
      textposition = "outside",
      hovertext = ~hover_text,
      hoverinfo = "text"
    ) %>%
      layout(
        title = list(
          text = "<b>OMRI Archive Scenario Comparison</b>",
          x = 0.02,
          xanchor = "left"
        ),
        xaxis = list(
          title = "<b>OMRI Scenario</b>",
          tickangle = -25,
          automargin = TRUE
        ),
        yaxis = list(title = "<b>Prediction</b>"),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        font = list(
          family = "Arial, Segoe UI, sans-serif",
          size = 14,
          color = "#1F1F1F"
        ),
        showlegend = FALSE,
        margin = list(l = 90, r = 50, t = 80, b = 130)
      ) %>%
      config(
        displaylogo = FALSE,
        toImageButtonOptions = list(
          format = "png",
          filename = "OMRI_scenario_comparison",
          width = 1600,
          height = 1000,
          scale = 2
        )
      )
  })

  output$download_omri_comparison <- downloadHandler(
    filename = function() paste0("OMRI_scenario_comparison_", Sys.Date(), ".csv"),
    content = function(file) {
      write_csv(omri_comparison_data() %>% mutate(across(where(is.numeric), ~round(.x, 2))), file)
    }
  )


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


  output$ecoptm_inputs_table <- gt::render_gt({
      
      badge <- function(text) sprintf(
        "<span style='display:inline-block;background:#f9e9ee;color:#c7254e;border:1px solid #efcbd5;border-radius:4px;padding:3px 8px;font-family:monospace;white-space:nowrap;'>%s</span>",
        text
      )
      
      link <- function(text, url) sprintf(
        "<a href='%s' target='_blank' rel='noopener noreferrer'>%s</a>",
        url, text
      )
      
      group_label <- function(number, content = "") sprintf(
        "<div style='display:flex;align-items:center;gap:12px;width:100%%;min-height:46px;'><strong>Input Number %s</strong>%s</div>",
        number, content
      )
      
      arrow <- "<span style='display:inline-flex;align-items:center;margin-left:6px;color:#0a7e8c;font-size:30px;line-height:1;transform:translateY(12px);'><i class='fa fa-level-down' aria-hidden='true'></i></span>"
      
      yol_equation <- sprintf(
        "<div style='display:flex;align-items:center;gap:8px;white-space:nowrap;'>%s<strong>=</strong>%s<strong>+</strong>%s%s</div>",
        badge("YOL"), badge("SACWEIR"), badge("FREWEIR"), arrow
      )
      
      qaqc_source_values <- c(
        link("USGS - 11447650", "https://waterdata.usgs.gov/monitoring-location/USGS-11447650/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true"),
        link("USGS - 11426000", "https://waterdata.usgs.gov/monitoring-location/11426000/"),
        link("WDL - A02170", "https://wdl.water.ca.gov/waterdatalibrary/StationDetails.aspx?dateFrom2=01%2f01%2f1900&StationTypeCode=&Station=A02170&IncludeVarData=False&SelectedAll=False&dateTo2=01%2f01%2f1900&source=search"),
        link("USGS - 11325500", "https://waterdata.usgs.gov/monitoring-location/USGS-11325500/"),
        "0 = closed; 1 = open"
      )
      
      realtime_source_values <- c(
        link("CDEC - FPT", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=FPT"),
        link("USGS - 11426000", "https://waterdata.usgs.gov/monitoring-location/11426000/"),
        link("CDEC - FRE", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=FRE"),
        link("USACE - CA00173", "https://water.usace.army.mil/overview/spk/locations/camanche"),
        "0 = closed; 1 = open"
      )
      
      input_data <- data.frame(
        Group_ID = c("input_1", "input_2", "input_2", "input_3", "input_4"),
        Acronym = c("FPT", "SACWEIR", "FREWEIR", "MOK", "DCC"),
        Full_Name = c(
          "Freeport",
          "Sacramento Weir",
          "Fremont Weir",
          "Mokelumne River",
          "Delta Cross Channel"
        ),
        Description = c(
          "Sacramento River flow at Freeport",
          "Sacramento Weir flow spill to Yolo Bypass",
          "Fremont Weir flow spill to Yolo Bypass",
          "Mokelumne River before its confluence with Cosumnes River",
          "Delta Cross Channel gate opening status recorded as a Boolean value"
        ),
        QAQC_Stage_Level_Data = qaqc_source_values,
        Real_Time_Data = realtime_source_values,
        stringsAsFactors = FALSE
      )
      
      input_data$QAQC_Stage_Level_Data <- lapply(
        input_data$QAQC_Stage_Level_Data,
        gt::html
      )
      
      input_data$Real_Time_Data <- lapply(
        input_data$Real_Time_Data,
        gt::html
      )
      
      input_data |>
        gt::gt() |>
        gt::cols_hide(columns = Group_ID) |>
        gt::cols_label(
          Acronym = "Model Input Acronym",
          Full_Name = "Full Name",
          Description = "Description",
          QAQC_Stage_Level_Data = "QA/QC Daily Data",
          Real_Time_Data = "Real-Time Data"
        ) |>
        gt::tab_row_group(
          label = gt::html(group_label(1)),
          rows = Group_ID == "input_1",
          id = "input_1"
        ) |>
        gt::tab_row_group(
          label = gt::html(group_label(2, yol_equation)),
          rows = Group_ID == "input_2",
          id = "input_2"
        ) |>
        gt::tab_row_group(
          label = gt::html(group_label(3)),
          rows = Group_ID == "input_3",
          id = "input_3"
        ) |>
        gt::tab_row_group(
          label = gt::html(group_label(4)),
          rows = Group_ID == "input_4",
          id = "input_4"
        ) |>
        gt::row_group_order(
          groups = c("input_1", "input_2", "input_3", "input_4")
        ) |>
        gt::text_transform(
          locations = gt::cells_body(columns = Acronym),
          fn = function(x) vapply(x, badge, character(1))
        ) |>
        gt::text_transform(
          locations = gt::cells_body(
            columns = c(QAQC_Stage_Level_Data, Real_Time_Data)
          ),
          fn = identity
        ) |>
        gt::cols_width(
          Acronym ~ gt::px(160),
          Full_Name ~ gt::px(210),
          Description ~ gt::px(420),
          QAQC_Stage_Level_Data ~ gt::px(300),
          Real_Time_Data ~ gt::px(300)
        ) |>
        gt::cols_align(
          align = "center",
          columns = everything()
        ) |>
        gt::tab_style(
          style = list(
            gt::cell_text(
              align = "center",
              v_align = "middle"
            ),
            gt::cell_borders(
              sides = c("left", "right", "bottom"),
              color = "#c7d7da",
              weight = gt::px(1)
            )
          ),
          locations = gt::cells_body(columns = everything())
        ) |>
        gt::tab_style(
          style = list(
            gt::cell_fill(color = "#eef7f9"),
            gt::cell_text(
              align = "left",
              v_align = "middle",
              size = "medium"
            ),
            gt::cell_borders(
              sides = c("top", "bottom"),
              color = "#0a7e8c",
              weight = gt::px(1)
            )
    badge <- function(text) sprintf(
      "<span style='display:inline-block;background:#f9e9ee;color:#c7254e;border:1px solid #efcbd5;border-radius:4px;padding:3px 8px;font-family:monospace;white-space:nowrap;'>%s</span>",
      text
    )
    
    link <- function(text, url) sprintf(
      "<a href='%s' target='_blank' rel='noopener noreferrer'>%s</a>",
      url, text
    )
    
    group_label <- function(number, content = "") sprintf(
      "<div style='display:flex;align-items:center;gap:12px;width:100%%;min-height:46px;'><strong>Input Number %s</strong>%s</div>",
      number, content
    )
    
    arrow <- "<span style='display:inline-flex;align-items:center;margin-left:6px;color:#0a7e8c;font-size:30px;line-height:1;transform:translateY(12px);'><i class='fa fa-level-down' aria-hidden='true'></i></span>"
    
    yol_equation <- sprintf(
      "<div style='display:flex;align-items:center;gap:8px;white-space:nowrap;'>%s<strong>=</strong>%s<strong>+</strong>%s%s</div>",
      badge("YOL"), badge("SACWEIR"), badge("FREWEIR"), arrow
    )
    
    qaqc_source_values <- c(
      link("USGS - 11447650", "https://waterdata.usgs.gov/monitoring-location/USGS-11447650/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true"),
      link("USGS - 11426000", "https://waterdata.usgs.gov/monitoring-location/11426000/"),
      link("WDL - A02170", "https://wdl.water.ca.gov/waterdatalibrary/StationDetails.aspx?dateFrom2=01%2f01%2f1900&StationTypeCode=&Station=A02170&IncludeVarData=False&SelectedAll=False&dateTo2=01%2f01%2f1900&source=search"),
      link("USGS - 11325500", "https://waterdata.usgs.gov/monitoring-location/USGS-11325500/"),
      "0 = closed; 1 = open"
    )
    
    realtime_source_values <- c(
      link("CDEC - FPT", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=FPT"),
      link("USGS - 11426000", "https://waterdata.usgs.gov/monitoring-location/11426000/"),
      link("CDEC - FRE", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=FRE"),
      link("USACE - CA00173", "https://water.usace.army.mil/overview/spk/locations/camanche"),
      "0 = closed; 1 = open"
    )
    
    input_data <- data.frame(
      Group_ID = c("input_1", "input_2", "input_2", "input_3", "input_4"),
      Acronym = c("FPT", "SACWEIR", "FREWEIR", "MOK", "DCC"),
      Full_Name = c(
        "Freeport",
        "Sacramento Weir",
        "Fremont Weir",
        "Mokelumne River",
        "Delta Cross Channel"
      ),
      Description = c(
        "Sacramento River flow at Freeport",
        "Sacramento Weir flow spill to Yolo Bypass",
        "Fremont Weir flow spill to Yolo Bypass",
        "Mokelumne River before its confluence with Cosumnes River",
        "Delta Cross Channel gate opening status recorded as a Boolean value"
      ),
      QAQC_Stage_Level_Data = qaqc_source_values,
      Real_Time_Data = realtime_source_values,
      stringsAsFactors = FALSE
    )
    
    input_data$QAQC_Stage_Level_Data <- lapply(
      input_data$QAQC_Stage_Level_Data,
      gt::html
    )
    
    input_data$Real_Time_Data <- lapply(
      input_data$Real_Time_Data,
      gt::html
    )
    
    input_data |>
      gt::gt() |>
      gt::cols_hide(columns = Group_ID) |>
      gt::cols_label(
        Acronym = "Model Input Acronym",
        Full_Name = "Full Name",
        Description = "Description",
        QAQC_Stage_Level_Data = "QA/QC Daily Data",
        Real_Time_Data = "Real-Time Data"
      ) |>
      gt::tab_row_group(
        label = gt::html(group_label(1)),
        rows = Group_ID == "input_1",
        id = "input_1"
      ) |>
      gt::tab_row_group(
        label = gt::html(group_label(2, yol_equation)),
        rows = Group_ID == "input_2",
        id = "input_2"
      ) |>
      gt::tab_row_group(
        label = gt::html(group_label(3)),
        rows = Group_ID == "input_3",
        id = "input_3"
      ) |>
      gt::tab_row_group(
        label = gt::html(group_label(4)),
        rows = Group_ID == "input_4",
        id = "input_4"
      ) |>
      gt::row_group_order(
        groups = c("input_1", "input_2", "input_3", "input_4")
      ) |>
      gt::text_transform(
        locations = gt::cells_body(columns = Acronym),
        fn = function(x) vapply(x, badge, character(1))
      ) |>
      gt::text_transform(
        locations = gt::cells_body(
          columns = c(QAQC_Stage_Level_Data, Real_Time_Data)
        ),
        fn = identity
      ) |>
      gt::cols_width(
        Acronym ~ gt::px(160),
        Full_Name ~ gt::px(210),
        Description ~ gt::px(420),
        QAQC_Stage_Level_Data ~ gt::px(300),
        Real_Time_Data ~ gt::px(300)
      ) |>
      gt::cols_align(
        align = "center",
        columns = everything()
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_text(
            align = "center",
            v_align = "middle"
          ),
          gt::cell_borders(
            sides = c("left", "right", "bottom"),
            color = "#c7d7da",
            weight = gt::px(1)
          )
        ),
        locations = gt::cells_body(columns = everything())
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#eef7f9"),
          gt::cell_text(
            align = "left",
            v_align = "middle",
            size = "medium"
          ),
          gt::cell_borders(
            sides = c("top", "bottom"),
            color = "#0a7e8c",
            weight = gt::px(1)
          )
        ),
        locations = gt::cells_row_groups()
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#0a7e8c"),
          gt::cell_text(
            color = "white",
            weight = "bold",
            align = "center",
            v_align = "middle"
          ),
          locations = gt::cells_row_groups()
        ) |>
        gt::tab_style(
          style = list(
            gt::cell_fill(color = "#0a7e8c"),
            gt::cell_text(
              color = "white",
              weight = "bold",
              align = "center",
              v_align = "middle"
            ),
            gt::cell_borders(
              sides = c("left", "right", "top", "bottom"),
              color = "#c7d7da",
              weight = gt::px(1)
            )
          ),
          locations = gt::cells_column_labels(columns = everything())
        ) |>
        gt::tab_source_note(
          source_note = gt::html(
            "<strong>Note:</strong> For some QA/QC data sources, only reviewed or approved stage data are available. Corresponding discharge values are calculated using a stage–discharge rating curve."
          )
        ) |>
        gt::tab_style(
          style = gt::cell_text(
            align = "left",
            color = "#555555",
            size = gt::px(12)
          ),
          locations = gt::cells_source_notes()
        ) |>
        gt::tab_options(
          table.width = gt::pct(100),
          table.font.names = "Segoe UI",
          table.font.size = gt::px(14),
          data_row.padding = gt::px(10),
          row_group.padding = gt::px(10)
        )
    })


  output$ptm_inputs_table <- gt::render_gt({
  
      badge <- function(text) sprintf(
        "<span style='display:inline-block;background:#f9e9ee;color:#c7254e;border:1px solid #efcbd5;border-radius:4px;padding:3px 8px;font-family:monospace;white-space:nowrap;'>%s</span>",
        text
      )
      
      link <- function(text, url) sprintf(
        "<a href='%s' target='_blank' rel='noopener noreferrer'>%s</a>",
        url, text
      )
      
      group_label <- function(number, content = "") sprintf(
        "<div style='display:flex;align-items:center;gap:12px;width:100%%;min-height:46px;'><strong>Input Number %s</strong>%s</div>",
        number, content
      )
      
      arrow <- "<span style='display:inline-flex;align-items:center;margin-left:6px;color:#0a7e8c;font-size:30px;line-height:1;transform:translateY(12px);'><i class='fa fa-level-down' aria-hidden='true'></i></span>"
      
      equation <- function(result, terms, operators) {
        rhs <- badge(terms[1])
        if (length(terms) > 1) {
          for (i in seq_len(length(terms) - 1)) {
            rhs <- paste0(rhs, sprintf("<strong style='color:#333333;'>%s</strong>", operators[i]), badge(terms[i + 1]))
          }
        ),
        locations = gt::cells_column_labels(columns = everything())
      ) |>
      gt::tab_source_note(
        source_note = gt::html(
          "<strong>Note:</strong> For some QA/QC data sources, only reviewed or approved stage data are available. Corresponding discharge values are calculated using a stage–discharge rating curve."
        )
      ) |>
      gt::tab_style(
        style = gt::cell_text(
          align = "left",
          color = "#555555",
          size = gt::px(12)
        ),
        locations = gt::cells_source_notes()
      ) |>
      gt::tab_options(
        table.width = gt::pct(100),
        table.font.names = "Segoe UI",
        table.font.size = gt::px(14),
        data_row.padding = gt::px(10),
        row_group.padding = gt::px(10)
      )
  })
  

  output$ptm_inputs_table <- gt::render_gt({

    badge <- function(text) sprintf(
      "<span style='display:inline-block;background:#f9e9ee;color:#c7254e;border:1px solid #efcbd5;border-radius:4px;padding:3px 8px;font-family:monospace;white-space:nowrap;'>%s</span>",
      text
    )
    
    link <- function(text, url) sprintf(
      "<a href='%s' target='_blank' rel='noopener noreferrer'>%s</a>",
      url, text
    )
    
    group_label <- function(number, content = "") sprintf(
      "<div style='display:flex;align-items:center;gap:12px;width:100%%;min-height:46px;'><strong>Input Number %s</strong>%s</div>",
      number, content
    )
    
    arrow <- "<span style='display:inline-flex;align-items:center;margin-left:6px;color:#0a7e8c;font-size:30px;line-height:1;transform:translateY(12px);'><i class='fa fa-level-down' aria-hidden='true'></i></span>"
    
    equation <- function(result, terms, operators) {
      rhs <- badge(terms[1])
      if (length(terms) > 1) {
        for (i in seq_len(length(terms) - 1)) {
          rhs <- paste0(rhs, sprintf("<strong style='color:#333333;'>%s</strong>", operators[i]), badge(terms[i + 1]))
        }
        paste0(
          "<div style='display:flex;align-items:center;gap:8px;white-space:nowrap;'>",
          badge(result), "<strong style='color:#333333;'>=</strong>", rhs, arrow, "</div>"
        )
      }
      
      exp_equation <- equation("EXP", c("CCF", "TPP"), "+")
      east_equation <- equation("EAST", c("MOK", "CAL", "COS"), c("+", "+"))
      xgeo_equation <- equation("XGEO", c("XGEO_A", "XGEO_C"), "-")
      
      qaqc_source_values <- c(
        "Click the link in <strong> Model Parameters </strong> to view the map",
        link("Historical O & M Reports - Available by Request", "https://water.ca.gov/Programs/State-Water-Project/Operations-and-Maintenance/Monthly-and-Annual-Operations-Reports"),
        link("Historical O & M Reports - Available by Request", "https://water.ca.gov/Programs/State-Water-Project/Operations-and-Maintenance/Monthly-and-Annual-Operations-Reports"),
        link("USGS - 11303500", "https://waterdata.usgs.gov/monitoring-location/USGS-11303500/#dataTypeId=daily-00060-0&period=P1Y&showFieldMeasurements=true"),
        link("USGS - 11447650", "https://waterdata.usgs.gov/monitoring-location/USGS-11447650/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true"),
        link("USGS - 11325500", "https://waterdata.usgs.gov/monitoring-location/11325500/"),
        link("USACE - CA10109", "https://water.usace.army.mil/overview/spk/locations/new%20hogan"),
        link("USGS - 11335000", "https://waterdata.usgs.gov/monitoring-location/USGS-11335000/#dataTypeId=daily-00060-0&period=P1Y&showFieldMeasurements=true"),
        link("USGS - 11447890", "https://waterdata.usgs.gov/monitoring-location/USGS-11447890/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true"),
        link("USGS - 11447905", "https://waterdata.usgs.gov/monitoring-location/USGS-11447905/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true")
      )
      
      realtime_source_values <- c(
        "Click the link in <strong> Model Parameters </strong> to view the map",
        link("CDEC - CLC", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=CLC"),
        link("CDEC - TRP", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=TRP"),
        link("CDEC - VNS", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=VNS"),
        link("CDEC - FPT", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=FPT"),
        link("USACE - CA00173", "https://water.usace.army.mil/overview/spk/locations/camanche"),
        link("CDEC - NHG", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=NHG"),
        link("USGS - 11335000", "https://waterdata.usgs.gov/monitoring-location/USGS-11335000"),
        link("USGS - 11447890", "https://waterdata.usgs.gov/monitoring-location/USGS-11447890"),
        link("USGS - 11447905", "https://waterdata.usgs.gov/monitoring-location/USGS-11447905")
      )
      
      input_data <- data.frame(
        Group_ID = c("input_1", "input_2", "input_2", "input_3", "input_4", "input_5", "input_5", "input_5", "input_6", "input_6"),
        Acronym = c("Location", "CCF", "TPP", "VNS", "SAC", "MOK", "CAL", "COS", "XGEO_A", "XGEO_C"),
        Full_Name = c(
          "Location nodes", "Clifton Court Forebay", "Tracy Pumping Plant", "Vernalis", "= FPT",
          "Mokelumne River", "Calaveras River", "Cosumnes River", "Georgiana Slough", "Georgiana Slough"
        ),
        Description = c(
          "One or a list of location node numbers from DSM2 model",
          "Reservoir inflow of the export facility Clifton Court Forebay",
          "Reservoir inflow of the export facility Tracy Pumping Plant",
          "San Joaquin River flow at Vernalis",
          "Sacramento River flow at Freeport",
          "Mokelumne River before its confluence with Cosumnes River",
          "Calaveras River flow from New Hogan Lake Reservoir outflow",
          "Cosumnes River at Michigan Bar",
          "Sacramento River above Delta Cross Channel",
          "Sacramento River below Georgiana Slough"
        ),
        QAQC_Daily_Data = qaqc_source_values,
        Real_Time_Data = realtime_source_values,
        stringsAsFactors = FALSE
      )
      
      input_data$QAQC_Daily_Data <- lapply(input_data$QAQC_Daily_Data, gt::html)
      input_data$Real_Time_Data <- lapply(input_data$Real_Time_Data, gt::html)
      
      input_data |>
        gt::gt() |>
        gt::cols_hide(columns = Group_ID) |>
        gt::cols_label(
          Acronym = "Model Input Acronyms",
          Full_Name = "Full Name",
          Description = "Description",
          QAQC_Daily_Data = "QA/QC Daily Data",
          Real_Time_Data = "Real-Time Data"
        ) |>
        gt::tab_row_group(label = gt::html(group_label(1)), rows = Group_ID == "input_1", id = "input_1") |>
        gt::tab_row_group(label = gt::html(group_label(2, exp_equation)), rows = Group_ID == "input_2", id = "input_2") |>
        gt::tab_row_group(label = gt::html(group_label(3)), rows = Group_ID == "input_3", id = "input_3") |>
        gt::tab_row_group(label = gt::html(group_label(4)), rows = Group_ID == "input_4", id = "input_4") |>
        gt::tab_row_group(label = gt::html(group_label(5, east_equation)), rows = Group_ID == "input_5", id = "input_5") |>
        gt::tab_row_group(label = gt::html(group_label(6, xgeo_equation)), rows = Group_ID == "input_6", id = "input_6") |>
        gt::row_group_order(groups = paste0("input_", 1:6)) |>
        gt::text_transform(
          locations = gt::cells_body(columns = Acronym),
          fn = function(x) vapply(x, badge, character(1))
        ) |>
        gt::text_transform(
          locations = gt::cells_body(columns = c(QAQC_Daily_Data, Real_Time_Data)),
          fn = identity
        ) |>
        gt::cols_width(
          Acronym ~ gt::px(170),
          Full_Name ~ gt::px(220),
          Description ~ gt::px(480),
          QAQC_Daily_Data ~ gt::px(280),
          Real_Time_Data ~ gt::px(280)
        ) |>
        gt::cols_align(align = "center", columns = everything()) |>
        gt::tab_style(
          style = list(
            gt::cell_text(align = "center", v_align = "middle"),
            gt::cell_borders(sides = c("left", "right", "bottom"), color = "#c7d7da", weight = gt::px(1))
          ),
          locations = gt::cells_body(columns = everything())
        ) |>
        gt::tab_style(
          style = list(
            gt::cell_fill(color = "#eef7f9"),
            gt::cell_text(align = "left", v_align = "middle", size = "medium"),
            gt::cell_borders(sides = c("top", "bottom"), color = "#0a7e8c", weight = gt::px(1))
          ),
          locations = gt::cells_row_groups()
        ) |>
        gt::tab_style(
          style = list(
            gt::cell_fill(color = "#0a7e8c"),
            gt::cell_text(color = "white", weight = "bold", align = "center", v_align = "middle"),
            gt::cell_borders(sides = c("left", "right", "top", "bottom"), color = "#c7d7da", weight = gt::px(1))
          ),
          locations = gt::cells_column_labels(columns = everything())
        ) |>
        gt::tab_source_note(
          source_note = gt::html(
            "<strong>Note:</strong> For some QA/QC data sources, only reviewed or approved stage data are available. Corresponding discharge values are calculated using a stage–discharge rating curve."
          )
        ) |>
        gt::tab_style(
          style = gt::cell_text(align = "left", color = "#555555", size = gt::px(12)),
          locations = gt::cells_source_notes()
        ) |>
        gt::tab_options(
          table.width = gt::pct(100),
          table.font.names = "Segoe UI",
          table.font.size = gt::px(14),
          data_row.padding = gt::px(10),
          row_group.padding = gt::px(10)
        )
    })


  output$ptm_parameters_table <- render_gt({
      
      map_link <- function(text, input_id) paste0(
        "<a href='#' onclick=\"",
        "Shiny.setInputValue('", input_id, "', Date.now(), {priority:'event'});",
        "return false;",
        "\">", text, "</a>"
      )
      
      data <- data.frame(
        Names = c(
          "Number of Trees",
          "Number of Leaves per Tree",
          "Total Number of Leaf Nodes",
          "Number of Split Nodes per Tree",
          "Total Number of Split Nodes",
          "Maximum Observed Tree Depth",
          "`DSM2_Node`",
          "`EXP`",
          "`VER`",
          "`SAC`",
          "`EAST`",
          "`XGEO`"
        ),
        `7-day` = c(
          "1000",
          "128",
          "12800",
          "127",
          "12700",
          "35",
          map_link("15 available nodes", "open_node_map_7day"),
          "321.580 - 14763.154",
          "639.961 - 62032.795",
          "6155.703 - 86690.764",
          "13.438 - 24552.754",
          "1808.487 - 14164.103"
        ),
        `30-day` = c(
          "1000",
          "128",
          "12800",
          "127",
          "12700",
          "36",
          map_link("39 available nodes", "open_node_map_30day"),
          "321.580 - 14763.154",
          "639.961 - 62032.795",
          "6155.703 - 86690.764",
          "13438 - 24552.754",
          "1808.487 - 14164.103"
        ),
        check.names = FALSE
      )
      
      data$`7-day` <- lapply(data$`7-day`, gt::html)
      data$`30-day` <- lapply(data$`30-day`, gt::html)
      
      gt(data) |>
        tab_row_group(
          label = html("<strong><em>Parameters</em></strong>"),
          rows = 1:6
        ) |>
        tab_row_group(
          label = html("<strong><em>Data Range</em></strong>"),
          rows = 7:12
        ) |>
        fmt_markdown(
          columns = Names,
          rows = 7:12
        ) |>
        cols_align(
          align = "center",
          columns = everything()
        ) |>
        tab_style(
          style = cell_text(
            weight = "bold",
            align = "center"
          ),
          locations = cells_column_labels()
        ) |>
        tab_style(
          style = cell_text(
            align = "center",
            size = "medium"
          ),
          locations = cells_row_groups()
        ) |>
        tab_style(
          style = cell_borders(
            sides = c("left", "right", "top", "bottom"),
      paste0(
        "<div style='display:flex;align-items:center;gap:8px;white-space:nowrap;'>",
        badge(result), "<strong style='color:#333333;'>=</strong>", rhs, arrow, "</div>"
      )
    }
    
    exp_equation <- equation("EXP", c("CCF", "TPP"), "+")
    east_equation <- equation("EAST", c("MOK", "CAL", "COS"), c("+", "+"))
    xgeo_equation <- equation("XGEO", c("XGEO_A", "XGEO_C"), "-")
    
    qaqc_source_values <- c(
      "Click the link in <strong> Model Parameters </strong> to view the map",
      link("Historical O & M Reports - Available by Request", "https://water.ca.gov/Programs/State-Water-Project/Operations-and-Maintenance/Monthly-and-Annual-Operations-Reports"),
      link("Historical O & M Reports - Available by Request", "https://water.ca.gov/Programs/State-Water-Project/Operations-and-Maintenance/Monthly-and-Annual-Operations-Reports"),
      link("USGS - 11303500", "https://waterdata.usgs.gov/monitoring-location/USGS-11303500/#dataTypeId=daily-00060-0&period=P1Y&showFieldMeasurements=true"),
      link("USGS - 11447650", "https://waterdata.usgs.gov/monitoring-location/USGS-11447650/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true"),
      link("USGS - 11325500", "https://waterdata.usgs.gov/monitoring-location/11325500/"),
      link("USACE - CA10109", "https://water.usace.army.mil/overview/spk/locations/new%20hogan"),
      link("USGS - 11335000", "https://waterdata.usgs.gov/monitoring-location/USGS-11335000/#dataTypeId=daily-00060-0&period=P1Y&showFieldMeasurements=true"),
      link("USGS - 11447890", "https://waterdata.usgs.gov/monitoring-location/USGS-11447890/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true"),
      link("USGS - 11447905", "https://waterdata.usgs.gov/monitoring-location/USGS-11447905/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true")
    )
    
    realtime_source_values <- c(
      "Click the link in <strong> Model Parameters </strong> to view the map",
      link("CDEC - CLC", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=CLC"),
      link("CDEC - TRP", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=TRP"),
      link("CDEC - VNS", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=VNS"),
      link("CDEC - FPT", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=FPT"),
      link("USACE - CA00173", "https://water.usace.army.mil/overview/spk/locations/camanche"),
      link("CDEC - NHG", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=NHG"),
      link("USGS - 11335000", "https://waterdata.usgs.gov/monitoring-location/USGS-11335000"),
      link("USGS - 11447890", "https://waterdata.usgs.gov/monitoring-location/USGS-11447890"),
      link("USGS - 11447905", "https://waterdata.usgs.gov/monitoring-location/USGS-11447905")
    )
    
    input_data <- data.frame(
      Group_ID = c("input_1", "input_2", "input_2", "input_3", "input_4", "input_5", "input_5", "input_5", "input_6", "input_6"),
      Acronym = c("Location", "CCF", "TPP", "VNS", "SAC", "MOK", "CAL", "COS", "XGEO_A", "XGEO_C"),
      Full_Name = c(
        "Location nodes", "Clifton Court Forebay", "Tracy Pumping Plant", "Vernalis", "= FPT",
        "Mokelumne River", "Calaveras River", "Cosumnes River", "Georgiana Slough", "Georgiana Slough"
      ),
      Description = c(
        "One or a list of location node numbers from DSM2 model",
        "Reservoir inflow of the export facility Clifton Court Forebay",
        "Reservoir inflow of the export facility Tracy Pumping Plant",
        "San Joaquin River flow at Vernalis",
        "Sacramento River flow at Freeport",
        "Mokelumne River before its confluence with Cosumnes River",
        "Calaveras River flow from New Hogan Lake Reservoir outflow",
        "Cosumnes River at Michigan Bar",
        "Sacramento River above Delta Cross Channel",
        "Sacramento River below Georgiana Slough"
      ),
      QAQC_Daily_Data = qaqc_source_values,
      Real_Time_Data = realtime_source_values,
      stringsAsFactors = FALSE
    )
    
    input_data$QAQC_Daily_Data <- lapply(input_data$QAQC_Daily_Data, gt::html)
    input_data$Real_Time_Data <- lapply(input_data$Real_Time_Data, gt::html)
    
    input_data |>
      gt::gt() |>
      gt::cols_hide(columns = Group_ID) |>
      gt::cols_label(
        Acronym = "Model Input Acronyms",
        Full_Name = "Full Name",
        Description = "Description",
        QAQC_Daily_Data = "QA/QC Daily Data",
        Real_Time_Data = "Real-Time Data"
      ) |>
      gt::tab_row_group(label = gt::html(group_label(1)), rows = Group_ID == "input_1", id = "input_1") |>
      gt::tab_row_group(label = gt::html(group_label(2, exp_equation)), rows = Group_ID == "input_2", id = "input_2") |>
      gt::tab_row_group(label = gt::html(group_label(3)), rows = Group_ID == "input_3", id = "input_3") |>
      gt::tab_row_group(label = gt::html(group_label(4)), rows = Group_ID == "input_4", id = "input_4") |>
      gt::tab_row_group(label = gt::html(group_label(5, east_equation)), rows = Group_ID == "input_5", id = "input_5") |>
      gt::tab_row_group(label = gt::html(group_label(6, xgeo_equation)), rows = Group_ID == "input_6", id = "input_6") |>
      gt::row_group_order(groups = paste0("input_", 1:6)) |>
      gt::text_transform(
        locations = gt::cells_body(columns = Acronym),
        fn = function(x) vapply(x, badge, character(1))
      ) |>
      gt::text_transform(
        locations = gt::cells_body(columns = c(QAQC_Daily_Data, Real_Time_Data)),
        fn = identity
      ) |>
      gt::cols_width(
        Acronym ~ gt::px(170),
        Full_Name ~ gt::px(220),
        Description ~ gt::px(480),
        QAQC_Daily_Data ~ gt::px(280),
        Real_Time_Data ~ gt::px(280)
      ) |>
      gt::cols_align(align = "center", columns = everything()) |>
      gt::tab_style(
        style = list(
          gt::cell_text(align = "center", v_align = "middle"),
          gt::cell_borders(sides = c("left", "right", "bottom"), color = "#c7d7da", weight = gt::px(1))
        ),
        locations = gt::cells_body(columns = everything())
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#eef7f9"),
          gt::cell_text(align = "left", v_align = "middle", size = "medium"),
          gt::cell_borders(sides = c("top", "bottom"), color = "#0a7e8c", weight = gt::px(1))
        ),
        locations = gt::cells_row_groups()
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#0a7e8c"),
          gt::cell_text(color = "white", weight = "bold", align = "center", v_align = "middle"),
          gt::cell_borders(sides = c("left", "right", "top", "bottom"), color = "#c7d7da", weight = gt::px(1))
        ),
        locations = gt::cells_column_labels(columns = everything())
      ) |>
      gt::tab_source_note(
        source_note = gt::html(
          "<strong>Note:</strong> For some QA/QC data sources, only reviewed or approved stage data are available. Corresponding discharge values are calculated using a stage–discharge rating curve."
        )
      ) |>
      gt::tab_style(
        style = gt::cell_text(align = "left", color = "#555555", size = gt::px(12)),
        locations = gt::cells_source_notes()
      ) |>
      gt::tab_options(
        table.width = gt::pct(100),
        table.font.names = "Segoe UI",
        table.font.size = gt::px(14),
        data_row.padding = gt::px(10),
        row_group.padding = gt::px(10)
      )
  })
  
  output$horizon_inputs_table <- gt::render_gt({
    
    badge <- function(text) sprintf(
      "<span style='display:inline-block;background:#f9e9ee;color:#c7254e;border:1px solid #efcbd5;border-radius:4px;padding:3px 8px;font-family:monospace;white-space:nowrap;'>%s</span>",
      text
    )
    
    link <- function(text, url) sprintf(
      "<a href='%s' target='_blank' rel='noopener noreferrer'>%s</a>",
      url, text
    )
    
    group_label <- function(number, content = "") sprintf(
      "<div style='display:flex;align-items:center;gap:12px;width:100%%;min-height:46px;'><strong>Input Number %s</strong>%s</div>",
      number, content
    )
    
    arrow <- "<span style='display:inline-flex;align-items:center;margin-left:6px;color:#0a7e8c;font-size:30px;line-height:1;transform:translateY(12px);'><i class='fa fa-level-down' aria-hidden='true'></i></span>"
    
    equation <- function(result, terms, operators) {
      rhs <- badge(terms[1])
      if (length(terms) > 1) {
        for (i in seq_len(length(terms) - 1)) {
          rhs <- paste0(
            rhs,
            sprintf("<strong style='color:#333333;'>%s</strong>", operators[i]),
            badge(terms[i + 1])
          )
        }
      }
      paste0(
        "<div style='display:flex;align-items:center;gap:8px;white-space:nowrap;'>",
        badge(result), "<strong style='color:#333333;'>=</strong>", rhs, arrow, "</div>"
      )
    }
    
    exp_equation <- equation("EXP", c("CCF", "TPP"), "+")
    east_equation <- equation("EAST", c("MOK", "CAL", "COS"), c("+", "+"))
    xgeo_equation <- equation("XGEO", c("XGEO_A", "XGEO_C"), "-")
    
    xgeo_group <- paste0(
      "<span style='white-space:nowrap;'>Combined interior Delta flow</span>",
      "<strong style='color:#333333;'>=</strong>",
      xgeo_equation
    )
    
    qaqc_source_values <- c(
      link("USGS - 11447650", "https://waterdata.usgs.gov/monitoring-location/USGS-11447650/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true"),
      link("Historical O & M Reports - Available by Request", "https://water.ca.gov/Programs/State-Water-Project/Operations-and-Maintenance/Monthly-and-Annual-Operations-Reports"),
      link("Historical O & M Reports - Available by Request", "https://water.ca.gov/Programs/State-Water-Project/Operations-and-Maintenance/Monthly-and-Annual-Operations-Reports"),
      link("USGS - 11325500", "https://waterdata.usgs.gov/monitoring-location/USGS-11325500/"),
      link("USACE - CA10109", "https://water.usace.army.mil/overview/spk/locations/new%20hogan"),
      link("USGS - 11335000", "https://waterdata.usgs.gov/monitoring-location/USGS-11335000/#dataTypeId=daily-00060-0&period=P1Y&showFieldMeasurements=true"),
      link("USGS - 11447890", "https://waterdata.usgs.gov/monitoring-location/USGS-11447890/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true"),
      link("USGS - 11447905", "https://waterdata.usgs.gov/monitoring-location/USGS-11447905/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true"),
      "{15+5k∣k=0,1,…,13}"
    )
    
    realtime_source_values <- c(
      link("CDEC - FPT", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=FPT"),
      link("CDEC - CLC", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=CLC"),
      link("CDEC - TRP", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=TRP"),
      link("USACE - CA00173", "https://water.usace.army.mil/overview/spk/locations/camanche"),
      link("CDEC - NHG", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=NHG"),
      link("USGS - 11335000", "https://waterdata.usgs.gov/monitoring-location/USGS-11335000/"),
      link("USGS - 11447890", "https://waterdata.usgs.gov/monitoring-location/USGS-11447890/"),
      link("USGS - 11447905", "https://waterdata.usgs.gov/monitoring-location/USGS-11447905/"),
      "{15+5k∣k=0,1,…,13}"
    )
    
    input_data <- data.frame(
      Group_ID = c("input_1", "input_2", "input_2", "input_3", "input_3", "input_3", "input_4", "input_4", "input_5"),
      Acronym = c("SAC", "CCF", "TPP", "MOK", "CAL", "COS", "XGEO_A", "XGEO_C", "ERL"),
      Full_Name = c(
        "= FPT", "Clifton Court Forebay", "Tracy Pumping Plant",
        "Mokelumne River", "Calaveras River", "Cosumnes River",
        "Georgiana Slough", "Georgiana Slough", "Entrainment Risk Level"
      ),
      Description = c(
        "Sacramento River flow at Freeport",
        "Reservoir inflow of the export facility Clifton Court Forebay",
        "Reservoir inflow of the export facility Tracy Pumping Plant",
        "Mokelumne River before its confluence with Cosumnes River",
        "Calaveras River flow from New Hogan Lake Reservoir outflow",
        "Cosumnes River flow at Michigan Bar",
        "Sacramento River above Delta Cross Channel",
        "Sacramento River below Georgiana Slough",
        "ERL expressed as an entrainment percentage from 15 to 80 in increments of 5"
      ),
      QAQC_Daily_Data = qaqc_source_values,
      Real_Time_Data = realtime_source_values,
      stringsAsFactors = FALSE
    )
    
    input_data$QAQC_Daily_Data <- lapply(input_data$QAQC_Daily_Data, gt::html)
    input_data$Real_Time_Data <- lapply(input_data$Real_Time_Data, gt::html)
    
    input_data |>
      gt::gt() |>
      gt::cols_hide(columns = Group_ID) |>
      gt::cols_label(
        Acronym = "Model Input Acronyms",
        Full_Name = "Full Name",
        Description = "Description",
        QAQC_Daily_Data = "QA/QC Daily Data",
        Real_Time_Data = "Real-Time Data"
      ) |>
      gt::tab_row_group(label = gt::html(group_label(1)), rows = Group_ID == "input_1", id = "input_1") |>
      gt::tab_row_group(label = gt::html(group_label(2, exp_equation)), rows = Group_ID == "input_2", id = "input_2") |>
      gt::tab_row_group(label = gt::html(group_label(3, east_equation)), rows = Group_ID == "input_3", id = "input_3") |>
      gt::tab_row_group(label = gt::html(group_label(4, xgeo_group)), rows = Group_ID == "input_4", id = "input_4") |>
      gt::tab_row_group(label = gt::html(group_label(5)), rows = Group_ID == "input_5", id = "input_5") |>
      gt::row_group_order(groups = paste0("input_", 1:5)) |>
      gt::text_transform(
        locations = gt::cells_body(columns = Acronym),
        fn = function(x) vapply(x, badge, character(1))
      ) |>
      gt::text_transform(
        locations = gt::cells_body(columns = c(QAQC_Daily_Data, Real_Time_Data)),
        fn = identity
      ) |>
      gt::cols_width(
        Acronym ~ gt::px(170),
        Full_Name ~ gt::px(220),
        Description ~ gt::px(480),
        QAQC_Daily_Data ~ gt::px(280),
        Real_Time_Data ~ gt::px(280)
      ) |>
      gt::cols_align(align = "center", columns = everything()) |>
      gt::tab_style(
        style = list(
          gt::cell_text(align = "center", v_align = "middle"),
          gt::cell_borders(
            sides = c("left", "right", "bottom"),
            color = "#c7d7da",
            weight = px(1)
          ),
          locations = list(
            cells_column_labels(),
            cells_body()
          )
        )
    })


  output$ecoptm_parameters_table <- render_gt({
      
      data <- data.frame(
        Names = c(
          "Number of Trees",
          "Number of Leaves per Tree",
          "Total Number of Leaf Nodes",
          "Number of Split Nodes per Tree",
          "Total Number of Split Nodes",
          "Maximum Observed Tree Depth",
          "`FPT`",
          "`YOL`",
          "`MOK`",
          "`DCC`"
        ),
        Interior = c(
          "500",
          "126 - 128",
          "63994",
          "125 - 127",
          "63494",
          "31",
          "6311.930 - 86690.764",
          "54.870 - 192337.793",
          "66.176 - 8652.699",
          "0, 1"
        ),
        Survival = c(
          "500",
          "124 - 128",
          "63988",
          "123 - 127",
          "63488",
          "30",
          "6311.930 - 86690.764",
          "54.870 - 192337.793",
          "66.176 - 8652.699",
          "0, 1"
        ),
        check.names = FALSE
      )
      
      gt(data) |>
        tab_row_group(
          label = html("<strong><em>Parameters</em></strong>"),
          rows = 1:6,
          id = "parameters"
        ) |>
        tab_row_group(
          label = html("<strong><em>Range</em></strong>"),
          rows = 7:10,
          id = "range"
        ) |>
        row_group_order(
          groups = c("parameters", "range")
        ) |>
        fmt_markdown(
          columns = Names,
          rows = 7:10
        ) |>
        cols_align(
          align = "center",
          columns = everything()
        ) |>
        tab_style(
          style = cell_text(
            weight = "bold",
            align = "center"
          ),
          locations = cells_column_labels()
        ) |>
        tab_style(
          style = cell_text(
        ),
        locations = gt::cells_body(columns = everything())
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#eef7f9"),
          gt::cell_text(align = "left", v_align = "middle", size = "medium"),
          gt::cell_borders(
            sides = c("top", "bottom"),
            color = "#0a7e8c",
            weight = gt::px(1)
          )
        ),
        locations = gt::cells_row_groups()
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#0a7e8c"),
          gt::cell_text(
            color = "white",
            weight = "bold",
            style = "italic",
            align = "center",
            size = "medium"
          ),
          locations = cells_row_groups()
        ) |>
        tab_style(
          style = cell_borders(
            sides = c("left", "right", "top", "bottom"),
            color = "#c7d7da",
            weight = px(1)
          ),
          locations = list(
            cells_column_labels(),
            cells_body()
          )
        )
    })
        ),
        locations = gt::cells_column_labels(columns = everything())
      ) |>
      gt::tab_source_note(
        source_note = gt::html(
          "<strong>Note:</strong> For some QA/QC data sources, only reviewed or approved stage data are available. Corresponding discharge values are calculated using a stage–discharge rating curve."
        )
      ) |>
      gt::tab_style(
        style = gt::cell_text(
          align = "left",
          color = "#555555",
          size = gt::px(12)
        ),
        locations = gt::cells_source_notes()
      ) |>
      gt::tab_options(
        table.width = gt::pct(100),
        table.font.names = "Segoe UI",
        table.font.size = gt::px(14),
        data_row.padding = gt::px(10),
        row_group.padding = gt::px(10)
      )
  })

  node_data <- readr::read_csv(
    file.path("STN_EMULATOR", "models", "delta_locations_coordinates.csv"),
    show_col_types = FALSE
  )
  
  nodes_7day <- c(1,7,21,25,34,39,41,75,86,99,113,145,174,232,469)
  
  node_data_7day <- node_data[
    node_data$DSM2_Node %in% nodes_7day,
    ,
    drop = FALSE
  ]
  
  create_node_map <- function(data, marker_color) {
    leaflet::leaflet(data) |>
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery) |>
      leaflet::addAwesomeMarkers(
        lng = ~X,
        lat = ~Y,
        icon = leaflet::awesomeIcons(
          icon = "map-marker",
          library = "fa",
          markerColor = marker_color,
          iconColor = "white"
        ),
        popup = ~paste0(
          "<strong>Node: </strong>", DSM2_Node,
          "<br><strong>Location: </strong>", Location,
          "<br><strong>Region: </strong>", Region
        )
      ) |>
      leaflet::addLabelOnlyMarkers(
        lng = ~X,
        lat = ~Y,
        label = ~as.character(DSM2_Node),
        labelOptions = leaflet::labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          offset = c(0, -17),
          textsize = "11px",
          style = list(
            "color" = "white",
            "font-weight" = "bold",
            "text-shadow" = "0 0 2px black"
          )
        )
      ) |>
      leaflet::fitBounds(
        lng1 = min(data$X, na.rm = TRUE),
        lat1 = min(data$Y, na.rm = TRUE),
        lng2 = max(data$X, na.rm = TRUE),
        lat2 = max(data$Y, na.rm = TRUE)
      )
  }
  
  observeEvent(input$open_node_map_7day, {
    showModal(
      modalDialog(
        title = "DSM2 Location Nodes: 7-Day Model",
        leaflet::leafletOutput("node_map_7day", height = "650px"),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  
  observeEvent(input$open_node_map_30day, {
    showModal(
      modalDialog(
        title = "DSM2 Location Nodes: 30-Day Model",
        leaflet::leafletOutput("node_map_30day", height = "650px"),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  
  output$node_map_7day <- leaflet::renderLeaflet({
    create_node_map(node_data_7day, "orange")
  })
  
  output$node_map_30day <- leaflet::renderLeaflet({
    create_node_map(node_data, "blue")
  })
  
  output$ptm_parameters_table <- render_gt({
    
    map_link <- function(text, input_id) paste0(
      "<a href='#' onclick=\"",
      "Shiny.setInputValue('", input_id, "', Date.now(), {priority:'event'});",
      "return false;",
      "\">", text, "</a>"
    )
    
    data <- data.frame(
      Names = c(
        "Number of Trees",
        "Number of Leaves per Tree",
        "Total Number of Leaf Nodes",
        "Number of Split Nodes per Tree",
        "Total Number of Split Nodes",
        "Maximum Observed Tree Depth",
        "`DSM2_Node`",
        "`EXP`",
        "`VER`",
        "`SAC`",
        "`EAST`",
        "`XGEO`"
      ),
      `7-day` = c(
        "1000",
        "128",
        "12800",
        "127",
        "12700",
        "35",
        map_link("15 available nodes", "open_node_map_7day"),
        "321.580 - 14763.154",
        "639.961 - 62032.795",
        "6155.703 - 86690.764",
        "13.438 - 24552.754",
        "1808.487 - 14164.103"
      ),
      `30-day` = c(
        "1000",
        "128",
        "12800",
        "127",
        "12700",
        "36",
        map_link("39 available nodes", "open_node_map_30day"),
        "321.580 - 14763.154",
        "639.961 - 62032.795",
        "6155.703 - 86690.764",
        "13438 - 24552.754",
        "1808.487 - 14164.103"
      ),
      check.names = FALSE
    )
    
    data$`7-day` <- lapply(data$`7-day`, gt::html)
    data$`30-day` <- lapply(data$`30-day`, gt::html)
    
    gt(data) |>
      tab_row_group(
        label = html("<strong><em>Parameters</em></strong>"),
        rows = 1:6
      ) |>
      tab_row_group(
        label = html("<strong><em>Data Range</em></strong>"),
        rows = 7:12
      ) |>
      fmt_markdown(
        columns = Names,
        rows = 7:12
      ) |>
      cols_align(
        align = "center",
        columns = everything()
      ) |>
      tab_style(
        style = cell_text(
          weight = "bold",
          align = "center"
        ),
        locations = cells_column_labels()
      ) |>
      tab_style(
        style = cell_text(
          align = "center",
          size = "medium"
        ),
        locations = cells_row_groups()
      ) |>
      tab_style(
        style = cell_borders(
          sides = c("left", "right", "top", "bottom"),
          color = "#c7d7da",
          weight = px(1)
        ),
        locations = list(
          cells_column_labels(),
          cells_body()
        )
      )
  })
  
  output$ecoptm_parameters_table <- render_gt({
    
    data <- data.frame(
      Names = c(
        "Number of Trees",
        "Number of Leaves per Tree",
        "Total Number of Leaf Nodes",
        "Number of Split Nodes per Tree",
        "Total Number of Split Nodes",
        "Maximum Observed Tree Depth",
        "`FPT`",
        "`YOL`",
        "`MOK`",
        "`DCC`"
      ),
      Interior = c(
        "500",
        "126 - 128",
        "63994",
        "125 - 127",
        "63494",
        "31",
        "6311.930 - 86690.764",
        "54.870 - 192337.793",
        "66.176 - 8652.699",
        "0, 1"
      ),
      Survival = c(
        "500",
        "124 - 128",
        "63988",
        "123 - 127",
        "63488",
        "30",
        "6311.930 - 86690.764",
        "54.870 - 192337.793",
        "66.176 - 8652.699",
        "0, 1"
      ),
      check.names = FALSE
    )
    
    gt(data) |>
      tab_row_group(
        label = html("<strong><em>Parameters</em></strong>"),
        rows = 1:6,
        id = "parameters"
      ) |>
      tab_row_group(
        label = html("<strong><em>Range</em></strong>"),
        rows = 7:10,
        id = "range"
      ) |>
      row_group_order(
        groups = c("parameters", "range")
      ) |>
      fmt_markdown(
        columns = Names,
        rows = 7:10
      ) |>
      cols_align(
        align = "center",
        columns = everything()
      ) |>
      tab_style(
        style = cell_text(
          weight = "bold",
          align = "center"
        ),
        locations = cells_column_labels()
      ) |>
      tab_style(
        style = cell_text(
          weight = "bold",
          style = "italic",
          align = "center",
          size = "medium"
        ),
        locations = cells_row_groups()
      ) |>
      tab_style(
        style = cell_borders(
          sides = c("left", "right", "top", "bottom"),
          color = "#c7d7da",
          weight = px(1)
        ),
        locations = list(
          cells_column_labels(),
          cells_body()
        )
      )
  })
  
  output$horizon_datarange_table <- render_gt({
    
    badge <- function(text) sprintf(
      "<span style='display:inline-block;background:#f9e9ee;color:#c7254e;border:1px solid #efcbd5;border-radius:4px;padding:3px 8px;font-family:monospace;white-space:nowrap;'>%s</span>",
      text
    )
    
    data <- data.frame(
      Names = c("SAC", "EXP", "EAST", "XGEO", "ERL"),
      `Data Range` = c(
        "6155.703 - 86690.764",
        "321.580 - 14763.154",
        "13.438 - 24552.754",
        "1808.487 - 14164.103",
        "15:5:80, int"
      ),
      check.names = FALSE
    )
    
    gt(data) |>
      text_transform(
        locations = cells_body(columns = Names),
        fn = function(x) vapply(x, badge, character(1))
      ) |>
      cols_align(align = "center", columns = everything()) |>
      tab_style(
        style = cell_text(weight = "bold", align = "center"),
        locations = cells_column_labels()
      ) |>
      tab_style(
        style = cell_borders(
          sides = c("left", "right", "top", "bottom"),
          color = "#c7d7da",
          weight = px(1)
        ),
        locations = list(cells_column_labels(), cells_body())
      ) |>
      cols_width(Names ~ px(220), `Data Range` ~ px(300)) |>
      tab_options(
        table.width = pct(100),
        table.font.names = "Segoe UI",
        table.font.size = px(14),
        data_row.padding = px(8)
      )
  })

  output$horizon_parameter_table <- render_gt({
    
    data <- data.frame(
      Names = c(
        "Number of Boosted Trees",
        "Total Number of Nodes",
        "Total Number of Internal Nodes",
        "Total Number of Leaf Nodes",
        "Minimum Nodes per Tree",
        "Maximum Nodes per Tree",
        "Maximum Tree Depth",
        "Number of Features"
      ),
      Parameters = c("1,200", "1,209,428", "604,114", "605,314", "475", "1567", "10", "5"),
      check.names = FALSE
    )
    
    gt(data) |>
      cols_align(align = "center", columns = everything()) |>
      tab_style(
        style = cell_text(weight = "bold", align = "center"),
        locations = cells_column_labels()
      ) |>
      tab_style(
        style = cell_borders(
          sides = c("left", "right", "top", "bottom"),
          color = "#c7d7da",
          weight = px(1)
        ),
        locations = list(cells_column_labels(), cells_body())
      ) |>
      cols_width(Names ~ px(320), Parameters ~ px(220)) |>
      tab_options(
        table.width = pct(100),
        table.font.names = "Segoe UI",
        table.font.size = px(14),
        data_row.padding = px(8)
      )
  })
  
}
