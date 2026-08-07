make_station_set <- function(MIA, Map_Label = MIA) {
  if (length(MIA) != length(Map_Label)) {
    stop("MIA and Map_Label must have the same length.")
  }
  
  data.frame(
    MIA = MIA,
    Map_Label = Map_Label,
    stringsAsFactors = FALSE
  )
}


STATION_SETS <- list(
  
  ptm_day7 = make_station_set(
    MIA = c(
      "CCF", "TPP", "VNS", "FPT", "MOK",
      "CAL", "COS", "XGEO_A", "XGEO_C"
    ),
    Map_Label = c(
      "CCF", "TPP", "VNS", "SAC", "MOK",
      "CAL", "COS", "XGEO_A", "XGEO_C"
    )
  ),
  
  ptm_day30 = make_station_set(
    MIA = c(
      "CCF", "TPP", "VNS", "FPT", "MOK",
      "CAL", "COS", "XGEO_A", "XGEO_C"
    ),
    Map_Label = c(
      "CCF", "TPP", "VNS", "SAC", "MOK",
      "CAL", "COS", "XGEO_A", "XGEO_C"
    )
  ),
  
  eco_ptm = make_station_set(
    MIA = c(
      "FPT", "SACWEIR", "FREWEIR", "MOK", "XGEO_A"
    ),
    Map_Label = c(
      "FPT", "SACWEIR", "FREWEIR", "MOK", "DCC"
    )
  ),
  
  horizon = make_station_set(
    MIA = c(
      "FPT", "CCF", "TPP", "MOK", "CAL", "COS", "XGEO_A", "XGEO_C"
    ),
    Map_Label = c(
      "SAC", "CCF", "TPP", "MOK", "CAL", "COS", "XGEO_A", "XGEO_C"
    )
  )
)


NODE_SETS <- list(
  
  ptm_day7 = c(
    1, 7, 21, 25, 34,
    39, 41, 75, 86, 99,
    113, 145, 174, 232, 469
  ),
  
  all_nodes = "ALL"
)


MAP_UI_GROUPS <- list(
  
  ptm_combined = list(
    ui_output_id = "ptm_combined_maps_ui",
    tabset_id = "ptm_combined_map_tabs",
    show_tabs = TRUE,
    tab_type = "tabs",
    height = "620px"
  ),
  
  ptm_station_only = list(
    ui_output_id = "ptm_station_only_maps_ui",
    tabset_id = "ptm_station_only_map_tabs",
    show_tabs = TRUE,
    tab_type = "tabs",
    height = "620px"
  ),
  
  eco_ptm = list(
    ui_output_id = "eco_ptm_map_ui",
    show_tabs = FALSE,
    height = "650px"
  ),
  
  horizon = list(
    ui_output_id = "horizon_map_ui",
    show_tabs = FALSE,
    height = "650px"
  )
)


MAP_DEFINITIONS <- list(
  
  ptm_combined_7day = list(
    output_id = "node_station_map_7day",
    ui_group = "ptm_combined",
    tab_title = "7-Day",
    station_set = "ptm_day7",
    node_set = "ptm_day7",
    include_nodes = TRUE,
    node_color = "orange",
    station_color = "green"
  ),
  
  ptm_combined_30day = list(
    output_id = "node_station_map_30day",
    ui_group = "ptm_combined",
    tab_title = "30-Day",
    station_set = "ptm_day30",
    node_set = "all_nodes",
    include_nodes = TRUE,
    node_color = "blue",
    station_color = "green"
  ),
  
  ptm_station_only_7day = list(
    output_id = "station_only_map_7day",
    ui_group = "ptm_station_only",
    tab_title = "7-Day",
    station_set = "ptm_day7",
    node_set = NULL,
    include_nodes = FALSE,
    node_color = "orange",
    station_color = "green"
  ),
  
  ptm_station_only_30day = list(
    output_id = "station_only_map_30day",
    ui_group = "ptm_station_only",
    tab_title = "30-Day",
    station_set = "ptm_day30",
    node_set = NULL,
    include_nodes = FALSE,
    node_color = "blue",
    station_color = "green"
  ),
  
  eco_ptm = list(
    output_id = "eco_ptm_map",
    ui_group = "eco_ptm",
    tab_title = "ECO-PTM",
    station_set = "eco_ptm",
    node_set = NULL,
    include_nodes = FALSE,
    node_color = "blue",
    station_color = "green"
  ),
  
  horizon = list(
    output_id = "horizon_map",
    ui_group = "horizon",
    tab_title = "Horizon",
    station_set = "horizon",
    node_set = NULL,
    include_nodes = FALSE,
    node_color = "blue",
    station_color = "green"
  )
)


MODEL_MAP_FILES <- list(
  node_csv = file.path(
    "STN_EMULATOR",
    "models",
    "delta_locations_coordinates.csv"
  ),
  station_xlsx = file.path(
    "STN_EMULATOR",
    "models",
    "Station_Location.xlsx"
  )
)
get_model_map_delta_layers <- local({
  
  cached_layers <- NULL
  
  function() {
    
    if (!is.null(cached_layers)) {
      return(cached_layers)
    }
    
    boundary_path <- file.path(
      "STN_EMULATOR",
      "shapefiles",
      "Bay_Delta_Poly_New.shp"
    )
    
    channels_path <- file.path(
      "STN_EMULATOR",
      "shapefiles",
      "hydro_delta_marsh.shp"
    )
    
    if (!file.exists(boundary_path)) {
      warning(
        "Delta boundary shapefile was not found: ",
        boundary_path
      )
      
      boundary <- NULL
    } else {
      boundary <- sf::st_read(
        boundary_path,
        quiet = TRUE
      ) |>
        sf::st_make_valid() |>
        sf::st_transform(4326)
    }
    
    if (!file.exists(channels_path)) {
      warning(
        "Delta channel shapefile was not found: ",
        channels_path
      )
      
      channels <- NULL
    } else {
      channels <- sf::st_read(
        channels_path,
        quiet = TRUE
      ) |>
        sf::st_make_valid() |>
        sf::st_transform(4326)
    }
    
    cached_layers <<- list(
      boundary = boundary,
      channels = channels
    )
    
    cached_layers
  }
})


add_delta_background_layers <- function(map) {
  
  delta_layers <- get_model_map_delta_layers()
  
  if (!is.null(delta_layers$boundary)) {
    map <- map |>
      leaflet::addPolygons(
        data = delta_layers$boundary,
        fillColor = "#00BFC4",
        fillOpacity = 0.18,
        color = "#00E5FF",
        opacity = 0.95,
        weight = 2,
        group = "Delta Boundary"
      )
  }
  
  if (!is.null(delta_layers$channels)) {
    map <- map |>
      leaflet::addPolylines(
        data = delta_layers$channels,
        color = "#00FFFF",
        weight = 2,
        opacity = 0.95,
        group = "Delta Channels"
      )
  }
  
  map
}

MODEL_MAP_COLUMNS <- list(
  required_node = c(
    "DSM2_Node", "X", "Y", "Location", "Region"
  ),
  required_station = c(
    "MIA", "LAT", "LON"
  ),
  station_mia_original = "Model Input Acronyms"
)


MODEL_MAP_STYLE <- list(
  provider = leaflet::providers$Esri.WorldImagery,
  node_group = "DSM2 Nodes",
  station_group = "Model Input Stations",
  node_icon = "map-marker",
  station_icon = "map-marker",
  icon_library = "fa",
  icon_color = "white",
  node_label_offset = c(0, -17),
  station_label_offset = c(0, -17),
  node_label_size = "11px",
  station_label_size = "11px",
  extent_padding_fraction = 0.05,
  extent_minimum_padding = 0.005,
  single_point_zoom = 12
)


validate_named_columns <- function(data, required_columns, data_name) {
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Missing columns in ",
        data_name,
        ": ",
        paste(missing_columns, collapse = ", ")
      )
    )
  }
}


validate_map_configuration <- function() {
  output_ids <- vapply(
    MAP_DEFINITIONS,
    function(config) config$output_id,
    character(1)
  )
  
  if (anyDuplicated(output_ids)) {
    stop("Duplicated map output IDs were found in MAP_DEFINITIONS.")
  }
  
  for (map_name in names(MAP_DEFINITIONS)) {
    config <- MAP_DEFINITIONS[[map_name]]
    
    if (!config$station_set %in% names(STATION_SETS)) {
      stop(
        paste0(
          "Unknown station_set for ",
          map_name,
          ": ",
          config$station_set
        )
      )
    }
    
    if (!config$ui_group %in% names(MAP_UI_GROUPS)) {
      stop(
        paste0(
          "Unknown ui_group for ",
          map_name,
          ": ",
          config$ui_group
        )
      )
    }
    
    if (
      isTRUE(config$include_nodes) &&
      (
        is.null(config$node_set) ||
        !config$node_set %in% names(NODE_SETS)
      )
    ) {
      stop(
        paste0(
          "Unknown or missing node_set for ",
          map_name,
          "."
        )
      )
    }
  }
}


build_station_popup <- function(data) {
  detail_columns <- setdiff(
    names(data),
    c("Map_Label", "LAT", "LON", "Popup")
  )
  
  format_popup_value <- function(value) {
    if (
      length(value) == 0 ||
      is.na(value) ||
      trimws(as.character(value)) == ""
    ) {
      return("N/A")
    }
    
    as.character(
      htmltools::htmlEscape(
        as.character(value)
      )
    )
  }
  
  vapply(
    seq_len(nrow(data)),
    function(i) {
      detail_lines <- vapply(
        detail_columns,
        function(column_name) {
          paste0(
            "<strong>",
            htmltools::htmlEscape(column_name),
            ":</strong> ",
            format_popup_value(data[[column_name]][i])
          )
        },
        character(1)
      )
      
      paste0(
        "<div style='min-width:240px;'>",
        "<strong style='font-size:15px;'>",
        htmltools::htmlEscape(data$Map_Label[i]),
        "</strong>",
        "<hr style='margin:6px 0;'>",
        paste(detail_lines, collapse = "<br>"),
        "<br><strong>Latitude:</strong> ",
        data$LAT[i],
        "<br><strong>Longitude:</strong> ",
        data$LON[i],
        "</div>"
      )
    },
    character(1)
  )
}


prepare_station_points <- function(station_data, station_config) {
  station_config$MIA <- trimws(
    as.character(station_config$MIA)
  )
  
  station_config$Map_Label <- trimws(
    as.character(station_config$Map_Label)
  )
  
  if (anyDuplicated(station_config$MIA)) {
    stop("Duplicated MIA values were found in a station set.")
  }
  
  matched_rows <- match(
    station_config$MIA,
    station_data$MIA
  )
  
  missing_mia <- station_config$MIA[
    is.na(matched_rows)
  ]
  
  if (length(missing_mia) > 0) {
    stop(
      paste0(
        "The following MIA values were not found in Station_Location.xlsx: ",
        paste(missing_mia, collapse = ", ")
      )
    )
  }
  
  result <- cbind(
    station_config,
    station_data[
      matched_rows,
      setdiff(names(station_data), "MIA"),
      drop = FALSE
    ]
  )
  
  result <- as.data.frame(
    result,
    check.names = FALSE
  )
  
  result$LAT <- suppressWarnings(
    as.numeric(result$LAT)
  )
  
  result$LON <- suppressWarnings(
    as.numeric(result$LON)
  )
  
  invalid_coordinates <- (
    !is.finite(result$LAT) |
      !is.finite(result$LON)
  )
  
  if (any(invalid_coordinates)) {
    stop(
      paste0(
        "Invalid coordinates were found for: ",
        paste(
          result$MIA[invalid_coordinates],
          collapse = ", "
        )
      )
    )
  }
  
  result$Popup <- build_station_popup(result)
  
  rownames(result) <- NULL
  
  result
}


prepare_node_sets <- function(node_data) {
  lapply(
    NODE_SETS,
    function(node_rule) {
      if (
        length(node_rule) == 1 &&
        is.character(node_rule) &&
        identical(node_rule, "ALL")
      ) {
        return(node_data)
      }
      
      node_data[
        node_data$DSM2_Node %in% node_rule,
        ,
        drop = FALSE
      ]
    }
  )
}


apply_map_extent <- function(map, longitude, latitude) {
  valid_coordinates <- (
    is.finite(longitude) &
      is.finite(latitude)
  )
  
  longitude <- longitude[valid_coordinates]
  latitude <- latitude[valid_coordinates]
  
  if (length(longitude) == 0) {
    return(map)
  }
  
  longitude_range <- range(longitude)
  latitude_range <- range(latitude)
  
  longitude_span <- diff(longitude_range)
  latitude_span <- diff(latitude_range)
  
  if (
    longitude_span == 0 &&
    latitude_span == 0
  ) {
    return(
      map |>
        leaflet::setView(
          lng = longitude[1],
          lat = latitude[1],
          zoom = MODEL_MAP_STYLE$single_point_zoom
        )
    )
  }
  
  longitude_padding <- max(
    longitude_span *
      MODEL_MAP_STYLE$extent_padding_fraction,
    MODEL_MAP_STYLE$extent_minimum_padding
  )
  
  latitude_padding <- max(
    latitude_span *
      MODEL_MAP_STYLE$extent_padding_fraction,
    MODEL_MAP_STYLE$extent_minimum_padding
  )
  
  map |>
    leaflet::fitBounds(
      lng1 = longitude_range[1] - longitude_padding,
      lat1 = latitude_range[1] - latitude_padding,
      lng2 = longitude_range[2] + longitude_padding,
      lat2 = latitude_range[2] + latitude_padding
    )
}


create_model_map <- function(
    station_points,
    node_points = NULL,
    include_nodes = FALSE,
    node_color = "blue",
    station_color = "green"
) {
  
  map <- leaflet::leaflet() |>
    leaflet::addProviderTiles(
      MODEL_MAP_STYLE$provider,
      group = "Esri World Imagery"
    ) |>
    add_delta_background_layers()
  
  layer_groups <- c(
    "Delta Boundary",
    "Delta Channels"
  )
  
  has_nodes <- (
    isTRUE(include_nodes) &&
      !is.null(node_points) &&
      nrow(node_points) > 0
  )
  
  has_stations <- (
    !is.null(station_points) &&
      nrow(station_points) > 0
  )
  
  if (has_nodes) {
    map <- map |>
      leaflet::addAwesomeMarkers(
        data = node_points,
        lng = ~X,
        lat = ~Y,
        group = MODEL_MAP_STYLE$node_group,
        icon = leaflet::awesomeIcons(
          icon = MODEL_MAP_STYLE$node_icon,
          library = MODEL_MAP_STYLE$icon_library,
          markerColor = node_color,
          iconColor = MODEL_MAP_STYLE$icon_color
        ),
        popup = ~paste0(
          "<strong>Node:</strong> ",
          DSM2_Node,
          "<br><strong>Location:</strong> ",
          Location,
          "<br><strong>Region:</strong> ",
          Region
        )
      ) |>
      leaflet::addLabelOnlyMarkers(
        data = node_points,
        lng = ~X,
        lat = ~Y,
        group = MODEL_MAP_STYLE$node_group,
        label = ~as.character(DSM2_Node),
        labelOptions = leaflet::labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          offset = MODEL_MAP_STYLE$node_label_offset,
          textsize = MODEL_MAP_STYLE$node_label_size,
          style = list(
            "color" = "white",
            "font-weight" = "bold",
            "text-shadow" = "0 0 2px black"
          )
        )
      )
    
    layer_groups <- c(
      layer_groups,
      MODEL_MAP_STYLE$node_group
    )
  }
  
  if (has_stations) {
    map <- map |>
      leaflet::addAwesomeMarkers(
        data = station_points,
        lng = ~LON,
        lat = ~LAT,
        group = MODEL_MAP_STYLE$station_group,
        icon = leaflet::awesomeIcons(
          icon = MODEL_MAP_STYLE$station_icon,
          library = MODEL_MAP_STYLE$icon_library,
          markerColor = station_color,
          iconColor = MODEL_MAP_STYLE$icon_color
        ),
        popup = ~Popup
      ) |>
      leaflet::addLabelOnlyMarkers(
        data = station_points,
        lng = ~LON,
        lat = ~LAT,
        group = MODEL_MAP_STYLE$station_group,
        label = ~Map_Label,
        labelOptions = leaflet::labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          offset = MODEL_MAP_STYLE$station_label_offset,
          textsize = MODEL_MAP_STYLE$station_label_size,
          style = list(
            "color" = "white",
            "font-weight" = "bold",
            "text-shadow" = "0 0 3px black"
          )
        )
      )
    
    layer_groups <- c(
      layer_groups,
      MODEL_MAP_STYLE$station_group
    )
  }
  
  map <- map |>
    leaflet::addLayersControl(
      baseGroups = c(
        "Esri World Imagery"
      ),
      overlayGroups = unique(layer_groups),
      options = leaflet::layersControlOptions(
        collapsed = FALSE
      )
    )
  
  longitude <- if (has_stations) {
    station_points$LON
  } else {
    numeric(0)
  }
  
  latitude <- if (has_stations) {
    station_points$LAT
  } else {
    numeric(0)
  }
  
  if (has_nodes) {
    longitude <- c(
      node_points$X,
      longitude
    )
    
    latitude <- c(
      node_points$Y,
      latitude
    )
  }
  
  apply_map_extent(
    map = map,
    longitude = longitude,
    latitude = latitude
  )
}



build_map_group_ui <- function(group_key) {
  group_config <- MAP_UI_GROUPS[[group_key]]
  
  map_configs <- MAP_DEFINITIONS[
    vapply(
      MAP_DEFINITIONS,
      function(config) identical(
        config$ui_group,
        group_key
      ),
      logical(1)
    )
  ]
  
  if (length(map_configs) == 0) {
    return(NULL)
  }
  
  height <- group_config$height
  
  if (!isTRUE(group_config$show_tabs)) {
    if (length(map_configs) != 1) {
      stop(
        paste0(
          "UI group ",
          group_key,
          " has show_tabs = FALSE but contains more than one map."
        )
      )
    }
    
    map_config <- map_configs[[1]]
    
    return(
      leaflet::leafletOutput(
        map_config$output_id,
        width = "100%",
        height = height
      )
    )
  }
  
  tab_panels <- lapply(
    map_configs,
    function(map_config) {
      shiny::tabPanel(
        title = map_config$tab_title,
        leaflet::leafletOutput(
          map_config$output_id,
          width = "100%",
          height = height
        )
      )
    }
  )
  
  do.call(
    shiny::tabsetPanel,
    c(
      list(
        id = group_config$tabset_id,
        type = group_config$tab_type
      ),
      tab_panels
    )
  )
}


model_maps_server <- function(input, output, session) {
  validate_map_configuration()
  
  node_data <- readr::read_csv(
    MODEL_MAP_FILES$node_csv,
    show_col_types = FALSE
  )
  
  station_data <- readxl::read_excel(
    MODEL_MAP_FILES$station_xlsx
  )
  
  names(node_data) <- trimws(
    names(node_data)
  )
  
  names(station_data) <- trimws(
    names(station_data)
  )
  
  names(station_data)[
    names(station_data) ==
      MODEL_MAP_COLUMNS$station_mia_original
  ] <- "MIA"
  
  validate_named_columns(
    node_data,
    MODEL_MAP_COLUMNS$required_node,
    "delta_locations_coordinates.csv"
  )
  
  validate_named_columns(
    station_data,
    MODEL_MAP_COLUMNS$required_station,
    "Station_Location.xlsx"
  )
  
  node_data$X <- suppressWarnings(
    as.numeric(node_data$X)
  )
  
  node_data$Y <- suppressWarnings(
    as.numeric(node_data$Y)
  )
  
  station_data$MIA <- trimws(
    as.character(station_data$MIA)
  )
  
  if (anyDuplicated(station_data$MIA)) {
    stop(
      "Duplicated MIA values were found in Station_Location.xlsx."
    )
  }
  
  prepared_station_sets <- lapply(
    STATION_SETS,
    function(station_config) {
      prepare_station_points(
        station_data,
        station_config
      )
    }
  )
  
  prepared_node_sets <- prepare_node_sets(
    node_data
  )
  
  for (map_name in names(MAP_DEFINITIONS)) {
    local({
      map_config <- MAP_DEFINITIONS[[map_name]]
      output_id <- map_config$output_id
      
      station_points <- prepared_station_sets[[
        map_config$station_set
      ]]
      
      node_points <- if (
        isTRUE(map_config$include_nodes)
      ) {
        prepared_node_sets[[
          map_config$node_set
        ]]
      } else {
        NULL
      }
      
      output[[output_id]] <- leaflet::renderLeaflet({
        create_model_map(
          station_points = station_points,
          node_points = node_points,
          include_nodes = map_config$include_nodes,
          node_color = map_config$node_color,
          station_color = map_config$station_color
        )
      })
      
      shiny::outputOptions(
        output,
        output_id,
        suspendWhenHidden = FALSE
      )
    })
  }
  
  
  for (group_key in names(MAP_UI_GROUPS)) {
    local({
      current_group_key <- group_key
      ui_output_id <-
        MAP_UI_GROUPS[[current_group_key]]$ui_output_id
      
      output[[ui_output_id]] <- shiny::renderUI({
        build_map_group_ui(
          current_group_key
        )
      })
    })
  }
}


ptm_maps_server <- model_maps_server