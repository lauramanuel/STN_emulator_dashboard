
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
library(gt)
library(lightgbm)
library(xgboost)
library(metR)
library(mgcv)
library (ps)


MODEL_DIR <- "STN_EMULATOR/models"
OUTPUT_DIR <- "STN_EMULATOR/Output"
SHAPE_DIR <- "STN_EMULATOR/shapefiles"
ARCHIVE_DIR <- "STN_EMULATOR/Archive_Folder"
DSM2_NODE_SHP <- file.path( SHAPE_DIR,  "i12_DSM2_Grid_V2025-05-28_Hist_nodes.shp")
DSM2_CHANNEL_SHP <- file.path(  SHAPE_DIR,  "i12_DSM2_Grid_V2025-05-28_Hist_channels_centerlines.shp")
DSM2_CHANNEL_DEF <- file.path(  "STN_EMULATOR",  "channel_std_delta_grid_NAVD_20121214.txt")
DSM2_PATH_FILE <- file.path(  "STN_EMULATOR",  "Region_Location_Node_Path.csv")

required_model_files <- c(
  file.path(MODEL_DIR, "PTM_Entrainment7d_lightgbm.txt"),
  file.path(MODEL_DIR, "PTM_Entrainment30d_lightgbm.txt"),
  file.path(MODEL_DIR, "ECOPTM_survival_lightgbm.txt"),
  file.path(MODEL_DIR, "ECOPTM_interior_lightgbm.txt"),
  file.path(MODEL_DIR, "xgb_event_horizon.json")
)

missing_model_files <- required_model_files[!file.exists(required_model_files)]

if (length(missing_model_files) > 0) {
  stop(
    paste0(
      "Required emulator model file(s) were not found: ",
      paste(missing_model_files, collapse = ", "),
      ". Open the app from the STN_ShinyApp folder."
    )
  )
}

cat("Loading PTM7 ")
ptm7_model <- lgb.load(file.path(MODEL_DIR, "PTM_Entrainment7d_lightgbm.txt"))
cat("Loading PTM30 ")
ptm30_model <- lgb.load(file.path(MODEL_DIR, "PTM_Entrainment30d_lightgbm.txt"))
cat("Loading ECO Survival ")
eco_survival_model <- lgb.load(file.path(MODEL_DIR, "ECOPTM_survival_lightgbm.txt"))
cat("Loading ECO Interior ")
eco_interior_model <- lgb.load(file.path(MODEL_DIR, "ECOPTM_interior_lightgbm.txt"))
cat("Loading Event Horizon model ")
#event_horizon_model <- xgb.load(  file.path(MODEL_DIR, "xgb_event_horizon.json"))
event_horizon_model <- NULL
cat("Finished Event Horizon model ")
cat("Finished loading models ")
get_event_horizon_model <- local({
  
  model <- NULL
  
  function() {
    
    if (is.null(model)) {
      
      cat("EH STEP 1\n")
      
      model <<- xgb.load(
        file.path(MODEL_DIR, "xgb_event_horizon.json")
      )
      
      cat("EH STEP 2\n")
      
      print(gc())
      
      cat("EH STEP 3\n")
    }
    
    model
  }
})
bound_percent <- function(x) pmax(0, pmin(100, x))

read_lgb_features <- function(model_file) {
  header <- readLines(model_file, n = 60, warn = FALSE)
  feature_line <- grep("^feature_names=", header, value = TRUE)
  if (length(feature_line) != 1) {
    stop("Could not read LightGBM feature names from: ", model_file)
  }
  strsplit(sub("^feature_names=", "", feature_line), "\\s+")[[1]]
}

fmt_int <- function(x) {
  format(
    round(x),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}
parse_dsm2_channel_definition <- function(path) {
  
  lines <- readLines(path, warn = FALSE)
  
  channel_start <- which(trimws(lines) == "CHANNEL")[1]
  
  if (is.na(channel_start)) {
    stop("Could not find CHANNEL block in: ", path)
  }
  
  header_line <- channel_start + 1
  
  end_candidates <- which(
    seq_along(lines) > header_line &
      trimws(lines) == "END"
  )
  
  if (length(end_candidates) == 0) {
    stop("Could not find END after CHANNEL block in: ", path)
  }
  
  channel_end <- end_candidates[1]
  
  channel_lines <- lines[(header_line + 1):(channel_end - 1)]
  
  channel_lines <- channel_lines[
    nzchar(trimws(channel_lines)) &
      !grepl("^\\s*#", channel_lines)
  ]
  
  channel_table <- read.table(
    text = paste(channel_lines, collapse = "\n"),
    header = FALSE,
    col.names = c(
      "CHAN_NO",
      "LENGTH_FT",
      "MANNING",
      "DISPERSION",
      "UPNODE",
      "DOWNNODE"
    ),
    stringsAsFactors = FALSE
  )
  
  channel_table |>
    dplyr::mutate(
      CHAN_NO = as.integer(CHAN_NO),
      LENGTH_FT = as.numeric(LENGTH_FT),
      UPNODE = as.integer(UPNODE),
      DOWNNODE = as.integer(DOWNNODE)
    )
}
find_first_existing_column <- function(data, candidates, label) {
  
  normalized_names <- toupper(gsub("[^A-Za-z0-9]", "", names(data)))
  normalized_candidates <- toupper(gsub("[^A-Za-z0-9]", "", candidates))
  
  match_index <- match(normalized_candidates, normalized_names)
  match_index <- match_index[!is.na(match_index)]
  
  if (length(match_index) == 0) {
    stop(
      "Could not identify ",
      label,
      " column. Available columns are: ",
      paste(names(data), collapse = ", ")
    )
  }
  
  names(data)[match_index[1]]
}
eco_survival_features <- read_lgb_features(
  file.path(MODEL_DIR, "ECOPTM_survival_lightgbm.txt")
)
eco_interior_features <- read_lgb_features(
  file.path(MODEL_DIR, "ECOPTM_interior_lightgbm.txt")
)


# ------------------------------------------------------------------
# Observed-condition trailing 7-day flow retrieval
# ------------------------------------------------------------------

cdec_daily_link <- function(station_id, end_date = Sys.Date()) {
  paste0(
    "https://cdec.water.ca.gov/dynamicapp/QueryDaily?s=",
    station_id,
    "&end=",
    format(as.Date(end_date), "%Y-%m-%d")
  )
}

observed_source_links <- list(
  CLC = cdec_daily_link("CLC"),
  TRP = cdec_daily_link("TRP"),
  VNS = cdec_daily_link("VNS"),
  FPT = cdec_daily_link("FPT"),
  NHG = cdec_daily_link("NHG"),
  MOK = "https://waterdata.usgs.gov/monitoring-location/USGS-11325500/#dataTypeId=daily-00060-0&period=P1Y&showFieldMeasurements=true",
  COS = "https://waterdata.usgs.gov/monitoring-location/USGS-11335000/#dataTypeId=daily-00060-0&period=P1Y&showFieldMeasurements=true",
  XGEO_A = "https://waterdata.usgs.gov/monitoring-location/USGS-11447890/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true",
  XGEO_C = "https://waterdata.usgs.gov/monitoring-location/USGS-11447905/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true"
)

download_observed_source_file <- function(
  url,
  fileext = ".txt",
  attempts = 3,
  pause_seconds = 1
) {
  destination <- tempfile(fileext = fileext)
  old_timeout <- getOption("timeout")
  previous_user_agent <- getOption("HTTPUserAgent")

  on.exit(
    {
      options(
        timeout = old_timeout,
        HTTPUserAgent = previous_user_agent
      )

      if (file.exists(destination)) {
        unlink(destination)
      }
    },
    add = TRUE
  )

  options(
    timeout = max(120, old_timeout),
    HTTPUserAgent = paste(
      "Mozilla/5.0",
      "(Windows NT 10.0; Win64; x64)",
      "AppleWebKit/537.36",
      "Chrome/126.0 Safari/537.36"
    )
  )

  last_error <- NULL

  for (attempt in seq_len(attempts)) {
    if (file.exists(destination)) {
      unlink(destination)
    }

    success <- tryCatch(
      {
        suppressWarnings(
          utils::download.file(
            url = url,
            destfile = destination,
            mode = "wb",
            method = "libcurl",
            quiet = TRUE
          )
        )

        file.exists(destination) &&
          isTRUE(file.info(destination)$size > 0)
      },
      error = function(error_condition) {
        last_error <<- conditionMessage(error_condition)
        FALSE
      }
    )

    if (isTRUE(success)) {
      return(readLines(destination, warn = FALSE))
    }

    if (attempt < attempts) {
      Sys.sleep(pause_seconds * attempt)
    }
  }

  stop(
    paste0(
      "The source could not be reached after ",
      attempts,
      " attempts",
      if (!is.null(last_error)) paste0(": ", last_error) else "."
    )
  )
}

parse_observed_dates <- function(values) {
  text_values <- trimws(as.character(values))

  parsed <- suppressWarnings(
    as.Date(
      substr(text_values, 1, 10),
      format = "%Y-%m-%d"
    )
  )

  missing <- is.na(parsed)

  if (any(missing)) {
    parsed[missing] <- suppressWarnings(
      as.Date(
        substr(text_values[missing], 1, 10),
        format = "%m/%d/%Y"
      )
    )
  }

  parsed
}

decode_cdec_html_text <- function(value) {
  value <- gsub("(?is)<br\\s*/?>", " ", value, perl = TRUE)
  value <- gsub("(?is)<[^>]+>", " ", value, perl = TRUE)
  value <- gsub("&nbsp;|&#160;", " ", value, ignore.case = TRUE)
  value <- gsub("&amp;", "&", value, ignore.case = TRUE)
  value <- gsub("&lt;", "<", value, ignore.case = TRUE)
  value <- gsub("&gt;", ">", value, ignore.case = TRUE)
  value <- gsub("&quot;", "\"", value, ignore.case = TRUE)
  value <- gsub("&#39;|&apos;", "'", value, ignore.case = TRUE)
  trimws(gsub("\\s+", " ", value))
}


extract_cdec_query_daily_rows <- function(html_lines) {
  html_text <- paste(html_lines, collapse = "\n")

  row_matches <- regmatches(
    html_text,
    gregexpr(
      "(?is)<tr\\b[^>]*>.*?</tr>",
      html_text,
      perl = TRUE
    )
  )[[1]]

  if (length(row_matches) == 0) {
    stop("The CDEC daily page did not contain a readable data table.")
  }

  rows <- lapply(
    row_matches,
    function(row_text) {
      cells <- regmatches(
        row_text,
        gregexpr(
          "(?is)<t[dh]\\b[^>]*>.*?</t[dh]>",
          row_text,
          perl = TRUE
        )
      )[[1]]

      if (length(cells) == 0) {
        return(character())
      }

      vapply(
        cells,
        decode_cdec_html_text,
        character(1)
      )
    }
  )

  rows[lengths(rows) > 0]
}


parse_cdec_numeric_value <- function(value) {
  cleaned <- trimws(as.character(value)[1])

  if (
    length(cleaned) == 0 ||
    is.na(cleaned) ||
    cleaned %in% c("", "---", "--", "M", "m", "BRT")
  ) {
    return(NA_real_)
  }

  cleaned <- gsub(",", "", cleaned, fixed = TRUE)

  match_position <- regexpr(
    "-?[0-9]+(?:\\.[0-9]+)?",
    cleaned,
    perl = TRUE
  )

  if (length(match_position) == 0 || match_position[1] < 0) {
    return(NA_real_)
  }

  numeric_text <- regmatches(
    cleaned,
    match_position
  )

  suppressWarnings(as.numeric(numeric_text)[1])
}


read_cdec_daily_values <- function(
  station_id,
  sensor_number,
  end_date = Sys.Date(),
  lookback_days = 30
) {
  station_id <- toupper(trimws(station_id))
  end_date <- as.Date(end_date)
  start_date <- end_date - lookback_days

  query_url <- paste0(
    "https://cdec.water.ca.gov/dynamicapp/req/JSONDataServlet?",
    "Stations=", utils::URLencode(station_id, reserved = TRUE),
    "&SensorNums=", utils::URLencode(as.character(sensor_number), reserved = TRUE),
    "&dur_code=D",
    "&Start=", format(start_date, "%Y-%m-%d"),
    "&End=", format(end_date, "%Y-%m-%d")
  )

  response_text <- paste(
    download_observed_source_file(
      query_url,
      fileext = ".json"
    ),
    collapse = "\n"
  )

  if (!nzchar(trimws(response_text))) {
    stop(
      paste0(
        "CDEC returned an empty response for ",
        station_id,
        " sensor ",
        sensor_number,
        "."
      )
    )
  }

  cdec_data <- tryCatch(
    jsonlite::fromJSON(
      response_text,
      simplifyDataFrame = TRUE
    ),
    error = function(error_condition) {
      stop(
        paste0(
          "The CDEC JSON response could not be parsed for ",
          station_id,
          " sensor ",
          sensor_number,
          ": ",
          conditionMessage(error_condition)
        )
      )
    }
  )

  if (is.null(cdec_data) || length(cdec_data) == 0) {
    stop(
      paste0(
        "CDEC returned no daily records for ",
        station_id,
        " sensor ",
        sensor_number,
        "."
      )
    )
  }

  cdec_data <- as.data.frame(
    cdec_data,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  normalized_names <- tolower(
    gsub(
      "[^a-z0-9]",
      "",
      names(cdec_data)
    )
  )

  date_candidates <- c(
    "obsdate",
    "date",
    "datetime"
  )

  value_candidates <- c(
    "value",
    "obsvalue"
  )

  date_index <- match(
    date_candidates,
    normalized_names,
    nomatch = 0
  )

  date_index <- date_index[date_index > 0][1]

  value_index <- match(
    value_candidates,
    normalized_names,
    nomatch = 0
  )

  value_index <- value_index[value_index > 0][1]

  if (
    length(date_index) == 0 ||
    is.na(date_index) ||
    length(value_index) == 0 ||
    is.na(value_index)
  ) {
    stop(
      paste0(
        "The CDEC response for ",
        station_id,
        " sensor ",
        sensor_number,
        " did not contain recognizable date and value fields. ",
        "Returned fields: ",
        paste(names(cdec_data), collapse = ", "),
        "."
      )
    )
  }

  result <- data.frame(
    Date = parse_observed_dates(
      cdec_data[[date_index]]
    ),
    Value = suppressWarnings(
      as.numeric(
        gsub(
          ",",
          "",
          as.character(cdec_data[[value_index]]),
          fixed = TRUE
        )
      )
    ),
    stringsAsFactors = FALSE
  )

  # All CDEC series used here are flows in cfs and should not be
  # negative. CDEC can return negative missing-value sentinels such as
  # -9999; exclude those before calculating the trailing average.
  result <- result[
    !is.na(result$Date) &
      is.finite(result$Value) &
      result$Value >= 0 &
      result$Date >= start_date &
      result$Date <= end_date,
    ,
    drop = FALSE
  ]

  result <- result[
    !duplicated(result$Date, fromLast = TRUE),
    ,
    drop = FALSE
  ]

  result <- result[
    order(result$Date),
    ,
    drop = FALSE
  ]

  if (nrow(result) == 0) {
    stop(
      paste0(
        "CDEC returned a response for ",
        station_id,
        " sensor ",
        sensor_number,
        ", but no nonnegative daily flow values were found between ",
        format(start_date, "%m/%d/%Y"),
        " and ",
        format(end_date, "%m/%d/%Y"),
        "."
      )
    )
  }

  attr(result, "source_url") <- query_url
  attr(result, "station_id") <- station_id
  attr(result, "sensor_number") <- sensor_number

  result
}

read_usgs_daily_values <- function(
  site_number,
  parameter_code,
  end_date = Sys.Date(),
  lookback_days = 45,
  use_latest_available = FALSE
) {
  start_date <- if (isTRUE(use_latest_available)) {
    as.Date("1900-01-01")
  } else {
    as.Date(end_date) - lookback_days
  }

  query_url <- paste0(
    "https://waterservices.usgs.gov/nwis/dv/",
    "?format=rdb",
    "&sites=", site_number,
    "&startDT=", format(start_date, "%Y-%m-%d"),
    "&endDT=", format(as.Date(end_date), "%Y-%m-%d"),
    "&parameterCd=", parameter_code,
    "&statCd=00003",
    "&siteStatus=all"
  )

  text_lines <- download_observed_source_file(query_url)

  data_lines <- text_lines[
    !grepl("^#", text_lines) &
      nzchar(trimws(text_lines))
  ]

  if (length(data_lines) < 3) {
    stop(
      paste0(
        "USGS site ",
        site_number,
        " returned no usable daily records."
      )
    )
  }

  usgs_data <- utils::read.delim(
    text = paste(data_lines, collapse = "\n"),
    header = TRUE,
    sep = "\t",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "Ice", "Eqp", "Ssn", "Rat", "Dis")
  )

  if (
    nrow(usgs_data) > 0 &&
    "agency_cd" %in% names(usgs_data) &&
    grepl(
      "^[0-9]+[a-zA-Z]$",
      as.character(usgs_data$agency_cd[1])
    )
  ) {
    usgs_data <- usgs_data[-1, , drop = FALSE]
  }

  date_index <- which(
    toupper(names(usgs_data)) %in% c("DATETIME", "DATE")
  )[1]

  parameter_columns <- which(
    grepl(
      paste0("_", parameter_code, "_"),
      names(usgs_data),
      fixed = TRUE
    ) &
      !grepl("_CD$", names(usgs_data))
  )

  if (is.na(date_index) || length(parameter_columns) == 0) {
    stop(
      paste0(
        "USGS response for site ",
        site_number,
        " did not contain the requested parameter ",
        parameter_code,
        "."
      )
    )
  }

  # Prefer the daily mean statistic column when several columns are returned.
  mean_columns <- parameter_columns[
    grepl("_00003$", names(usgs_data)[parameter_columns])
  ]

  value_index <- if (length(mean_columns) > 0) {
    mean_columns[1]
  } else {
    parameter_columns[1]
  }

  result <- data.frame(
    Date = parse_observed_dates(
      usgs_data[[date_index]]
    ),
    Value = suppressWarnings(
      as.numeric(
        gsub(",", "", usgs_data[[value_index]])
      )
    )
  )

  result <- result[
    !is.na(result$Date) &
      is.finite(result$Value),
    ,
    drop = FALSE
  ]

  result <- result[
    order(result$Date),
    ,
    drop = FALSE
  ]

  if (nrow(result) == 0) {
    stop(
      paste0(
        "No usable daily USGS values were returned for site ",
        site_number,
        "."
      )
    )
  }

  result
}

trailing_seven_summary <- function(data) {
  data <- data[
    !is.na(data$Date) &
      is.finite(data$Value) &
      data$Value > -9000,
    ,
    drop = FALSE
  ]

  data <- data[
    order(data$Date),
    ,
    drop = FALSE
  ]

  if (nrow(data) < 7) {
    stop("Fewer than seven usable daily values were available.")
  }

  selected <- utils::tail(data, 7)

  list(
    value = mean(selected$Value, na.rm = TRUE),
    start_date = min(selected$Date),
    end_date = max(selected$Date),
    dates = selected$Date,
    values = selected$Value
  )
}

safe_observed_summary <- function(
  expression,
  attempts = 2,
  pause_seconds = 1
) {
  expression_code <- substitute(expression)
  evaluation_environment <- parent.frame()
  last_error <- NULL

  for (attempt in seq_len(attempts)) {
    result <- tryCatch(
      {
        value <- eval(expression_code, envir = evaluation_environment)
        list(ok = TRUE, data = value, message = NULL)
      },
      error = function(error_condition) {
        last_error <<- conditionMessage(error_condition)
        NULL
      }
    )

    if (!is.null(result)) {
      return(result)
    }

    if (attempt < attempts) {
      Sys.sleep(pause_seconds * attempt)
    }
  }

  list(
    ok = FALSE,
    data = NULL,
    message = paste0(
      "The live source was temporarily unavailable after ",
      attempts,
      " attempts. The existing input value is being used."
    ),
    technical_message = last_error
  )
}

format_observed_value <- function(value) {
  if (!is.finite(value)) {
    return("Unavailable")
  }

  format(
    round(value, 0),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}


server <- function(input, output, session) {


  observed_flow_state <- reactiveVal(
    list(
      status = "loading",
      message = "Retrieving observed trailing 7-day average flows.",
      updated_at = NULL
    )
  )

  observed_note <- function(
    label,
    source_items,
    special_note = NULL
  ) {
    state <- observed_flow_state()

    if (!identical(state$status, "ready")) {
      return(
        tags$div(
          class = "observed-flow-note",
          if (identical(state$status, "loading")) {
            "Retrieving the latest observed trailing 7-day average."
          } else {
            paste0(
              "Live source refresh is temporarily unavailable. ",
              "The existing box value is being used and the app will retry when this input method is selected again."
            )
          }
        )
      )
    }

    item <- state[[label]]

    if (is.null(item) || !isTRUE(item$ok)) {
      return(
        tags$div(
          class = "observed-flow-note",
          paste0(
            "Live source refresh is temporarily unavailable. ",
            "The existing box value is being used. Re-select Enter a Single Set of Values to retry."
          )
        )
      )
    }

    link_tags <- lapply(
      seq_along(source_items),
      function(index) {
        source <- source_items[[index]]

        tagList(
          if (index > 1) ", " else NULL,
          tags$a(
            href = source$url,
            target = "_blank",
            rel = "noopener noreferrer",
            source$text
          )
        )
      }
    )

    tags$div(
      class = "observed-flow-note",
      tags$b(
        paste0(
          "Trailing 7-day average flow as of ",
          format(item$end_date, "%m/%d/%Y"),
          ": ",
          format_observed_value(item$value),
          " cfs."
        )
      ),
      tags$br(),
      "Source",
      if (length(source_items) > 1) "s: " else ": ",
      tagList(link_tags),
      if (!is.null(special_note)) {
        tagList(
          tags$br(),
          tags$span(special_note)
        )
      }
    )
  }

  output$current_ptm_exp_note <- renderUI({
    observed_note(
      "EXP",
      list(
        list(
          text = "CLC INFLOW CFS (sensor 76)",
          url = observed_source_links$CLC
        ),
        list(
          text = "TRP DC PUMP CFS (sensor 70)",
          url = observed_source_links$TRP
        )
      )
    )
  })

  output$current_ptm_ver_note <- renderUI({
    observed_note(
      "VER",
      list(
        list(
          text = "VNS M FLOW CFS (sensor 41)",
          url = observed_source_links$VNS
        )
      )
    )
  })

  output$current_ptm_sac_note <- renderUI({
    observed_note(
      "SAC",
      list(
        list(
          text = "FPT FLOW CFS (sensor 20)",
          url = observed_source_links$FPT
        )
      )
    )
  })

  output$current_ptm_east_note <- renderUI({
    state <- observed_flow_state()
    special_note <- NULL

    if (
      identical(state$status, "ready") &&
      !is.null(state$EAST) &&
      isTRUE(state$EAST$ok)
    ) {
      special_note <- paste0(
        "EAST = MOK + CAL + COS. The MOK daily record used here ends ",
        format(state$MOK$end_date, "%m/%d/%Y"),
        "; its latest available seven daily values are combined with the ",
        "current CAL and COS trailing averages."
      )
    }

    observed_note(
      "EAST",
      list(
        list(
          text = "MOK (USGS 11325500)",
          url = observed_source_links$MOK
        ),
        list(
          text = "CAL: NHG OUTFLOW CFS (sensor 23)",
          url = observed_source_links$NHG
        ),
        list(
          text = "COS (USGS 11335000 discharge)",
          url = observed_source_links$COS
        )
      ),
      special_note = special_note
    )
  })

  output$current_ptm_xgeo_note <- renderUI({
    observed_note(
      "XGEO",
      list(
        list(
          text = "XGEO_A (USGS 11447890)",
          url = observed_source_links$XGEO_A
        ),
        list(
          text = "XGEO_C (USGS 11447905)",
          url = observed_source_links$XGEO_C
        )
      ),
      special_note = "XGEO is calculated as XGEO_A minus XGEO_C."
    )
  })

  observeEvent(
    input$current_input_method,
    {
      if (!identical(input$current_input_method, "single")) {
        return(invisible(NULL))
      }

      observed_flow_state(
        list(
          status = "loading",
          message = "Retrieving observed trailing 7-day average flows.",
          updated_at = Sys.time()
        )
      )

      clc <- safe_observed_summary(
        trailing_seven_summary(
          read_cdec_daily_values("CLC", 76)
        )
      )

      trp <- safe_observed_summary(
        trailing_seven_summary(
          read_cdec_daily_values("TRP", 70)
        )
      )

      vns <- safe_observed_summary(
        trailing_seven_summary(
          read_cdec_daily_values("VNS", 41)
        )
      )

      fpt <- safe_observed_summary(
        trailing_seven_summary(
          read_cdec_daily_values("FPT", 20)
        )
      )

      nhg <- safe_observed_summary(
        trailing_seven_summary(
          read_cdec_daily_values("NHG", 23)
        )
      )

      mok <- safe_observed_summary(
        trailing_seven_summary(
          read_usgs_daily_values(
            site_number = "11325500",
            parameter_code = "00060",
            end_date = as.Date("2024-09-30"),
            lookback_days = 45
          )
        )
      )

      cos <- safe_observed_summary(
        trailing_seven_summary(
          read_usgs_daily_values(
            site_number = "11335000",
            parameter_code = "00060"
          )
        )
      )

      xgeo_a <- safe_observed_summary(
        trailing_seven_summary(
          read_usgs_daily_values(
            site_number = "11447890",
            parameter_code = "72137"
          )
        )
      )

      xgeo_c <- safe_observed_summary(
        trailing_seven_summary(
          read_usgs_daily_values(
            site_number = "11447905",
            parameter_code = "72137"
          )
        )
      )

      exp_result <- if (isTRUE(clc$ok) && isTRUE(trp$ok)) {
        list(
          ok = TRUE,
          value = clc$data$value + trp$data$value,
          start_date = max(clc$data$start_date, trp$data$start_date),
          end_date = min(clc$data$end_date, trp$data$end_date),
          message = NULL
        )
      } else {
        list(
          ok = FALSE,
          message = paste(
            c(
              if (!isTRUE(clc$ok)) paste0("CLC: ", clc$message) else NULL,
              if (!isTRUE(trp$ok)) paste0("TRP: ", trp$message) else NULL
            ),
            collapse = " "
          )
        )
      }

      ver_result <- if (isTRUE(vns$ok)) {
        c(list(ok = TRUE), vns$data)
      } else {
        list(ok = FALSE, message = vns$message)
      }

      sac_result <- if (isTRUE(fpt$ok)) {
        c(list(ok = TRUE), fpt$data)
      } else {
        list(ok = FALSE, message = fpt$message)
      }

      east_result <- if (
        isTRUE(mok$ok) &&
        isTRUE(nhg$ok) &&
        isTRUE(cos$ok)
      ) {
        list(
          ok = TRUE,
          value = mok$data$value + nhg$data$value + cos$data$value,
          start_date = min(
            mok$data$start_date,
            nhg$data$start_date,
            cos$data$start_date
          ),
          end_date = max(
            mok$data$end_date,
            nhg$data$end_date,
            cos$data$end_date
          ),
          message = NULL
        )
      } else {
        list(
          ok = FALSE,
          message = paste(
            c(
              if (!isTRUE(mok$ok)) paste0("MOK: ", mok$message) else NULL,
              if (!isTRUE(nhg$ok)) paste0("CAL: ", nhg$message) else NULL,
              if (!isTRUE(cos$ok)) paste0("COS: ", cos$message) else NULL
            ),
            collapse = " "
          )
        )
      }

      xgeo_result <- if (
        isTRUE(xgeo_a$ok) &&
        isTRUE(xgeo_c$ok)
      ) {
        list(
          ok = TRUE,
          value = xgeo_a$data$value - xgeo_c$data$value,
          start_date = max(
            xgeo_a$data$start_date,
            xgeo_c$data$start_date
          ),
          end_date = min(
            xgeo_a$data$end_date,
            xgeo_c$data$end_date
          ),
          message = NULL
        )
      } else {
        list(
          ok = FALSE,
          message = paste(
            c(
              if (!isTRUE(xgeo_a$ok)) {
                paste0("XGEO_A: ", xgeo_a$message)
              } else {
                NULL
              },
              if (!isTRUE(xgeo_c$ok)) {
                paste0("XGEO_C: ", xgeo_c$message)
              } else {
                NULL
              }
            ),
            collapse = " "
          )
        )
      }

      state <- list(
        status = "ready",
        updated_at = Sys.time(),
        EXP = exp_result,
        VER = ver_result,
        SAC = sac_result,
        EAST = east_result,
        XGEO = xgeo_result,
        MOK = if (isTRUE(mok$ok)) mok$data else NULL
      )

      observed_flow_state(state)

      if (isTRUE(exp_result$ok)) {
        updateNumericInput(
          session,
          "current_ptm_exp",
          value = round(exp_result$value, 0)
        )
      }

      if (isTRUE(ver_result$ok)) {
        updateNumericInput(
          session,
          "current_ptm_ver",
          value = round(ver_result$value, 0)
        )
      }

      if (isTRUE(sac_result$ok)) {
        updateNumericInput(
          session,
          "current_ptm_sac",
          value = round(sac_result$value, 0)
        )
      }

      if (isTRUE(east_result$ok)) {
        updateNumericInput(
          session,
          "current_ptm_east",
          value = round(east_result$value, 0)
        )
      }

      if (isTRUE(xgeo_result$ok)) {
        updateNumericInput(
          session,
          "current_ptm_xgeo",
          value = round(xgeo_result$value, 0)
        )
      }

      failed_labels <- c(
        if (!isTRUE(exp_result$ok)) "EXP" else NULL,
        if (!isTRUE(ver_result$ok)) "VER" else NULL,
        if (!isTRUE(sac_result$ok)) "SAC" else NULL,
        if (!isTRUE(east_result$ok)) "EAST" else NULL,
        if (!isTRUE(xgeo_result$ok)) "XGEO" else NULL
      )

      if (length(failed_labels) > 0) {
        showNotification(
          paste0(
            "Live values are temporarily unavailable for: ",
            paste(failed_labels, collapse = ", "),
            ". Existing input values are being used; re-select the input method to retry."
          ),
          type = "warning",
          duration = 10
        )
      }
    },
    ignoreInit = FALSE
  )

  
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
  
  dsm2_nodes_raw <- st_read(
    DSM2_NODE_SHP,
    quiet = TRUE
  )
  
  dsm2_channels_raw <- st_read(
    DSM2_CHANNEL_SHP,
    quiet = TRUE
  )
  
  channel_definition <- parse_dsm2_channel_definition(
    DSM2_CHANNEL_DEF
  )
  
  eh_paths <- readr::read_csv(
    DSM2_PATH_FILE,
    show_col_types = FALSE
  )
  
  node_id_col <- find_first_existing_column(
    dsm2_nodes_raw,
    c(
      "DSM2_Node",
      "DSM2NODE",
      "NODE",
      "NODE_ID",
      "NODEID",
      "ID"
    ),
    "DSM2 node ID"
  )
  
  channel_id_col <- find_first_existing_column(
    dsm2_channels_raw,
    c(
      "CHAN_NO",
      "CHANO",
      "CHANNEL",
      "CHANNEL_ID",
      "CHANNELID",
      "CHAN",
      "ID"
    ),
    "DSM2 channel ID"
  )
  
  dsm2_nodes <- dsm2_nodes_raw |>
    dplyr::mutate(
      DSM2_Node = as.integer(.data[[node_id_col]])
    ) |>
    st_transform(26910)
  
  dsm2_channels <- dsm2_channels_raw |>
    dplyr::mutate(
      CHAN_NO = as.integer(.data[[channel_id_col]])
    ) |>
    dplyr::left_join(
      channel_definition,
      by = "CHAN_NO"
    ) |>
    st_transform(26910)
  
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
  add_event_horizon_bridge_edges <- function(edges) {
    
    if (
      is.null(event_horizon_bridge_channels) ||
      nrow(event_horizon_bridge_channels) == 0 ||
      nrow(edges) == 0
    ) {
      edges$VISUAL_ONLY <- FALSE
      return(edges)
    }
    
    edges <- edges |>
      dplyr::mutate(
        VISUAL_ONLY = FALSE
      )
    
    bridge_rows <- lapply(
      seq_len(nrow(edges)),
      function(i) {
        
        edge <- edges[i, ]
        
        matching_bridge <- event_horizon_bridge_channels |>
          dplyr::filter(
            FROM_NODE == edge$FROM_NODE,
            TO_NODE == edge$TO_NODE
          )
        
        if (nrow(matching_bridge) == 0) {
          return(NULL)
        }
        
        dplyr::bind_rows(
          lapply(
            seq_len(nrow(matching_bridge)),
            function(j) {
              
              bridge <- matching_bridge[j, ]
              
              bridge_def <- channel_definition |>
                dplyr::filter(
                  CHAN_NO == bridge$BRIDGE_CHAN_NO
                )
              
              if (nrow(bridge_def) == 0) {
                warning(
                  "Bridge channel ",
                  bridge$BRIDGE_CHAN_NO,
                  " was not found in the channel definition table."
                )
                return(NULL)
              }
              
              path_order_offset <- ifelse(
                bridge$BRIDGE_POSITION == "before",
                -0.1,
                0.1
              )
              
              tibble::tibble(
                FROM_NODE = bridge_def$UPNODE[1],
                TO_NODE = bridge_def$DOWNNODE[1],
                PATH_ORDER = edge$PATH_ORDER + path_order_offset,
                CHAN_NO = bridge_def$CHAN_NO[1],
                LENGTH_FT = 0,
                UPNODE = bridge_def$UPNODE[1],
                DOWNNODE = bridge_def$DOWNNODE[1],
                CUM_START_FT = edge$CUM_START_FT,
                CUM_END_FT = edge$CUM_START_FT,
                VISUAL_ONLY = TRUE
              )
            }
          )
        )
      }
    )
    
    bridge_rows <- bridge_rows[
      !vapply(
        bridge_rows,
        is.null,
        logical(1)
      )
    ]
    
    if (length(bridge_rows) == 0) {
      return(edges)
    }
    
    dplyr::bind_rows(
      edges,
      dplyr::bind_rows(bridge_rows)
    ) |>
      dplyr::arrange(
        PATH_ORDER
      )
  }
  
  path_to_edges <- function(path_string) {
    
    nodes <- strsplit(as.character(path_string), "_", fixed = TRUE)[[1]]
    nodes <- as.integer(nodes)
    
    if (length(nodes) < 2) {
      return(
        data.frame(
          FROM_NODE = integer(),
          TO_NODE = integer(),
          PATH_ORDER = integer()
        )
      )
    }
    
    data.frame(
      FROM_NODE = nodes[-length(nodes)],
      TO_NODE = nodes[-1],
      PATH_ORDER = seq_len(length(nodes) - 1)
    )
  }
  orient_channel_geometry <- function(channel_geom, from_node, to_node, node_sf) {
    
    from_geom <- node_sf |>
      dplyr::filter(DSM2_Node == from_node) |>
      st_geometry()
    
    to_geom <- node_sf |>
      dplyr::filter(DSM2_Node == to_node) |>
      st_geometry()
    
    if (length(from_geom) == 0 || length(to_geom) == 0) {
      return(channel_geom)
    }
    
    line_cast <- st_cast(channel_geom, "LINESTRING", warn = FALSE)
    coords <- st_coordinates(line_cast)
    
    first_pt <- st_sfc(
      st_point(coords[1, c("X", "Y")]),
      crs = st_crs(line_cast)
    )
    
    last_pt <- st_sfc(
      st_point(coords[nrow(coords), c("X", "Y")]),
      crs = st_crs(line_cast)
    )
    
    from_to_first <- as.numeric(st_distance(from_geom, first_pt))
    from_to_last <- as.numeric(st_distance(from_geom, last_pt))
    
    if (from_to_last < from_to_first) {
      reversed_coords <- coords[nrow(coords):1, c("X", "Y")]
      return(
        st_sfc(
          st_linestring(as.matrix(reversed_coords)),
          crs = st_crs(line_cast)
        )
      )
    }
    
    channel_geom
  }
  event_horizon_bridge_channels <- tibble::tribble(
    ~FROM_NODE, ~TO_NODE, ~BRIDGE_CHAN_NO, ~BRIDGE_POSITION,
    121L,       123L,     144L,           "before"
  )
  make_event_horizon_path_geometry <- function(
    distance_miles,
    regions = c("OMR")
  ) {
    
    distance_ft <- as.numeric(distance_miles) * 5280
    
    validate(
      need(
        !is.na(distance_ft) && distance_ft > 0,
        "Event Horizon distance must be greater than zero."
      )
    )
    
    path_table <- eh_paths |>
      dplyr::filter(Start_Node == 72)
    
    if (!is.null(regions)) {
      path_table <- path_table |>
        dplyr::filter(Region %in% regions)
    }
    
    validate(
      need(
        nrow(path_table) > 0,
        "No matching Event Horizon paths were found."
      )
    )
    
    all_path_lines <- lapply(
      seq_len(nrow(path_table)),
      function(path_i) {
        
        one_path <- path_table[path_i, ]
        
        edges <- path_to_edges(one_path$Path)
        
        if (nrow(edges) == 0) {
          return(NULL)
        }
        
        edges <- edges |>
          dplyr::left_join(
            channel_definition |>
              dplyr::select(CHAN_NO, LENGTH_FT, UPNODE, DOWNNODE),
            by = c(
              "FROM_NODE" = "UPNODE",
              "TO_NODE" = "DOWNNODE"
            )
          )
        
        # If the path direction is opposite of the stored UPNODE/DOWNNODE,
        # try the reverse direction.
        missing_index <- is.na(edges$CHAN_NO)
        
        if (any(missing_index)) {
          
          reverse_lookup <- edges[missing_index, c("FROM_NODE", "TO_NODE", "PATH_ORDER")] |>
            dplyr::left_join(
              channel_definition |>
                dplyr::select(CHAN_NO, LENGTH_FT, UPNODE, DOWNNODE),
              by = c(
                "FROM_NODE" = "DOWNNODE",
                "TO_NODE" = "UPNODE"
              )
            )
          
          edges[missing_index, c("CHAN_NO", "LENGTH_FT")] <-
            reverse_lookup[, c("CHAN_NO", "LENGTH_FT")]
        }
        
        edges <- edges |>
          dplyr::filter(!is.na(CHAN_NO), !is.na(LENGTH_FT)) |>
          dplyr::arrange(PATH_ORDER) |>
          dplyr::mutate(
            CUM_START_FT = dplyr::lag(cumsum(LENGTH_FT), default = 0),
            CUM_END_FT = cumsum(LENGTH_FT)
          )
        
        if (nrow(edges) == 0) {
          return(NULL)
        }
        
        keep_edges <- edges |>
          dplyr::filter(CUM_START_FT < distance_ft)
        
        if (nrow(keep_edges) == 0) {
          return(NULL)
        }
        keep_edges <- add_event_horizon_bridge_edges(
          keep_edges
        )
        
        segment_geoms <- lapply(
          seq_len(nrow(keep_edges)),
          function(edge_i) {
            
            edge <- keep_edges[edge_i, ]
            
            channel <- dsm2_channels |>
              dplyr::filter(CHAN_NO == edge$CHAN_NO)
            
            if (nrow(channel) == 0) {
              return(NULL)
            }
            
            geom <- st_geometry(channel)
            
            geom <- orient_channel_geometry(
              channel_geom = geom,
              from_node = edge$FROM_NODE,
              to_node = edge$TO_NODE,
              node_sf = dsm2_nodes
            )
            
            if (
              "VISUAL_ONLY" %in% names(edge) &&
              isTRUE(edge$VISUAL_ONLY)
            ) {
              
              out_geom <- geom
              
            } else if (edge$CUM_END_FT <= distance_ft) {
              
              out_geom <- geom
              
            } else {
              
              remaining_ft <- distance_ft - edge$CUM_START_FT
              
              fraction <- max(
                0,
                min(
                  1,
                  remaining_ft / edge$LENGTH_FT
                )
              )
              
              out_geom <- lwgeom::st_linesubstring(
                st_cast(geom, "LINESTRING", warn = FALSE),
                0,
                fraction
              )
            }
            st_sf(
              Region = one_path$Region,
              Location = one_path$`Insertion Location Name`,
              Target_Node = one_path$DSM2_Node,
              Start_Node = one_path$Start_Node,
              Path_TotalLength_ft = one_path$TotalLength_ft,
              Event_Horizon_miles = distance_miles,
              Event_Horizon_ft = distance_ft,
              CHAN_NO = edge$CHAN_NO,
              FROM_NODE = edge$FROM_NODE,
              TO_NODE = edge$TO_NODE,
              PATH_ORDER = edge$PATH_ORDER,
              geometry = st_geometry(out_geom),
              crs = st_crs(dsm2_channels),
              Visual_Only = ifelse(
                "VISUAL_ONLY" %in% names(edge),
                edge$VISUAL_ONLY,
                FALSE
              )
            )
          }
        )
        
        segment_geoms <- segment_geoms[!vapply(segment_geoms, is.null, logical(1))]
        
        if (length(segment_geoms) == 0) {
          return(NULL)
        }
        
        dplyr::bind_rows(segment_geoms)
      }
    )
    
    all_path_lines <- all_path_lines[!vapply(all_path_lines, is.null, logical(1))]
    
    validate(
      need(
        length(all_path_lines) > 0,
        "No Event Horizon channel geometry could be created from the selected paths."
      )
    )
    
    dplyr::bind_rows(all_path_lines)
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
    
    raw <- predict(get_event_horizon_model(), as.matrix(model_input))
    
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
  
  # ===============================================================
  # Archive-folder workflow helpers
  # ===============================================================
  
  list_archive_dates <- function() {
    
    if (!dir.exists(ARCHIVE_DIR)) {
      return(character(0))
    }
    
    folders <- list.dirs(
      ARCHIVE_DIR,
      full.names = FALSE,
      recursive = FALSE
    )
    
    folders <- folders[grepl("^[0-9]{8}$", folders)]
    
    folders[order(folders, decreasing = TRUE)]
  }
  
  
  archive_csv_files <- function(archive_date) {
    
    req(archive_date)
    
    folder_path <- file.path(
      ARCHIVE_DIR,
      archive_date
    )
    
    files <- list.files(
      folder_path,
      pattern = "\\.csv$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    files[grepl("OMRI", basename(files), ignore.case = TRUE)]
  }
  
  
  extract_omri_scenario <- function(file_path) {
    
    file_name <- tools::file_path_sans_ext(
      basename(file_path)
    )
    
    matches <- regmatches(
      file_name,
      gregexpr(
        "-[0-9]+",
        file_name,
        perl = TRUE
      )
    )[[1]]
    
    if (length(matches) == 0) {
      return(file_name)
    }
    
    tail(matches, 1)
  }
  
  
  archive_scenario_choices <- function(archive_date) {
    
    files <- archive_csv_files(archive_date)
    
    if (length(files) == 0) {
      return(character(0))
    }
    
    scenarios <- vapply(
      files,
      extract_omri_scenario,
      character(1)
    )
    
    numeric_scenarios <- suppressWarnings(
      as.numeric(scenarios)
    )
    
    scenarios[
      order(
        is.na(numeric_scenarios),
        numeric_scenarios,
        scenarios
      )
    ]
  }
  
  
  archive_file_for_scenario <- function(
    archive_date,
    scenario
  ) {
    
    files <- archive_csv_files(archive_date)
    
    scenarios <- vapply(
      files,
      extract_omri_scenario,
      character(1)
    )
    
    matching <- files[
      scenarios == scenario
    ]
    
    validate(
      need(
        length(matching) > 0,
        paste0(
          "No archive CSV was found for OMRI scenario ",
          scenario,
          " in ",
          archive_date,
          "."
        )
      )
    )
    
    matching[1]
  }
  
  
  normalize_archive_name <- function(x) {
    
    toupper(
      gsub(
        "[^A-Za-z0-9]+",
        "",
        trimws(as.character(x))
      )
    )
  }
  
  
  find_archive_column <- function(
    data,
    aliases,
    required = TRUE
  ) {
    
    normalized_names <- normalize_archive_name(
      names(data)
    )
    
    normalized_aliases <- normalize_archive_name(
      aliases
    )
    
    match_index <- match(
      normalized_aliases,
      normalized_names
    )
    
    match_index <- match_index[
      !is.na(match_index)
    ]
    
    if (length(match_index) == 0) {
      
      if (required) {
        stop(
          paste0(
            "Archive file is missing a required column. Expected one of: ",
            paste(
              aliases,
              collapse = ", "
            )
          )
        )
      }
      
      return(NULL)
    }
    
    names(data)[
      match_index[1]
    ]
  }
  
  
  parse_archive_dates <- function(x) {
    
    if (inherits(x, "Date")) {
      return(x)
    }
    
    if (is.numeric(x)) {
      return(
        as.Date(
          x,
          origin = "1899-12-30"
        )
      )
    }
    
    x <- trimws(
      as.character(x)
    )
    
    formats <- c(
      "%m/%d/%Y",
      "%Y-%m-%d",
      "%m/%d/%y",
      "%Y%m%d"
    )
    
    result <- rep(
      as.Date(NA),
      length(x)
    )
    
    for (fmt in formats) {
      
      missing_index <- is.na(result)
      
      result[missing_index] <- as.Date(
        x[missing_index],
        format = fmt
      )
    }
    
    result
  }
  
  
  # ===============================================================
  # XGEO retrieval for archive files
  # ===============================================================
  # XGEO is calculated as:
  #   XGEO = flow at USGS 11447890 - flow at USGS 11447905
  #
  # Daily values are downloaded only when the archive CSV does not
  # already contain XGEO or XGEO_A / XGEO_C. A cache CSV is written
  # inside the selected dated archive folder so later runs do not
  # need to download the same dates again.
  
  download_usgs_daily_flow <- function(
    site_number,
    start_date,
    end_date,
    search_buffer_days = 14
  ) {

    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)

    query_start <- start_date - search_buffer_days
    query_end <- end_date + search_buffer_days

    query_url <- paste0(
      "https://waterservices.usgs.gov/nwis/iv/",
      "?format=json",
      "&sites=", site_number,
      "&startDT=", format(query_start, "%Y-%m-%d"),
      "&endDT=", format(query_end + 1, "%Y-%m-%d"),
      "&parameterCd=00060",
      "&siteStatus=all"
    )

    response_lines <- download_observed_source_file(
      url = query_url,
      fileext = ".json",
      attempts = 3,
      pause_seconds = 1
    )

    response_text <- paste(
      response_lines,
      collapse = "\n"
    )

    response_json <- tryCatch(
      jsonlite::fromJSON(
        response_text,
        simplifyVector = FALSE
      ),
      error = function(error_condition) {
        stop(
          paste0(
            "USGS JSON response for site ",
            site_number,
            " could not be parsed: ",
            conditionMessage(error_condition)
          )
        )
      }
    )

    time_series <- response_json$value$timeSeries

    if (
      is.null(time_series) ||
      length(time_series) == 0
    ) {
      stop(
        paste0(
          "USGS site ",
          site_number,
          " returned no discharge time series near the requested dates."
        )
      )
    }

    observation_rows <- list()
    row_index <- 1L

    for (series_item in time_series) {

      value_groups <- series_item$values

      if (
        is.null(value_groups) ||
        length(value_groups) == 0
      ) {
        next
      }

      for (value_group in value_groups) {

        observations <- value_group$value

        if (
          is.null(observations) ||
          length(observations) == 0
        ) {
          next
        }

        for (observation in observations) {

          observation_rows[[row_index]] <- data.frame(
            DATE_TIME = as.character(
              observation$dateTime
            ),
            FLOW = suppressWarnings(
              as.numeric(
                observation$value
              )
            ),
            stringsAsFactors = FALSE
          )

          row_index <- row_index + 1L
        }
      }
    }

    if (length(observation_rows) == 0) {
      stop(
        paste0(
          "USGS site ",
          site_number,
          " returned no usable discharge observations."
        )
      )
    }

    usgs_values <- bind_rows(
      observation_rows
    )

    parsed_date_time <- suppressWarnings(
      as.POSIXct(
        usgs_values$DATE_TIME,
        tz = "America/Los_Angeles"
      )
    )

    missing_date_time <- is.na(
      parsed_date_time
    )

    if (any(missing_date_time)) {
      parsed_date_time[missing_date_time] <- suppressWarnings(
        as.POSIXct(
          usgs_values$DATE_TIME[missing_date_time],
          format = "%Y-%m-%dT%H:%M:%S%z",
          tz = "America/Los_Angeles"
        )
      )
    }

    daily_data <- data.frame(
      DATE = as.Date(
        parsed_date_time,
        tz = "America/Los_Angeles"
      ),
      FLOW = usgs_values$FLOW
    ) %>%
      filter(
        !is.na(DATE),
        is.finite(FLOW)
      ) %>%
      group_by(
        DATE
      ) %>%
      summarise(
        FLOW = mean(
          FLOW,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      arrange(
        DATE
      )

    if (nrow(daily_data) == 0) {
      stop(
        paste0(
          "No usable daily discharge values were available from USGS site ",
          site_number,
          " within ",
          search_buffer_days,
          " days of the requested period."
        )
      )
    }

    daily_data
  }


  fill_with_nearest_available_date <- function(
    available_data,
    required_dates,
    value_columns
  ) {

    required_dates <- sort(
      unique(
        as.Date(
          required_dates
        )
      )
    )

    available_data <- available_data %>%
      mutate(
        DATE = as.Date(
          DATE
        )
      ) %>%
      filter(
        !is.na(DATE)
      ) %>%
      arrange(
        DATE
      )

    if (
      nrow(available_data) == 0 ||
      length(required_dates) == 0
    ) {
      return(
        tibble::tibble()
      )
    }

    nearest_rows <- lapply(
      required_dates,
      function(required_date) {

        usable_rows <- available_data %>%
          filter(
            if_all(
              all_of(
                value_columns
              ),
              ~ !is.na(.x)
            )
          )

        if (nrow(usable_rows) == 0) {
          return(NULL)
        }

        nearest_index <- which.min(
          abs(
            as.numeric(
              usable_rows$DATE - required_date
            )
          )
        )

        selected_row <- usable_rows[
          nearest_index,
          ,
          drop = FALSE
        ]

        selected_row$SOURCE_DATE <- selected_row$DATE
        selected_row$DATE <- required_date

        selected_row
      }
    )

    nearest_rows <- nearest_rows[
      !vapply(
        nearest_rows,
        is.null,
        logical(1)
      )
    ]

    if (length(nearest_rows) == 0) {
      return(
        tibble::tibble()
      )
    }

    bind_rows(
      nearest_rows
    )
  }


  get_archive_xgeo <- function(
    file_path,
    required_dates
  ) {

    required_dates <- sort(
      unique(
        as.Date(
          required_dates
        )
      )
    )

    required_dates <- required_dates[
      !is.na(
        required_dates
      )
    ]

    validate(
      need(
        length(required_dates) > 0,
        "No valid archive dates were available for calculating XGEO."
      )
    )

    archive_date_folder <- dirname(
      file_path
    )

    cache_file <- file.path(
      archive_date_folder,
      "XGEO_USGS_daily.csv"
    )

    cached_xgeo <- tibble::tibble(
      DATE = as.Date(character()),
      XGEO_A = numeric(),
      XGEO_C = numeric(),
      XGEO = numeric()
    )

    if (file.exists(cache_file)) {

      cached_xgeo <- tryCatch(
        {
          readr::read_csv(
            cache_file,
            show_col_types = FALSE
          ) %>%
            mutate(
              DATE = as.Date(
                DATE
              )
            ) %>%
            select(
              DATE,
              XGEO_A,
              XGEO_C,
              XGEO
            )
        },
        error = function(e) {
          tibble::tibble(
            DATE = as.Date(character()),
            XGEO_A = numeric(),
            XGEO_C = numeric(),
            XGEO = numeric()
          )
        }
      )
    }

    complete_cached_dates <- cached_xgeo$DATE[
      !is.na(cached_xgeo$XGEO_A) &
        !is.na(cached_xgeo$XGEO_C) &
        !is.na(cached_xgeo$XGEO)
    ]

    missing_dates <- setdiff(
      required_dates,
      complete_cached_dates
    )

    if (length(missing_dates) > 0) {

      download_start <- min(
        missing_dates
      )

      download_end <- max(
        missing_dates
      )

      xgeo_a_download <- tryCatch(
        {
          download_usgs_daily_flow(
            site_number = "11447890",
            start_date = download_start,
            end_date = download_end,
            search_buffer_days = 14
          ) %>%
            rename(
              XGEO_A = FLOW
            )
        },
        error = function(error_condition) {
          NULL
        }
      )

      xgeo_c_download <- tryCatch(
        {
          download_usgs_daily_flow(
            site_number = "11447905",
            start_date = download_start,
            end_date = download_end,
            search_buffer_days = 14
          ) %>%
            rename(
              XGEO_C = FLOW
            )
        },
        error = function(error_condition) {
          NULL
        }
      )

      available_xgeo <- full_join(
        if (is.null(xgeo_a_download)) {
          tibble::tibble(
            DATE = as.Date(character()),
            XGEO_A = numeric()
          )
        } else {
          xgeo_a_download
        },
        if (is.null(xgeo_c_download)) {
          tibble::tibble(
            DATE = as.Date(character()),
            XGEO_C = numeric()
          )
        } else {
          xgeo_c_download
        },
        by = "DATE"
      ) %>%
        bind_rows(
          cached_xgeo %>%
            select(
              DATE,
              XGEO_A,
              XGEO_C
            )
        ) %>%
        arrange(
          DATE
        ) %>%
        distinct(
          DATE,
          .keep_all = TRUE
        )

      filled_xgeo <- fill_with_nearest_available_date(
        available_data = available_xgeo,
        required_dates = missing_dates,
        value_columns = c(
          "XGEO_A",
          "XGEO_C"
        )
      )

      if (nrow(filled_xgeo) > 0) {

        filled_xgeo <- filled_xgeo %>%
          mutate(
            XGEO = XGEO_A - XGEO_C
          ) %>%
          select(
            DATE,
            XGEO_A,
            XGEO_C,
            XGEO
          )

        cached_xgeo <- bind_rows(
          cached_xgeo,
          filled_xgeo
        ) %>%
          arrange(
            DATE
          ) %>%
          distinct(
            DATE,
            .keep_all = TRUE
          )
      }
    }

    result <- cached_xgeo %>%
      filter(
        DATE %in% required_dates
      ) %>%
      arrange(
        DATE
      )

    missing_after_download <- setdiff(
      required_dates,
      result$DATE[
        !is.na(
          result$XGEO
        )
      ]
    )

    if (length(missing_after_download) > 0) {

      nearest_cached <- fill_with_nearest_available_date(
        available_data = cached_xgeo %>%
          filter(
            !is.na(XGEO_A),
            !is.na(XGEO_C),
            !is.na(XGEO)
          ),
        required_dates = missing_after_download,
        value_columns = c(
          "XGEO_A",
          "XGEO_C",
          "XGEO"
        )
      )

      if (nrow(nearest_cached) > 0) {

        result <- bind_rows(
          result,
          nearest_cached %>%
            select(
              DATE,
              XGEO_A,
              XGEO_C,
              XGEO
            )
        ) %>%
          arrange(
            DATE
          ) %>%
          distinct(
            DATE,
            .keep_all = TRUE
          )
      }
    }

    still_missing <- setdiff(
      required_dates,
      result$DATE[
        !is.na(
          result$XGEO
        )
      ]
    )

    if (length(still_missing) > 0) {

      # Final non-stopping safeguard. This is used only when neither the
      # USGS service nor the local cache contains a nearby complete record.
      fallback_rows <- tibble::tibble(
        DATE = as.Date(
          still_missing
        ),
        XGEO_A = 0,
        XGEO_C = 0,
        XGEO = 0
      )

      result <- bind_rows(
        result,
        fallback_rows
      ) %>%
        arrange(
          DATE
        ) %>%
        distinct(
          DATE,
          .keep_all = TRUE
        )

      warning(
        paste0(
          "No nearby USGS XGEO records were available for ",
          paste(
            format(
              still_missing
            ),
            collapse = ", "
          ),
          ". XGEO was set to 0 so the emulator could continue."
        ),
        call. = FALSE
      )
    }

    cached_xgeo <- bind_rows(
      cached_xgeo,
      result
    ) %>%
      arrange(
        DATE
      ) %>%
      distinct(
        DATE,
        .keep_all = TRUE
      )

    try(
      readr::write_csv(
        cached_xgeo,
        cache_file
      ),
      silent = TRUE
    )

    result %>%
      filter(
        DATE %in% required_dates
      ) %>%
      arrange(
        DATE
      )
  }

  
  
  read_archive_csv <- function(file_path) {
    
    raw <- readr::read_csv(
      file_path,
      col_names = FALSE,
      show_col_types = FALSE,
      progress = FALSE,
      na = c(
        "",
        "NA",
        "N/A",
        "#N/A",
        "-"
      )
    )
    
    header_scores <- apply(
      raw,
      1,
      function(row_values) {
        
        normalized <- normalize_archive_name(
          as.character(row_values)
        )
        
        sum(
          c(
            "DATE",
            "CCF",
            "TPP",
            "VNS",
            "FPT"
          ) %in% normalized
        )
      }
    )
    
    header_row <- which.max(
      header_scores
    )
    
    if (
      length(header_row) == 0 ||
      header_scores[header_row] < 3
    ) {
      stop(
        paste0(
          "Could not identify the data-header row in ",
          basename(file_path),
          "."
        )
      )
    }
    
    headers <- as.character(
      unlist(
        raw[
          header_row,
          ,
          drop = TRUE
        ]
      )
    )
    
    headers[
      is.na(headers) |
        trimws(headers) == ""
    ] <- paste0(
      "UNNAMED_",
      which(
        is.na(headers) |
          trimws(headers) == ""
      )
    )
    
    archive_data <- raw[
      seq.int(
        header_row + 1,
        nrow(raw)
      ),
      ,
      drop = FALSE
    ]
    
    names(archive_data) <- make.unique(
      headers
    )
    
    archive_data <- archive_data %>%
      filter(
        if_any(
          everything(),
          ~ !is.na(.x)
        )
      )
    
    date_col <- find_archive_column(
      archive_data,
      c(
        "DATE",
        "Date"
      )
    )
    
    type_col <- find_archive_column(
      archive_data,
      c(
        "data type",
        "data.type",
        "data_type",
        "datatype",
        "measured forecast",
        "measured/forecast",
        "record type",
        "type"
      ),
      required = FALSE
    )
    
    # In these archive files, the words "Measured" and "Forecast"
    # may be stored in a column whose header cell is blank because the
    # label "data type" appears on the metadata row above the true
    # table header. When the header lookup fails, identify that column
    # directly from its values.
    if (is.null(type_col)) {
      
      type_scores <- vapply(
        archive_data,
        function(column_values) {
          
          normalized_values <- normalize_archive_name(column_values)
          
          valid_values <- normalized_values %in% c(
            "MEASURED",
            "MEASURE",
            "FORECAST",
            "FORECASTED"
          )
          
          if (sum(valid_values, na.rm = TRUE) < 3) {
            return(0)
          }
          
          mean(valid_values, na.rm = TRUE)
        },
        numeric(1)
      )
      
      best_type_column <- which.max(type_scores)
      
      if (
        length(best_type_column) == 1 &&
        type_scores[best_type_column] >= 0.5
      ) {
        type_col <- names(archive_data)[best_type_column]
      }
    }
    
    ccf_col <- find_archive_column(
      archive_data,
      c(
        "CCF",
        "CLC"
      )
    )
    
    tpp_col <- find_archive_column(
      archive_data,
      c(
        "TPP",
        "TRP"
      )
    )
    
    ver_col <- find_archive_column(
      archive_data,
      c(
        "VNS",
        "VER",
        "Vernalis"
      )
    )
    
    sac_col <- find_archive_column(
      archive_data,
      c(
        "FPT",
        "SAC",
        "Freeport"
      )
    )
    
    moke_col <- find_archive_column(
      archive_data,
      c(
        "MOKE",
        "MOK"
      )
    )
    
    cal_col <- find_archive_column(
      archive_data,
      c(
        "CAL",
        "Calaveras"
      )
    )
    
    cos_col <- find_archive_column(
      archive_data,
      c(
        "COS",
        "Cosumnes"
      )
    )
    
    sacweir_col <- find_archive_column(
      archive_data,
      c(
        "SACWEIR",
        "SAC WEIR",
        "Sacramento Weir"
      ),
      required = FALSE
    )
    
    freweir_col <- find_archive_column(
      archive_data,
      c(
        "FREWEIR",
        "FRE WEIR",
        "Fremont Weir"
      ),
      required = FALSE
    )
    
    dcc_col <- find_archive_column(
      archive_data,
      c(
        "DCC",
        "Delta Cross Channel"
      ),
      required = FALSE
    )
    
    xgeo_col <- find_archive_column(
      archive_data,
      c(
        "XGEO",
        "X GEO"
      ),
      required = FALSE
    )
    
    xgeo_a_col <- find_archive_column(
      archive_data,
      c(
        "XGEO_A",
        "XGEOA",
        "XGEO A"
      ),
      required = FALSE
    )
    
    xgeo_c_col <- find_archive_column(
      archive_data,
      c(
        "XGEO_C",
        "XGEOC",
        "XGEO C"
      ),
      required = FALSE
    )
    
    to_numeric <- function(column_name) {
      
      if (is.null(column_name)) {
        return(
          rep(
            NA_real_,
            nrow(archive_data)
          )
        )
      }
      
      readr::parse_number(
        as.character(
          archive_data[[column_name]]
        )
      )
    }
    
    xgeo_values <- if (!is.null(xgeo_col)) {
      
      to_numeric(
        xgeo_col
      )
      
    } else if (
      !is.null(xgeo_a_col) &&
      !is.null(xgeo_c_col)
    ) {
      
      to_numeric(
        xgeo_a_col
      ) -
        to_numeric(
          xgeo_c_col
        )
      
    } else {
      
      # The archive CSV does not contain XGEO. It will be downloaded
      # from the two USGS Georgiana Slough gauges after DATE and
      # DATA_TYPE have been parsed.
      rep(
        NA_real_,
        nrow(archive_data)
      )
    }
    
    parsed_dates <- parse_archive_dates(
      archive_data[[date_col]]
    )
    
    # Some archive CSVs do not carry a usable `data type` header even
    # though the file includes a Forecast start date above the data table.
    # When the data-type column is absent, infer Measured versus Forecast
    # from that metadata date.
    forecast_start_date <- as.Date(NA)
    
    if (is.null(type_col) && header_row > 1) {
      
      metadata <- raw[
        seq_len(header_row - 1),
        ,
        drop = FALSE
      ]
      
      metadata_matrix <- as.matrix(metadata)
      metadata_text <- as.character(metadata_matrix)
      metadata_norm <- normalize_archive_name(metadata_text)
      
      label_positions <- which(
        grepl(
          "FORECASTSTARTDATE",
          metadata_norm,
          fixed = TRUE
        )
      )
      
      if (length(label_positions) > 0) {
        
        label_pos <- label_positions[1]
        candidate_positions <- seq.int(
          label_pos + 1,
          min(length(metadata_text), label_pos + ncol(metadata_matrix) * 3)
        )
        
        candidate_dates <- parse_archive_dates(
          metadata_text[candidate_positions]
        )
        
        candidate_dates <- candidate_dates[
          !is.na(candidate_dates)
        ]
        
        if (length(candidate_dates) > 0) {
          forecast_start_date <- candidate_dates[1]
        }
      }
    }
    
    data_type_values <- if (!is.null(type_col)) {
      
      trimws(
        as.character(
          archive_data[[type_col]]
        )
      )
      
    } else if (!is.na(forecast_start_date)) {
      
      ifelse(
        parsed_dates >= forecast_start_date,
        "Forecast",
        "Measured"
      )
      
    } else {
      
      # Final fallback based on the archive request convention shown in
      # these files: 31 historical days followed by 14 forecast days.
      row_count <- length(parsed_dates)
      forecast_count <- min(14, row_count)
      
      c(
        rep("Measured", row_count - forecast_count),
        rep("Forecast", forecast_count)
      )
    }
    
    result <- tibble::tibble(
      DATE = parsed_dates,
      
      DATA_TYPE = data_type_values,
      
      # CCF and TPP are stored as acre-feet per day in the archive
      # files. Convert AF/day to cubic feet per second.
      CCF = to_numeric(
        ccf_col
      ) * 43560 / 86400,
      
      TPP = to_numeric(
        tpp_col
      ) * 43560 / 86400,
      
      VER = to_numeric(
        ver_col
      ),
      
      SAC = to_numeric(
        sac_col
      ),
      
      MOKE = to_numeric(
        moke_col
      ),
      
      CAL = to_numeric(
        cal_col
      ),
      
      COS = to_numeric(
        cos_col
      ),
      
      SACWEIR = if (is.null(sacweir_col)) {
        0
      } else {
        to_numeric(sacweir_col)
      },
      
      FREWEIR = if (is.null(freweir_col)) {
        0
      } else {
        to_numeric(freweir_col)
      },
      
      DCC = if (is.null(dcc_col)) {
        0
      } else {
        to_numeric(dcc_col)
      },
      
      XGEO = xgeo_values
    ) %>%
      mutate(
        DATA_TYPE = case_when(
          grepl(
            "MEAS",
            DATA_TYPE,
            ignore.case = TRUE
          ) ~ "Measured",
          
          grepl(
            "FORE",
            DATA_TYPE,
            ignore.case = TRUE
          ) ~ "Forecast",
          
          TRUE ~ DATA_TYPE
        ),
        
        EXP = CCF + TPP,
        EAST = MOKE + CAL + COS,
        YOL = SACWEIR + FREWEIR
      ) %>%
      filter(
        !is.na(DATE),
        DATA_TYPE %in% c(
          "Measured",
          "Forecast"
        )
      ) %>%
      arrange(DATE)
    
    # If XGEO was not present in the archive CSV, download daily
    # values for all measured dates and calculate XGEO = A - C.
    if (
      all(
        is.na(
          result$XGEO
        )
      )
    ) {
      
      measured_dates <- result %>%
        filter(
          DATA_TYPE == "Measured"
        ) %>%
        pull(
          DATE
        )
      
      downloaded_xgeo <- get_archive_xgeo(
        file_path = file_path,
        required_dates = measured_dates
      )
      
      result <- result %>%
        left_join(
          downloaded_xgeo %>%
            select(
              DATE,
              XGEO_DOWNLOADED = XGEO
            ),
          by = "DATE"
        ) %>%
        mutate(
          XGEO = ifelse(
            DATA_TYPE == "Measured",
            XGEO_DOWNLOADED,
            XGEO
          )
        ) %>%
        select(
          -XGEO_DOWNLOADED
        )
    }
    
    latest_measured_xgeo <- result %>%
      filter(
        DATA_TYPE == "Measured",
        !is.na(XGEO)
      ) %>%
      arrange(DATE) %>%
      slice_tail(n = 1) %>%
      pull(XGEO)
    
    if (
      length(latest_measured_xgeo) == 1 &&
      !is.na(latest_measured_xgeo)
    ) {
      
      result <- result %>%
        mutate(
          XGEO = ifelse(
            DATA_TYPE == "Forecast",
            latest_measured_xgeo,
            XGEO
          )
        )
    }
    
    validate(
      need(
        any(
          result$DATA_TYPE == "Measured"
        ),
        "The archive CSV contains no measured rows."
      ),
      
      need(
        any(
          !is.na(
            result$XGEO
          )
        ),
        paste(
          "No XGEO values were found or downloaded.",
          "Check the internet connection or the XGEO_USGS_daily.csv cache."
        )
      )
    )
    
    validate(
      need(
        nrow(result) > 0,
        paste0(
          "No measured or forecast records were parsed from ",
          basename(file_path),
          "."
        )
      ),
      
      need(
        all(
          c("EXP", "VER", "SAC", "EAST", "XGEO") %in% names(result)
        ),
        "The archive data could not be converted to the emulator input naming convention."
      )
    )
    
    result
  }
  
  
  mean_archive_inputs <- function(data) {
    
    data %>%
      summarise(
        EXP = mean(
          EXP,
          na.rm = TRUE
        ),
        VER = mean(
          VER,
          na.rm = TRUE
        ),
        SAC = mean(
          SAC,
          na.rm = TRUE
        ),
        EAST = mean(
          EAST,
          na.rm = TRUE
        ),
        XGEO = mean(
          XGEO,
          na.rm = TRUE
        ),
        YOL = mean(
          YOL,
          na.rm = TRUE
        ),
        MOKE = mean(
          MOKE,
          na.rm = TRUE
        ),
        DCC = fmt_int(
          mean(
            DCC,
            na.rm = TRUE
          )
        )
      )
  }
  
  
  rolling_measured_7day_inputs <- function(data) {
    
    measured <- data %>%
      filter(
        DATA_TYPE == "Measured"
      ) %>%
      arrange(DATE)
    
    validate(
      need(
        nrow(measured) >= 13,
        paste(
          "At least 13 measured rows are required to create",
          "seven rolling 7-day windows."
        )
      )
    )
    
    end_dates <- tail(
      measured$DATE,
      7
    )
    
    bind_rows(
      lapply(
        end_dates,
        function(end_date) {
          
          window_data <- measured %>%
            filter(
              DATE <= end_date
            ) %>%
            slice_tail(
              n = 7
            )
          
          mean_archive_inputs(
            window_data
          ) %>%
            mutate(
              Window_Start_Date = min(
                window_data$DATE
              ),
              Window_End_Date = max(
                window_data$DATE
              ),
              Window_Days = nrow(
                window_data
              )
            )
        }
      )
    )
  }
  
  
  measured_30day_inputs <- function(data) {
    
    measured <- data %>%
      filter(
        DATA_TYPE == "Measured"
      ) %>%
      arrange(DATE) %>%
      slice_tail(
        n = 30
      )
    
    validate(
      need(
        nrow(measured) >= 30,
        "At least 30 measured rows are required."
      )
    )
    
    latest_dcc <- measured %>%
      filter(
        !is.na(DCC)
      ) %>%
      slice_tail(
        n = 1
      ) %>%
      pull(
        DCC
      )

    if (length(latest_dcc) == 0) {
      latest_dcc <- 0
    }

    mean_archive_inputs(
      measured
    ) %>%
      mutate(
        # DCC is a daily status: 0 = closed and 1 = open.
        # Use the status recorded on the latest measured date.
        DCC = as.numeric(
          latest_dcc[1]
        ),
        Window_Start_Date = min(
          measured$DATE
        ),
        Window_End_Date = max(
          measured$DATE
        ),
        Window_Days = nrow(
          measured
        )
      )
  }
  
  
  forecast_7day_inputs <- function(data) {
    
    forecast <- data %>%
      filter(
        DATA_TYPE == "Forecast"
      ) %>%
      arrange(DATE) %>%
      slice_head(
        n = 7
      )
    
    validate(
      need(
        nrow(forecast) >= 7,
        "At least seven forecast rows are required."
      )
    )
    
    mean_archive_inputs(
      forecast
    ) %>%
      mutate(
        Window_Start_Date = min(
          forecast$DATE
        ),
        Window_End_Date = max(
          forecast$DATE
        ),
        Window_Days = nrow(
          forecast
        )
      )
  }
  
  
  predict_ptm_from_windows <- function(
    windows,
    model,
    model_name,
    nodes,
    condition,
    scenario_name,
    archive_date,
    omri_scenario
  ) {
    
    ref <- reference_data()
    
    bind_rows(
      lapply(
        seq_len(
          nrow(windows)
        ),
        function(i) {
          
          window <- windows[
            i,
            ,
            drop = FALSE
          ]
          
          model_input <- data.frame(
            DSM2_Node = as.numeric(
              nodes
            ),
            EXP = window$EXP,
            VER = window$VER,
            SAC = window$SAC,
            EAST = window$EAST,
            XGEO = window$XGEO
          )
          
          raw <- predict(
            model,
            as.matrix(
              model_input
            )
          )
          
          data.frame(
            Condition = condition,
            Input_Method = "folder",
            Archive_Date = archive_date,
            OMRI_Scenario = omri_scenario,
            Scenario_Name = scenario_name,
            Model = model_name,
            Window_Start_Date = window$Window_Start_Date,
            Window_End_Date = window$Window_End_Date,
            Window_Days = window$Window_Days,
            DSM2_Node = as.character(
              nodes
            ),
            Prediction_Raw = raw,
            Prediction_Final = bound_percent(
              raw
            ),
            Output_Unit = "Percent",
            EXP = window$EXP,
            VER = window$VER,
            SAC = window$SAC,
            EAST = window$EAST,
            XGEO = window$XGEO,
            stringsAsFactors = FALSE
          ) %>%
            left_join(
              ref$node_meta,
              by = "DSM2_Node"
            )
        }
      )
    )
  }
  
  
  make_archive_ptm_current <- function(
    archive_date,
    scenario_name
  ) {
    
    source_file <- archive_csv_files(
      archive_date
    )[1]
    
    validate(
      need(
        length(source_file) > 0,
        paste0(
          "No OMRI CSV files were found in ",
          archive_date,
          "."
        )
      )
    )
    
    archive_data <- read_archive_csv(
      source_file
    )
    
    ref <- reference_data()
    
    rolling_7 <- rolling_measured_7day_inputs(
      archive_data
    )
    
    measured_30 <- measured_30day_inputs(
      archive_data
    )
    
    bind_rows(
      predict_ptm_from_windows(
        rolling_7,
        ptm7_model,
        "PTM 7-Day Entrainment",
        ref$nodes_7d,
        "Observed Conditions",
        scenario_name,
        archive_date,
        "Measured"
      ),
      
      predict_ptm_from_windows(
        measured_30,
        ptm30_model,
        "PTM 30-Day Entrainment",
        ref$nodes_30d,
        "Observed Conditions",
        scenario_name,
        archive_date,
        "Measured"
      )
    )
  }
  
  
  make_archive_ptm_forecast <- function(
    archive_date,
    omri_scenario,
    scenario_name
  ) {
    
    source_file <- archive_file_for_scenario(
      archive_date,
      omri_scenario
    )
    
    archive_data <- read_archive_csv(
      source_file
    )
    
    forecast_7 <- forecast_7day_inputs(
      archive_data
    )
    
    ref <- reference_data()
    
    predict_ptm_from_windows(
      forecast_7,
      ptm7_model,
      "PTM 7-Day Entrainment",
      ref$nodes_7d,
      "Forecast Conditions",
      scenario_name,
      archive_date,
      omri_scenario
    )
  }
  
  
  make_archive_eco_current <- function(
    archive_date,
    scenario_name
  ) {
    
    source_file <- archive_csv_files(
      archive_date
    )[1]
    
    archive_data <- read_archive_csv(
      source_file
    )
    
    inputs <- measured_30day_inputs(
      archive_data
    )
    
    result <- make_eco_results(
      condition = "Observed Conditions",
      scenario_name = scenario_name,
      sac = inputs$SAC,
      yol = inputs$YOL,
      moke = inputs$MOKE,
      dcc = inputs$DCC
    )
    
    result %>%
      mutate(
        Input_Method = "folder",
        Archive_Date = archive_date,
        OMRI_Scenario = "Measured",
        Window_Start_Date = inputs$Window_Start_Date,
        Window_End_Date = inputs$Window_End_Date,
        Window_Days = inputs$Window_Days
      )
  }
  
  
  predict_eh_from_windows <- function(
    windows,
    condition,
    scenario_name,
    archive_date,
    omri_scenario,
    risk
  ) {
    
    bind_rows(
      lapply(
        seq_len(
          nrow(windows)
        ),
        function(i) {
          
          window <- windows[
            i,
            ,
            drop = FALSE
          ]
          
          result <- make_eh_result(
            condition = condition,
            scenario_name = scenario_name,
            exp = window$EXP,
            ver = window$VER,
            east = window$EAST,
            xgeo = window$XGEO,
            risk = risk
          )
          
          result %>%
            mutate(
              Input_Method = "folder",
              Archive_Date = archive_date,
              OMRI_Scenario = omri_scenario,
              Window_Start_Date = window$Window_Start_Date,
              Window_End_Date = window$Window_End_Date,
              Window_Days = window$Window_Days
            )
        }
      )
    )
  }
  
  
  make_archive_eh_current <- function(
    archive_date,
    scenario_name,
    risk
  ) {
    
    source_file <- archive_csv_files(
      archive_date
    )[1]
    
    archive_data <- read_archive_csv(
      source_file
    )
    
    rolling_7 <- rolling_measured_7day_inputs(
      archive_data
    )
    
    predict_eh_from_windows(
      rolling_7,
      "Observed Conditions",
      scenario_name,
      archive_date,
      "Measured",
      risk
    )
  }
  
  
  make_archive_eh_forecast <- function(
    archive_date,
    omri_scenario,
    scenario_name,
    risk
  ) {
    
    source_file <- archive_file_for_scenario(
      archive_date,
      omri_scenario
    )
    
    archive_data <- read_archive_csv(
      source_file
    )
    
    forecast_7 <- forecast_7day_inputs(
      archive_data
    )
    
    predict_eh_from_windows(
      forecast_7,
      "Forecast Conditions",
      scenario_name,
      archive_date,
      omri_scenario,
      risk
    )
  }
  
  
  latest_result_window <- function(
    data,
    model_name
  ) {
    
    filtered <- data %>%
      filter(
        Model == model_name
      )
    
    if (
      "Window_End_Date" %in% names(filtered) &&
      any(
        !is.na(
          filtered$Window_End_Date
        )
      )
    ) {
      
      latest_date <- max(
        filtered$Window_End_Date,
        na.rm = TRUE
      )
      
      filtered <- filtered %>%
        filter(
          Window_End_Date == latest_date
        )
    }
    
    filtered
  }
  
  
  selected_result_window <- function(
    data,
    model_name,
    selected_date = NULL
  ) {
    
    filtered <- data %>%
      filter(
        Model == model_name
      )
    
    if (
      !is.null(selected_date) &&
      "Window_End_Date" %in% names(filtered) &&
      any(!is.na(filtered$Window_End_Date))
    ) {
      
      selected_date <- as.Date(
        selected_date,
        origin = "1970-01-01"
      )
      
      filtered <- filtered %>%
        mutate(
          Window_End_Date = as.Date(
            Window_End_Date,
            origin = "1970-01-01"
          )
        ) %>%
        filter(
          Window_End_Date == selected_date
        )
    }
    
    if (nrow(filtered) == 0) {
      return(
        latest_result_window(
          data,
          model_name
        )
      )
    }
    
    filtered
  }
  
  
  make_ptm_timeseries <- function(
    data,
    selected_nodes,
    title
  ) {
    
    required_columns <- c(
      "Model",
      "DSM2_Node",
      "Prediction_Final",
      "Window_End_Date"
    )
    
    if (
      is.null(data) ||
      nrow(data) == 0 ||
      !all(required_columns %in% names(data))
    ) {
      return(
        plotly::plot_ly() %>%
          layout(
            annotations = list(
              list(
                text = paste(
                  "The rolling time series is available only for",
                  "Read from Archive Folder current-condition runs."
                ),
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                font = list(size = 15)
              )
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          ) %>%
          config(displayModeBar = FALSE)
      )
    }
    
    plot_data <- data %>%
      filter(
        Model == "PTM 7-Day Entrainment",
        !is.na(Window_End_Date)
      )
    
    if (
      !is.null(selected_nodes) &&
      length(selected_nodes) > 0
    ) {
      plot_data <- plot_data %>%
        filter(
          DSM2_Node %in% selected_nodes
        )
    }
    
    if (nrow(plot_data) == 0) {
      return(
        plotly::plot_ly() %>%
          layout(
            annotations = list(
              list(
                text = "No rolling PTM results are available for the selected nodes.",
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                font = list(size = 15)
              )
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          ) %>%
          config(displayModeBar = FALSE)
      )
    }
    
    if (!"Location" %in% names(plot_data)) {
      plot_data$Location <- NA_character_
    }
    
    plot_data <- plot_data %>%
      mutate(
        Window_End_Date = as.Date(
          Window_End_Date,
          origin = "1970-01-01"
        ),
        
        Node_Label = ifelse(
          is.na(Location) | Location == "",
          DSM2_Node,
          paste0(
            DSM2_Node,
            " - ",
            Location
          )
        ),
        
        Hover_Text = paste0(
          "<b>Window end:</b> ",
          format(
            Window_End_Date,
            "%b %d, %Y"
          ),
          "<br><b>Node:</b> ",
          Node_Label,
          "<br><b>Prediction:</b> ",
          sprintf(
            "%.0f%%",
            Prediction_Final
          )
        )
      )
    
    plot_ly(
      plot_data,
      x = ~Window_End_Date,
      y = ~Prediction_Final,
      color = ~Node_Label,
      type = "scatter",
      mode = "lines+markers",
      text = ~Hover_Text,
      hoverinfo = "text"
    ) %>%
      layout(
        title = list(
          text = paste0(
            "<b>",
            title,
            "</b>"
          ),
          x = 0.02,
          xanchor = "left"
        ),
        
        xaxis = list(
          title = "<b>Rolling 7-Day Window End Date</b>",
          tickformat = "%b %d",
          automargin = TRUE
        ),
        
        yaxis = list(
          title = "<b>Predicted Entrainment (%)</b>",
          range = c(
            0,
            105
          )
        ),
        
        legend = list(
          orientation = "v"
        ),
        
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        
        font = list(
          family = "Arial, Segoe UI, sans-serif",
          size = 13,
          color = "#1F1F1F"
        )
      ) %>%
      config(
        displaylogo = FALSE,
        toImageButtonOptions = list(
          format = "png",
          filename = "PTM_rolling_7day_timeseries",
          width = 1600,
          height = 900,
          scale = 2
        )
      )
  }
  
  
  make_eh_timeseries <- function(
    data,
    title
  ) {
    
    required_columns <- c(
      "Prediction_Final",
      "Window_End_Date",
      "Risk_Level_Percent"
    )
    
    if (
      is.null(data) ||
      nrow(data) == 0 ||
      !all(required_columns %in% names(data))
    ) {
      return(
        plotly::plot_ly() %>%
          layout(
            annotations = list(
              list(
                text = paste(
                  "The rolling Event Horizon time series is available only",
                  "for Read from Archive Folder current-condition runs."
                ),
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                font = list(size = 15)
              )
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          ) %>%
          config(displayModeBar = FALSE)
      )
    }
    
    plot_data <- data %>%
      filter(
        !is.na(Window_End_Date)
      ) %>%
      mutate(
        Window_End_Date = as.Date(
          Window_End_Date,
          origin = "1970-01-01"
        )
      ) %>%
      arrange(
        Window_End_Date
      )
    
    if (nrow(plot_data) == 0) {
      return(
        plotly::plot_ly() %>%
          layout(
            annotations = list(
              list(
                text = "No rolling Event Horizon results are available.",
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE
              )
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          ) %>%
          config(displayModeBar = FALSE)
      )
    }
    
    plot_data <- plot_data %>%
      mutate(
        Hover_Text = paste0(
          "<b>Window end:</b> ",
          format(
            Window_End_Date,
            "%b %d, %Y"
          ),
          "<br><b>Risk level:</b> ",
          Risk_Level_Percent,
          "%",
          "<br><b>Event Horizon:</b> ",
          sprintf(
            "%.0f river miles",
            Prediction_Final
          )
        )
      )
    
    plot_ly(
      plot_data,
      x = ~Window_End_Date,
      y = ~Prediction_Final,
      type = "scatter",
      mode = "lines+markers",
      line = list(
        color = "#B2182B",
        width = 3
      ),
      marker = list(
        color = "#B2182B",
        size = 9
      ),
      text = ~Hover_Text,
      hoverinfo = "text"
    ) %>%
      layout(
        title = list(
          text = paste0(
            "<b>",
            title,
            "</b>"
          ),
          x = 0.02,
          xanchor = "left"
        ),
        
        xaxis = list(
          title = "<b>Rolling 7-Day Window End Date</b>",
          tickformat = "%b %d"
        ),
        
        yaxis = list(
          title = "<b>Predicted Event Horizon (River Miles)</b>"
        ),
        
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        
        font = list(
          family = "Arial, Segoe UI, sans-serif",
          size = 14,
          color = "#1F1F1F"
        )
      ) %>%
      config(
        displaylogo = FALSE,
        toImageButtonOptions = list(
          format = "png",
          filename = "Event_Horizon_rolling_7day_timeseries",
          width = 1500,
          height = 850,
          scale = 2
        )
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
          paste0(DSM2_Node, " - ", Location)
        ),
        value_label = sprintf("%.0f%%", Prediction_Final),
        hover_text = paste0(
          "<b>DSM2 Node:</b> ", DSM2_Node,
          "<br><b>Location:</b> ", Location,
          "<br><b>Region:</b> ", Region,
          "<br><b>Predicted entrainment:</b> ",
          sprintf("%.0f%%", Prediction_Final)
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
        margin = list(
          l = 300,
          r = 95,
          t = 100,
          b = 35
        )
#        annotations = list(
#          list(
 #          text = "Note: Predictions are emulator outputs rounded to two decimal places.",
  #          x = 0,
#            y = -0.12,
 #           xref = "paper",
#            yref = "paper",
#            showarrow = FALSE,
 #           xanchor = "left",
#            font = list(size = 12, color = "#404040")
 #         )
 #       )
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
    
    validate(
      need(
        nrow(nodes) >= 3,
        "Not enough nodes to create risk zones."
      )
    )
    
    #-----------------------------------------
    # High-risk nodes
    #-----------------------------------------
    
    high_nodes <- nodes %>%
      filter(
        entrainment >= threshold
      )
    
    validate(
      need(
        nrow(high_nodes) >= 3,
        paste(
          "Not enough nodes exceed the",
          threshold,
          "% threshold."
        )
      )
    )
    
    #-----------------------------------------
    # Very-low-risk nodes
    #
    # These create "holes" in the high-risk
    # polygon but only if entrainment is
    # truly low.
    #-----------------------------------------
    
    low_nodes <- nodes %>%
      filter(
        entrainment < max(
          10,
          threshold * 0.25
        )
      )
    
    #-----------------------------------------
    # Build high-risk envelope
    #-----------------------------------------
    
    high_zone <- high_nodes %>%
      st_buffer(6000) %>%
      st_union() %>%
      st_convex_hull()
    
    #-----------------------------------------
    # Build low-risk exclusion areas
    #-----------------------------------------
    
    if (nrow(low_nodes) >= 3) {
      
      low_zone <- low_nodes %>%
        st_union() %>%
        st_convex_hull() %>%
        st_buffer(4000)
      
      high_zone <- st_difference(
        high_zone,
        low_zone
      )
      
    }
    
    #-----------------------------------------
    # Clip to Delta boundary
    #-----------------------------------------
    
    high_zone <- st_intersection(
      high_zone,
      delta_boundary
    )
    
    #-----------------------------------------
    # Everything else becomes low risk
    #-----------------------------------------
    
    low_zone <- st_difference(
      delta_boundary,
      high_zone
    )
    
    list(
      high = st_transform(
        high_zone,
        4326
      ),
      low = st_transform(
        low_zone,
        4326
      )
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
      sprintf("%.0f", entrainment), "%</div>",
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
        
        label = ~paste0(
          round(
            entrainment,
            0
          ),
          "%"
        ),
        
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = FALSE,
          offset = c(0, -10),
          className = "entrainment-permanent-label"
        ),
        
        group = "Entrainment Labels"
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
          paste0("High Risk Zone: > ", threshold, "%"),
          paste0("Low Risk Zone: < ", threshold, "%")
        ),
        title = "Entrainment Risk Zones",
        opacity = 0.8
      ) %>%
      addLayersControl(
        overlayGroups = c(
          "Delta Boundary", "Channels", "Low Risk Zone",
          "High Risk Zone", "Nodes", "Entrainment Labels"
        ),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      leaflet::setView(
        lng = -121.60,
        lat = 38.05,
        zoom = 9
      )
  }
  make_combined_ptm_eh_map <- function(
    ptm_df,
    eh_df,
    threshold
  ) {
    
    validate(
      need(
        nrow(ptm_df) > 0,
        "Run the PTM and Event Horizon emulators to display results."
      ),
      need(
        nrow(eh_df) > 0,
        "No Event Horizon emulator result is available."
      )
    )
    
    sf_nodes <- ptm_df %>%
      filter(
        !is.na(X),
        !is.na(Y)
      ) %>%
      transmute(
        DSM2_Node,
        Location,
        Region,
        entrainment = Prediction_Final,
        X,
        Y
      ) %>%
      st_as_sf(
        coords = c("X", "Y"),
        crs = 4326,
        remove = FALSE
      ) %>%
      st_transform(26910)
    
    zones <- make_entrainment_zones(
      sf_nodes,
      threshold
    )
    
    display_nodes <- st_transform(
      sf_nodes,
      4326
    )
    
    eh_lines <- make_event_horizon_path_geometry(
      distance_miles = eh_df$Prediction_Final[1],
      regions = c("OMR")
    )
    
    eh_lines_display <- st_transform(
      eh_lines,
      4326
    )
    
    pal <- colorNumeric(
      "viridis",
      domain = range(
        display_nodes$entrainment,
        na.rm = TRUE
      )
    )
    
    leaflet() %>%
      addProviderTiles(
        providers$CartoDB.Positron
      ) %>%
      
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
        fillColor = ~pal(entrainment),
        fillOpacity = 0.9,
        stroke = TRUE,
        weight = 1,
        
        popup = ~paste0(
          "<div style='font-size:16px;line-height:1.55;min-width:280px;'>",
          "<div><b>DSM2 Node:</b> ", DSM2_Node, "</div>",
          "<div><b>Location:</b> ", Location, "</div>",
          "<div><b>Region:</b> ", Region, "</div>",
          "<div style='font-size:18px;font-weight:800;color:#8b1e1e;'>",
          "Entrainment: ",
          sprintf("%.0f", entrainment),
          "%",
          "</div>",
          "</div>"
        ),
        
        group = "PTM Nodes"
      ) %>%
      
      addLabelOnlyMarkers(
        data = display_nodes,
        
        label = ~paste0(
          round(
            entrainment,
            0
          ),
          "%"
        ),
        
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          offset = c(0, -10),
          className = "entrainment-permanent-label"
        ),
        
        group = "Entrainment Labels"
      ) %>%
      
      addPolylines(
        data = eh_lines_display,
        color = "#B2182B",
        weight = 5,
        opacity = 0.95,
        
        popup = ~paste0(
          "<b>Event Horizon:</b> ",
          sprintf(
            "%.1f",
            Event_Horizon_miles
          ),
          " river miles",
          "<br><b>Risk level:</b> ",
          threshold,
          "%"
        ),
        
        group = "Event Horizon Reach"
      ) %>%
      
      addLegend(
        position = "bottomright",
        pal = pal,
        values = display_nodes$entrainment,
        title = "PTM Entrainment (%)",
        opacity = 1
      ) %>%
      
      addLegend(
        position = "topright",
        
        colors = c(
          "#e8b5b5",
          "#a9d4e6",
          "#B2182B"
        ),
        
        labels = c(
          paste0(
            "High Risk Zone: > ",
            threshold,
            "%"
          ),
          paste0(
            "Low Risk Zone: < ",
            threshold,
            "%"
          ),
          paste0(
            "Event Horizon: ",
            sprintf(
              "%.1f",
              eh_df$Prediction_Final[1]
            ),
            " miles"
          )
        ),
        
        title = "Map Key",
        opacity = 0.85
      ) %>%
      
      addLayersControl(
        overlayGroups = c(
          "Delta Boundary",
          "Channels",
          "Low Risk Zone",
          "High Risk Zone",
          "PTM Nodes",
          "Entrainment Labels",
          "Event Horizon Reach"
        ),
        
        options = layersControlOptions(
          collapsed = FALSE
        )
      ) %>%
      
      leaflet::setView(
        lng = -121.60,
        lat = 38.05,
        zoom = 9
      )
  }
  
   
  make_event_geometry <- function(distance_miles) {
    fraction <- max(0, min(1, distance_miles / river_length_miles))
    list(
      high_line = lwgeom::st_linesubstring(river_centerline, 0, fraction),
      point = lwgeom::st_linesubstring(river_centerline, fraction, fraction)
    )
  }
  
  make_eh_map <- function(df) {
    
    validate(
      need(
        nrow(df) > 0,
        "Run the Event Horizon model to display results."
      )
    )
    
    eh_lines <- make_event_horizon_path_geometry(
      distance_miles = df$Prediction_Final[1],
      regions = c("OMR")
    )
    
    eh_lines_display <- st_transform(eh_lines, 4326)
    channels_display <- st_transform(dsm2_channels, 4326)
    nodes_display <- st_transform(dsm2_nodes, 4326)
    
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      
      addPolylines(
        data = channels_display,
        color = "#9ECAE1",
        weight = 1,
        opacity = 0.65,
        group = "DSM2 Channels"
      ) |>
      
      addPolylines(
        data = eh_lines_display,
        color = "#B2182B",
        weight = 5,
        opacity = 0.95,
        popup = ~paste0(
          "<div style='font-size:15px;line-height:1.45;'>",
          "<b>Region:</b> ", Region,
          "<br><b>Path target:</b> ", Location,
          "<br><b>Target node:</b> ", Target_Node,
          "<br><b>Channel:</b> ", CHAN_NO,
          "<br><b>From node:</b> ", FROM_NODE,
          "<br><b>To node:</b> ", TO_NODE,
          "<br><b>Event Horizon:</b> ",
          sprintf("%.0f", Event_Horizon_miles),
          " river miles",
          "</div>"
        ),
        group = "Event Horizon"
      ) |>
      
      addCircleMarkers(
        data = nodes_display |>
          dplyr::filter(DSM2_Node == 72),
        radius = 7,
        color = "#1F1F1F",
        fillColor = "#FFD34E",
        fillOpacity = 1,
        weight = 2,
        label = "Start node 72: Clifton Court Forebay",
        group = "Start Node"
      ) |>
      
      addLayersControl(
        overlayGroups = c(
          "DSM2 Channels",
          "Event Horizon",
          "Start Node"
        ),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      
    leaflet::setView(
      lng = -121.60,
      lat = 38.05,
      zoom = 10
    )
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
    # ---------------------------------------------------------------
    # Create smooth Event Horizon surface for contour lines
    # Contours represent Event Horizon distance in river miles.
    # ---------------------------------------------------------------
    
    contour_breaks <- c(5, 10, 15, 20)
    
    contour_background <- background %>%
      filter(
        !is.na(EXP),
        !is.na(VER),
        !is.na(Historical_Event_Horizon),
        EXP > 0,
        VER > 0
      ) %>%
      mutate(
        log_VER = log10(VER)
      )
    eh_breaks <- seq(
      floor(min(contour_background$Historical_Event_Horizon, na.rm = TRUE)),
      ceiling(max(contour_background$Historical_Event_Horizon, na.rm = TRUE)),
      length.out = 21
    )
    
    eh_labels <- paste0(
      round(head(eh_breaks, -1)),
      "-",
      round(tail(eh_breaks, -1)),
      " mi"
    )
    
    contour_background <- contour_background %>%
      mutate(
        EH_Bin = cut(
          Historical_Event_Horizon,
          breaks = eh_breaks,
          labels = eh_labels,
          include.lowest = TRUE
        )
      )
    validate(
      need(
        nrow(contour_background) >= 30,
        "Not enough historical Event Horizon points to draw contour lines."
      )
    )
    
    contour_model <- mgcv::gam(
      Historical_Event_Horizon ~ te(EXP, log_VER, k = c(12, 12)),
      data = contour_background,
      method = "REML"
    )
    
    contour_grid <- expand.grid(
      EXP = seq(
        min(contour_background$EXP, na.rm = TRUE),
        max(contour_background$EXP, na.rm = TRUE),
        length.out = 180
      ),
      log_VER = seq(
        min(contour_background$log_VER, na.rm = TRUE),
        max(contour_background$log_VER, na.rm = TRUE),
        length.out = 180
      )
    )
    
    # Mask the grid to the convex hull of the historical point cloud.
    # This prevents contours from extrapolating far outside the observed data.
    hull_index <- chull(
      contour_background$EXP,
      contour_background$log_VER
    )
    
    hull_points <- contour_background[hull_index, ]
    
    inside_hull <- sp::point.in.polygon(
      point.x = contour_grid$EXP,
      point.y = contour_grid$log_VER,
      pol.x = hull_points$EXP,
      pol.y = hull_points$log_VER
    )
    
    contour_prediction <- contour_grid %>%
      mutate(
        Historical_Event_Horizon = as.numeric(
          predict(
            contour_model,
            newdata = contour_grid
          )
        ),
        inside_hull = inside_hull > 0
      ) %>%
      filter(
        inside_hull,
        !is.na(Historical_Event_Horizon)
      )
    scenario_point <- df %>%
      filter(
        EXP > 0,
        VER > 0
      ) %>%
      mutate(
        log_VER = log10(VER)
      )
    contour_df <- ggplot_build(
      ggplot(
        contour_prediction,
        aes(
          x = EXP,
          y = log_VER,
          z = Historical_Event_Horizon
        )
      ) +
        geom_contour(
          breaks = c(5, 10, 15, 20)
        )
    )$data[[1]]
    contour_labels <- contour_df %>%
      group_by(level) %>%
      slice(n() %/% 2) %>%
      ungroup() %>%
      mutate(
        label = paste0(level, " mi")
      )
 
    
    p <- ggplot() +
      
      geom_contour(
        data = contour_prediction,
        aes(
          x = EXP,
          y = log_VER,
          z = Historical_Event_Horizon
        ),
        breaks = c(5, 10, 15, 20),
        color = "black",
        linewidth = 0.4,
        alpha = 0.7
      ) +
      
      geom_text(
        data = contour_labels,
        aes(
          x = x,
          y = y,
          label = label
        ),
        inherit.aes = FALSE,
        size = 4,
        fontface = "bold",
        color = "black"
      )+
      
      geom_point(
        data = contour_background,
        aes(
          x = EXP,
          y = log_VER,
          color = EH_Bin,
          text = paste0(
            "Historical Point",
            "<br>Export: ",
            fmt_int(EXP),
            " cfs",
            "<br>Vernalis: ",
            fmt_int(VER),
            " cfs",
            "<br>Event Horizon: ",
            fmt_int(Historical_Event_Horizon),
            " miles"
          )
        ),
        size = 2,
        alpha = 0.45
      ) +
      
      geom_point(
        data = scenario_point,
        aes(
          x = EXP,
          y = log_VER,
          text = paste0(
            "Selected Scenario",
            "<br>Name: ",
            Scenario_Name,
            "<br>Condition: ",
            Condition,
            if (
              "Window_End_Date" %in% names(scenario_point)
            ) {
              paste0(
                "<br>Observed date: ",
                format(
                  as.Date(
                    Window_End_Date,
                    origin = "1970-01-01"
                  ),
                  "%m/%d/%Y"
                )
              )
            } else {
              ""
            },
            "<br>Export: ",
            fmt_int(EXP),
            " cfs",
            "<br>Vernalis: ",
            fmt_int(VER),
            " cfs",
            "<br>Event Horizon: ",
            fmt_int(Prediction_Final),
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
    
      
      scale_color_viridis_d(
        option = "viridis",
        name = "Historical Event Horizon Distance (miles)",
        drop = FALSE
      )+
      
      scale_x_continuous(
        labels = scales::comma
      ) +
      
      scale_y_continuous(
        breaks = log10(
          c(
            1000,
            2000,
            5000,
            10000,
            20000
          )
        ),
        labels = scales::comma(
          c(
            1000,
            2000,
            5000,
            10000,
            20000
          )
        )
      ) +
      
      labs(
        title = paste0(
          "Historical Event Horizon Conditions > ",
          risk,
          "% Risk"
        ),
        subtitle =
          "The selected emulator result(s) are highlighted in red.",
        x = "Combined Export, EXP (cfs)",
        y = "Vernalis Flow, VER (cfs)"
      ) +
      
      theme_bw(base_family = "Arial", base_size = 14) +
      theme(
        text = element_text(
          family = "Arial",
          size = 14,
          color = "#1F1F1F"
        ),
        
        plot.title = element_text(
          family = "Arial",
          face = "bold",
          size = 18,
          color = "#1F1F1F"
        ),
        
        plot.subtitle = element_text(
          family = "Arial",
          size = 13,
          color = "#404040"
        ),
        
        axis.title = element_text(
          family = "Arial",
          face = "bold",
          size = 14,
          color = "#1F1F1F"
        ),
        
        axis.text = element_text(
          family = "Arial",
          size = 12,
          color = "#1F1F1F"
        ),
        
        legend.title = element_text(
          family = "Arial",
          face = "bold",
          size = 12,
          color = "#1F1F1F"
        ),
        
        legend.text = element_text(
          family = "Arial",
          size = 11,
          color = "#1F1F1F"
        )
      )
    
    
    
    ggplotly(
      p,
      tooltip = "text"
    ) %>%
      
      layout(
        font = list(
          family = "Arial, Segoe UI, sans-serif",
          size = 14,
          color = "#1F1F1F"
        ),
        
        legend = list(
          font = list(
            family = "Arial, Segoe UI, sans-serif",
            size = 12,
            color = "#1F1F1F"
          )
        )
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
    
    # Create plain coordinate columns for reliable Entrainment Labels.
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
        mutate(Risk_Zone = paste0("High risk: > ", threshold, "%"))
      
      zone_values <- stats::setNames(
        c("#A9D4E6", "#E8B5B5"),
        c(
          paste0("Low risk: < ", threshold, "%"),
          paste0("High risk: > ", threshold, "%")
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
        name = "Predicted entrainment (%)",
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
            "Red shading indicates entrainment > ",
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
          sprintf("%.0f", df$Prediction_Final[1]),
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
  
  
  # ===============================================================
  # Archive selector controls
  # ===============================================================
  
  available_archive_dates <- reactive({
    list_archive_dates()
  })
  
  
  output$current_archive_date_ui <- renderUI({
    
    choices <- available_archive_dates()
    
    selectInput(
      "current_archive_date",
      "Archive Run Date:",
      choices = choices,
      selected = if (length(choices) > 0) choices[1] else NULL
    )
  })
  
  
  output$forecast_archive_date_ui <- renderUI({
    
    choices <- available_archive_dates()
    
    selectInput(
      "forecast_archive_date",
      "Archive Run Date:",
      choices = choices,
      selected = if (length(choices) > 0) choices[1] else NULL
    )
  })
  
  
  output$forecast_archive_scenario_ui <- renderUI({
    
    req(
      input$forecast_archive_date
    )
    
    scenarios <- archive_scenario_choices(
      input$forecast_archive_date
    )
    
    selectInput(
      "forecast_archive_scenario",
      "OMRI Forecast Scenario:",
      choices = stats::setNames(scenarios, paste0("OMRI ", scenarios)),
      selected = if (length(scenarios) > 0) scenarios[1] else NULL
    )
  })
  
  
  output$current_ptm_archive_summary <- renderUI({
    
    req(
      input$current_archive_date
    )
    
    tags$div(
      class = "alert alert-light",
      tags$b("Selected archive: "),
      input$current_archive_date,
      tags$br(),
      "Measured rows will be used."
    )
  })
  
  
  output$forecast_ptm_archive_summary <- renderUI({
    
    req(
      input$forecast_archive_date,
      input$forecast_archive_scenario
    )
    
    tags$div(
      class = "alert alert-light",
      tags$b("Selected archive: "),
      input$forecast_archive_date,
      tags$br(),
      tags$b("OMRI scenario: "),
      input$forecast_archive_scenario
    )
  })
  
  
  register_condition <- function(
    prefix,
    condition_label
  ) {
    
    is_current <- identical(
      prefix,
      "current"
    )
    
    
    ptm_result <- eventReactive(
      input[[paste0(
        "run_",
        prefix,
        "_ptm"
      )]],
      {
        
        input_method <- input[[paste0(
          prefix,
          "_input_method"
        )]]
        
        if (identical(input_method, "single")) {
          
          result <- make_ptm_results(
            condition = condition_label,
            scenario_name = input[[paste0(
              prefix,
              "_ptm_name"
            )]],
            exp = input[[paste0(
              prefix,
              "_ptm_exp"
            )]],
            ver = input[[paste0(
              prefix,
              "_ptm_ver"
            )]],
            sac = input[[paste0(
              prefix,
              "_ptm_sac"
            )]],
            east = input[[paste0(
              prefix,
              "_ptm_east"
            )]],
            xgeo = input[[paste0(
              prefix,
              "_ptm_xgeo"
            )]]
          )
          
          run_name <- input[[paste0(
            prefix,
            "_ptm_name"
          )]]
          
        } else {
          
          req(
            identical(
              input_method,
              "folder"
            )
          )
          
          archive_date <- input[[paste0(
            prefix,
            "_archive_date"
          )]]
          
          run_name <- input[[paste0(
            prefix,
            "_ptm_archive_name"
          )]]
          
          if (is_current) {
            
            result <- make_archive_ptm_current(
              archive_date = archive_date,
              scenario_name = run_name
            )
            
          } else {
            
            req(
              input$forecast_archive_scenario
            )
            
            result <- make_archive_ptm_forecast(
              archive_date = archive_date,
              omri_scenario = input$forecast_archive_scenario,
              scenario_name = run_name
            )
          }
        }
        
        add_saved_run(
          run_name,
          condition_label,
          "PTM",
          result
        )
        
        result
      }
    )
    
    
    eco_result <- eventReactive(
      input[[paste0(
        "run_",
        prefix,
        "_eco"
      )]],
      {
        
        input_method <- input[[paste0(
          prefix,
          "_input_method"
        )]]
        
        if (identical(input_method, "single")) {
          
          result <- make_eco_results(
            condition = condition_label,
            scenario_name = input[[paste0(
              prefix,
              "_eco_name"
            )]],
            sac = input[[paste0(
              prefix,
              "_eco_sac"
            )]],
            yol = input[[paste0(
              prefix,
              "_eco_yol"
            )]],
            moke = input[[paste0(
              prefix,
              "_eco_moke"
            )]],
            dcc = as.numeric(
              input[[paste0(
                prefix,
                "_eco_dcc"
              )]]
            )
          )
          
          run_name <- input[[paste0(
            prefix,
            "_eco_name"
          )]]
          
        } else {
          
          validate(
            need(
              is_current,
              paste(
                "ECO-PTM archive-folder runs are available",
                "only for Observed Conditions."
              )
            )
          )
          
          archive_date <- input[[paste0(
            prefix,
            "_archive_date"
          )]]
          
          run_name <- input[[paste0(
            prefix,
            "_eco_archive_name"
          )]]
          
          result <- make_archive_eco_current(
            archive_date = archive_date,
            scenario_name = run_name
          )
        }
        
        add_saved_run(
          run_name,
          condition_label,
          "ECO-PTM",
          result
        )
        
        result
      }
    )
    
    
    eh_result <- eventReactive(
      input[[paste0(
        "run_",
        prefix,
        "_ptm"
      )]],
      {
        
        input_method <- input[[paste0(
          prefix,
          "_input_method"
        )]]
        
        risk <- as.numeric(
          input[[paste0(
            prefix,
            "_ptm_threshold"
          )]]
        )
        
        if (identical(input_method, "single")) {
          
          result <- make_eh_result(
            condition = condition_label,
            scenario_name = input[[paste0(
              prefix,
              "_ptm_name"
            )]],
            exp = input[[paste0(
              prefix,
              "_ptm_exp"
            )]],
            ver = input[[paste0(
              prefix,
              "_ptm_ver"
            )]],
            east = input[[paste0(
              prefix,
              "_ptm_east"
            )]],
            xgeo = input[[paste0(
              prefix,
              "_ptm_xgeo"
            )]],
            risk = risk
          )
          
          run_name <- input[[paste0(
            prefix,
            "_ptm_name"
          )]]
          
        } else {
          
          archive_date <- input[[paste0(
            prefix,
            "_archive_date"
          )]]
          
          run_name <- input[[paste0(
            prefix,
            "_ptm_archive_name"
          )]]
          
          if (is_current) {
            
            result <- make_archive_eh_current(
              archive_date = archive_date,
              scenario_name = run_name,
              risk = risk
            )
            
          } else {
            
            req(
              input$forecast_archive_scenario
            )
            
            result <- make_archive_eh_forecast(
              archive_date = archive_date,
              omri_scenario = input$forecast_archive_scenario,
              scenario_name = run_name,
              risk = risk
            )
          }
        }
        
        add_saved_run(
          run_name,
          condition_label,
          "Event Horizon",
          result
        )
        
        result
      }
    )
    
    
    # ---------------------------------------------------------------
    # Archive-current time-window controls
    # ---------------------------------------------------------------
    
    output[[paste0(
      prefix,
      "_ptm7_map_date_ui"
    )]] <- renderUI({
      
      req(
        ptm_result()
      )
      
      result_data <- ptm_result()
      
      if (
        !"Window_End_Date" %in% names(result_data)
      ) {
        return(NULL)
      }
      
      dates <- result_data %>%
        filter(
          Model == "PTM 7-Day Entrainment",
          !is.na(Window_End_Date)
        ) %>%
        pull(
          Window_End_Date
        ) %>%
        as.Date(origin = "1970-01-01") %>%
        unique() %>%
        sort()
      
      if (
        length(dates) <= 1
      ) {
        return(NULL)
      }
      
      sliderInput(
        paste0(
          prefix,
          "_ptm7_map_date"
        ),
        "Map Window End Date:",
        min = min(dates),
        max = max(dates),
        value = max(dates),
        step = 1,
        timeFormat = "%b %d, %Y",
        animate = animationOptions(
          interval = 1400,
          loop = TRUE
        )
      )
    })
    
    
    output[[paste0(
      prefix,
      "_ptm7_timeseries_nodes_ui"
    )]] <- renderUI({
      
      req(
        ptm_result()
      )
      
      data <- ptm_result() %>%
        filter(
          Model == "PTM 7-Day Entrainment"
        ) %>%
        distinct(
          DSM2_Node,
          Location
        ) %>%
        arrange(
          suppressWarnings(
            as.numeric(
              DSM2_Node
            )
          )
        )
      
      selectizeInput(
        paste0(
          prefix,
          "_ptm7_timeseries_nodes"
        ),
        "DSM2 Nodes Displayed:",
        choices = stats::setNames(
          data$DSM2_Node,
          paste0(
            data$DSM2_Node,
            " - ",
            data$Location
          )
        ),
        selected = head(
          data$DSM2_Node,
          5
        ),
        multiple = TRUE,
        options = list(
          plugins = list(
            "remove_button"
          )
        )
      )
    })
    
    
    output[[paste0(
      prefix,
      "_eh_map_date_ui"
    )]] <- renderUI({
      
      req(
        eh_result()
      )
      
      result_data <- eh_result()
      
      if (
        !"Window_End_Date" %in% names(result_data)
      ) {
        return(NULL)
      }
      
      dates <- result_data %>%
        filter(
          !is.na(Window_End_Date)
        ) %>%
        pull(
          Window_End_Date
        ) %>%
        as.Date(origin = "1970-01-01") %>%
        unique() %>%
        sort()
      
      if (
        length(dates) <= 1
      ) {
        return(NULL)
      }
      
      sliderInput(
        paste0(
          prefix,
          "_eh_map_date"
        ),
        "Map Window End Date:",
        min = min(dates),
        max = max(dates),
        value = max(dates),
        step = 1,
        timeFormat = "%b %d, %Y",
        animate = animationOptions(
          interval = 1400,
          loop = TRUE
        )
      )
    })
    
    
    # ---------------------------------------------------------------
    # PTM displays
    # ---------------------------------------------------------------
    
    output[[paste0(
      prefix,
      "_ptm7_timeseries"
    )]] <- renderPlotly({
      
      make_ptm_timeseries(
        ptm_result(),
        input[[paste0(
          prefix,
          "_ptm7_timeseries_nodes"
        )]],
        paste(
          condition_label,
          ": 7-Day Rolling Entrainment Prediction"
        )
      )
    })
    
    
    output[[paste0(
      prefix,
      "_ptm7_plot"
    )]] <- renderPlotly({
      
      data <- latest_result_window(
        ptm_result(),
        "PTM 7-Day Entrainment"
      )
      
      make_ptm_bar(
        data,
        paste(
          condition_label,
          ": PTM 7-Day Entrainment"
        )
      )
    })
    
    
    output[[paste0(
      prefix,
      "_ptm30_plot"
    )]] <- renderPlotly({
      
      data <- latest_result_window(
        ptm_result(),
        "PTM 30-Day Entrainment"
      )
      
      make_ptm_bar(
        data,
        paste(
          condition_label,
          ": PTM 30-Day Entrainment"
        )
      )
    })
    
    
    output[[paste0(
      prefix,
      "_ptm7_map"
    )]] <- renderLeaflet({
      
      ptm_data <- selected_result_window(
        ptm_result(),
        "PTM 7-Day Entrainment",
        input[[paste0(
          prefix,
          "_ptm7_map_date"
        )]]
      )
      
      make_combined_ptm_eh_map(
        ptm_df = ptm_data,
        eh_df = selected_eh_result(),
        
        threshold = as.numeric(
          input[[paste0(
            prefix,
            "_ptm_threshold"
          )]]
        )
      )
    })
    
    
    output[[paste0(
      prefix,
      "_ptm30_map"
    )]] <- renderLeaflet({
      
      data <- latest_result_window(
        ptm_result(),
        "PTM 30-Day Entrainment"
      )
      
      make_ptm_map(
        data,
        as.numeric(
          input[[paste0(
            prefix,
            "_ptm_threshold"
          )]]
        )
      )
    })
    
    
    output[[paste0(
      "download_",
      prefix,
      "_ptm7_map"
    )]] <- downloadHandler(
      
      filename = function() {
        
        paste0(
          prefix,
          "_PTM_7day_map_",
          format(
            Sys.Date(),
            "%Y-%m-%d"
          ),
          ".png"
        )
      },
      
      contentType = "image/png",
      
      content = function(file) {
        
        data <- selected_result_window(
          ptm_result(),
          "PTM 7-Day Entrainment",
          input[[paste0(
            prefix,
            "_ptm7_map_date"
          )]]
        )
        
        plot <- make_ptm_png_plot(
          df = data,
          threshold = input[[paste0(
            prefix,
            "_ptm_threshold"
          )]],
          title = paste(
            condition_label,
            ": PTM 7-Day Entrainment Risk Map"
          )
        )
        
        ggsave(
          filename = file,
          plot = plot,
          device = "png",
          width = 13,
          height = 9,
          units = "in",
          dpi = 300,
          bg = "white"
        )
      }
    )
    
    
    output[[paste0(
      "download_",
      prefix,
      "_ptm30_map"
    )]] <- downloadHandler(
      
      filename = function() {
        
        paste0(
          prefix,
          "_PTM_30day_map_",
          format(
            Sys.Date(),
            "%Y-%m-%d"
          ),
          ".png"
        )
      },
      
      contentType = "image/png",
      
      content = function(file) {
        
        data <- latest_result_window(
          ptm_result(),
          "PTM 30-Day Entrainment"
        )
        
        plot <- make_ptm_png_plot(
          df = data,
          threshold = input[[paste0(
            prefix,
            "_ptm_threshold"
          )]],
          title = paste(
            condition_label,
            ": PTM 30-Day Entrainment Risk Map"
          )
        )
        
        ggsave(
          filename = file,
          plot = plot,
          device = "png",
          width = 13,
          height = 9,
          units = "in",
          dpi = 300,
          bg = "white"
        )
      }
    )
    
    
    output[[paste0(
      prefix,
      "_ptm7_table"
    )]] <- renderTable({
      
      latest_result_window(
        ptm_result(),
        "PTM 7-Day Entrainment"
      ) %>%
        transmute(
          Window_End_Date = if (
            "Window_End_Date" %in% names(.)
          ) {
            format(
              as.Date(
                Window_End_Date,
                origin = "1970-01-01"
              ),
              "%m/%d/%Y"
            )
          } else {
            NA_character_
          },
          
          DSM2_Node,
          Location,
          
          
          Entrainment_Percentage = fmt_int(
            Prediction_Final
          )
        ) %>%
        arrange(
          suppressWarnings(
            as.numeric(
              DSM2_Node
            )
          ),
          DSM2_Node
        )
    })
    
    
    output[[paste0(
      prefix,
      "_ptm30_table"
    )]] <- renderTable({
      
      latest_result_window(
        ptm_result(),
        "PTM 30-Day Entrainment"
      ) %>%
        transmute(
          Window_End_Date = if (
            "Window_End_Date" %in% names(.)
          ) {
            format(
              as.Date(
                Window_End_Date,
                origin = "1970-01-01"
              ),
              "%m/%d/%Y"
            )
          } else {
            NA_character_
          },
          
          DSM2_Node,
          Location,
          Region,
          
          Entrainment_Percentage = fmt_int(
            Prediction_Final
          )
        ) %>%
        arrange(
          suppressWarnings(
            as.numeric(
              DSM2_Node
            )
          ),
          DSM2_Node
        )
    })
    
    
    # ---------------------------------------------------------------
    # ECO-PTM displays
    # ---------------------------------------------------------------
    
    output[[paste0(
      prefix,
      "_eco_table"
    )]] <- renderTable({
      
      eco_result() %>%
        transmute(
          Archive_Date = if (
            "Archive_Date" %in% names(.)
          ) {
            Archive_Date
          } else {
            NA_character_
          },
          
          Window = if (
            all(
              c(
                "Window_Start_Date",
                "Window_End_Date"
              ) %in% names(.)
            )
          ) {
            paste(
              format(
                as.Date(
                  Window_Start_Date,
                  origin = "1970-01-01"
                ),
                "%m/%d/%Y"
              ),
              "to",
              format(
                as.Date(
                  Window_End_Date,
                  origin = "1970-01-01"
                ),
                "%m/%d/%Y"
              )
            )
          } else {
            NA_character_
          },
          
          Model,
          
          Predicted_Population_Percentage = fmt_int(
            Prediction_Final
          ),
          
          SAC = fmt_int(
            SAC
          ),
          
          YOL = fmt_int(
            YOL
          ),
          
          MOKE = fmt_int(
            MOKE
          ),
          
          DCC
        )
    })
    
    
    # ---------------------------------------------------------------
    # Event Horizon displays
    # ---------------------------------------------------------------
    
    output[[paste0(
      prefix,
      "_eh_timeseries"
    )]] <- renderPlotly({
      
      make_eh_timeseries(
        eh_result(),
        paste(
          condition_label,
          ": Event Horizon Rolling Predictions"
        )
      )
    })
    
    
    selected_eh_result <- reactive({
      
      result <- eh_result()
      
      if (
        "Window_End_Date" %in% names(result) &&
        any(
          !is.na(
            result$Window_End_Date
          )
        )
      ) {
        
        selected_date <- input[[paste0(
          prefix,
          "_ptm7_map_date"
        )]]
        
        if (
          !is.null(selected_date)
        ) {
          
          selected_date <- as.Date(
            selected_date,
            origin = "1970-01-01"
          )
          
          result <- result %>%
            filter(
              Window_End_Date == selected_date
            )
        }
        
        if (nrow(result) == 0) {
          
          latest_date <- max(
            eh_result()$Window_End_Date,
            na.rm = TRUE
          )
          
          result <- eh_result() %>%
            filter(
              Window_End_Date == latest_date
            )
        }
      }
      
      result
    })
  
    output[[paste0(
      prefix,
      "_eh_summary"
    )]] <- renderUI({
      
      result <- selected_eh_result()
      
      req(
        nrow(result) > 0
      )
      
      risk_level <- as.numeric(
        result$Risk_Level_Percent[1]
      )
      
      river_miles <- as.numeric(
        result$Prediction_Final[1]
      )
      
      tags$div(
        class = "event-horizon-summary",
        
        tags$div(
          class = "event-horizon-summary-title",
          "Event Horizon Emulator"
        ),
        
        tags$div(
          class = "event-horizon-summary-text",
          
          "The predicted upstream extent (distance) of the Event Horizon, ",
          "measured in river miles from Clifton Court Forebay, under the ",
          "specified hydrologic conditions and selected entrainment risk level ",
          
          tags$strong(
            class = "event-horizon-highlight",
            paste0(
              risk_level,
              "%"
            )
          ),
          
          " is ",
          
          tags$strong(
            class = "event-horizon-highlight",
            paste0(
              sprintf(
                "%.1f",
                river_miles
              ),
              " miles"
            )
          ),
          
          "."
        )
      )
    })
    
    
    
    output[[paste0(
      prefix,
      "_eh_table"
    )]] <- renderTable({
      
      latest <- eh_result()
      
      if (
        "Window_End_Date" %in% names(latest) &&
        any(
          !is.na(
            latest$Window_End_Date
          )
        )
      ) {
        
        latest <- latest %>%
          filter(
            Window_End_Date == max(
              Window_End_Date,
              na.rm = TRUE
            )
          )
      }
      
      latest %>%
        transmute(
          Archive_Date = if (
            "Archive_Date" %in% names(.)
          ) {
            Archive_Date
          } else {
            NA_character_
          },
          
          OMRI_Scenario = if (
            "OMRI_Scenario" %in% names(.)
          ) {
            OMRI_Scenario
          } else {
            NA_character_
          },
          
          Window_End_Date = if (
            "Window_End_Date" %in% names(.)
          ) {
            format(
              as.Date(
                Window_End_Date,
                origin = "1970-01-01"
              ),
              "%m/%d/%Y"
            )
          } else {
            NA_character_
          },
          
          Scenario_Name,
          Risk_Level_Percent,
          
          Event_Horizon_Miles = fmt_int(
            Prediction_Final
          ),
          
          EXP = fmt_int(
            EXP
          ),
          
          VER = fmt_int(
            VER
          ),
          
          EAST = fmt_int(
            EAST
          ),
          
          XGEO = fmt_int(
            XGEO
          )
        )
    })
    
    
    output[[paste0(
      prefix,
      "_eh7_table"
    )]] <- renderTable({

      result <- eh_result()

      req(
        nrow(result) > 0
      )

      if (
        "Window_End_Date" %in% names(result) &&
        any(
          !is.na(
            result$Window_End_Date
          )
        )
      ) {
        result <- result %>%
          mutate(
            Observed_Date = as.Date(
              Window_End_Date,
              origin = "1970-01-01"
            )
          ) %>%
          arrange(
            Observed_Date
          ) %>%
          slice_tail(
            n = 7
          )
      } else {
        result <- result %>%
          mutate(
            Observed_Date = as.Date(NA)
          )
      }

      result %>%
        transmute(
          Observed_Date = ifelse(
            is.na(Observed_Date),
            NA_character_,
            format(
              Observed_Date,
              "%m/%d/%Y"
            )
          ),

          Event_Horizon_Miles = sprintf(
            "%.1f",
            as.numeric(
              Prediction_Final
            )
          )
        )
    })


    output[[paste0(
      prefix,
      "_eh_map"
    )]] <- renderLeaflet({
      
      make_eh_map(
        selected_eh_result()
      )
    })
    
    
    output[[paste0(
      "download_",
      prefix,
      "_eh_map"
    )]] <- downloadHandler(
      
      filename = function() {
        
        paste0(
          prefix,
          "_Event_Horizon_map_",
          Sys.Date(),
          ".png"
        )
      },
      
      contentType = "image/png",
      
      content = function(file) {
        
        plot <- make_eh_png_plot(
          selected_eh_result(),
          paste(
            condition_label,
            ": Event Horizon Map"
          )
        )
        
        ggsave(
          filename = file,
          plot = plot,
          device = "png",
          width = 13,
          height = 9,
          units = "in",
          dpi = 300,
          bg = "white"
        )
      }
    )
    
    
    output[[paste0(
      prefix,
      "_eh_scatter"
    )]] <- renderPlotly({

      input_method <- input[[paste0(
        prefix,
        "_input_method"
      )]]

      scatter_data <- if (
        is_current &&
        identical(
          input_method,
          "folder"
        )
      ) {
        eh_result()
      } else {
        selected_eh_result()
      }

      make_eh_scatter(
        scatter_data
      )
    })
    
    
    # ---------------------------------------------------------------
    # Downloads
    # ---------------------------------------------------------------
    
    output[[paste0(
      "download_",
      prefix,
      "_ptm"
    )]] <- downloadHandler(
      
      filename = function() {
        
        paste0(
          prefix,
          "_PTM_results_",
          Sys.Date(),
          ".csv"
        )
      },
      
      content = function(file) {
        
        write_csv(
          ptm_result() %>%
            mutate(
              across(
                where(
                  is.numeric
                ),
                ~ round(
                  .x
                )
              )
            ),
          file
        )
      }
    )
    
    
    output[[paste0(
      "download_",
      prefix,
      "_eco"
    )]] <- downloadHandler(
      
      filename = function() {
        
        paste0(
          prefix,
          "_ECO_PTM_results_",
          Sys.Date(),
          ".csv"
        )
      },
      
      content = function(file) {
        
        write_csv(
          eco_result() %>%
            mutate(
              across(
                where(
                  is.numeric
                ),
                ~ round(
                  .x
                )
              )
            ),
          file
        )
      }
    )
    
    
    output[[paste0(
      "download_",
      prefix,
      "_eh"
    )]] <- downloadHandler(
      
      filename = function() {
        
        paste0(
          prefix,
          "_Event_Horizon_result_",
          Sys.Date(),
          ".csv"
        )
      },
      
      content = function(file) {
        
        write_csv(
          eh_result() %>%
            mutate(
              across(
                where(
                  is.numeric
                ),
                ~ round(
                  .x
                )
              )
            ),
          file
        )
      }
    )
  }
  
  
  register_condition(
    "current",
    "Observed Conditions"
  )
  
  register_condition(
    "forecast",
    "Forecast Conditions"
  )
  
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
          paste(Condition, Scenario_Name, sep = " - "),
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
          sprintf("%.0f", Prediction_Final),
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
        text = ~sprintf("%.0f", Prediction_Final),
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
        text = ~sprintf("%.0f", Prediction_Final),
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
      write_csv(comparison_data() %>% mutate(across(where(is.numeric), ~round(.x))), file)
    }
  )
  
  
  
  # ===============================================================
  # Automatic OMRI archive comparison
  # ===============================================================
  
  output$omri_archive_dates_ui <- renderUI({
    
    choices <- available_archive_dates()
    
    selectizeInput(
      "omri_archive_dates",
      "Archive Run Dates:",
      choices = choices,
      selected = choices[1],
      multiple = TRUE,
      options = list(
        plugins = list(
          "remove_button"
        )
      )
    )
  })
  
  
  omri_comparison_results <- eventReactive(
    input$build_omri_comparison,
    {
      
      req(
        input$omri_archive_dates,
        input$omri_comparison_model
      )
      
      model_name <- input$omri_comparison_model
      
      all_results <- bind_rows(
        lapply(
          input$omri_archive_dates,
          function(archive_date) {
            
            scenarios <- archive_scenario_choices(
              archive_date
            )
            
            validate(
              need(
                length(scenarios) > 0,
                paste0(
                  "No OMRI CSV files were found for ",
                  archive_date,
                  "."
                )
              )
            )
            
            bind_rows(
              lapply(
                scenarios,
                function(scenario) {
                  
                  scenario_name <- paste(
                    archive_date,
                    scenario,
                    sep = " - OMRI "
                  )
                  
                  if (
                    identical(
                      model_name,
                      "PTM 7-Day Entrainment"
                    )
                  ) {
                    
                    make_archive_ptm_forecast(
                      archive_date = archive_date,
                      omri_scenario = scenario,
                      scenario_name = scenario_name
                    )
                    
                  } else {
                    
                    make_archive_eh_forecast(
                      archive_date = archive_date,
                      omri_scenario = scenario,
                      scenario_name = scenario_name,
                      risk = as.numeric(
                        input$omri_comparison_risk
                      )
                    )
                  }
                }
              )
            )
          }
        )
      )
      
      all_results
    }
  )
  
  
  output$omri_comparison_status <- renderUI({
    
    if (input$build_omri_comparison == 0) {
      
      tags$div(
        class = "alert alert-info",
        paste(
          "Select one or more archive dates, choose a forecast model,",
          "and click Run All Available OMRI Scenarios."
        )
      )
      
    } else {
      
      result <- omri_comparison_results()
      
      tags$div(
        class = "alert alert-success",
        paste0(
          "Comparison created for ",
          length(
            unique(
              result$Archive_Date
            )
          ),
          " archive date(s) and ",
          length(
            unique(
              paste(
                result$Archive_Date,
                result$OMRI_Scenario
              )
            )
          ),
          " archive-date/scenario combination(s)."
        )
      )
    }
  })
  
  
  output$omri_comparison_table <- renderTable({
    
    data <- omri_comparison_results()
    
    if (
      identical(
        input$omri_comparison_model,
        "PTM 7-Day Entrainment"
      )
    ) {
      
      data %>%
        transmute(
          Archive_Date,
          OMRI_Scenario,
          Window_Start_Date,
          Window_End_Date,
          DSM2_Node,
          Location,
          Entrainment_Percentage = fmt_int(
            Prediction_Final
          )
        ) %>%
        arrange(
          Archive_Date,
          suppressWarnings(
            as.numeric(
              OMRI_Scenario
            )
          ),
          suppressWarnings(
            as.numeric(
              DSM2_Node
            )
          )
        )
      
    } else {
      
      data %>%
        transmute(
          Archive_Date,
          OMRI_Scenario,
          Window_Start_Date,
          Window_End_Date,
          Risk_Level_Percent,
          Event_Horizon_Miles = fmt_int(
            Prediction_Final
          ),
          EXP = fmt_int(
            EXP
          ),
          VER = fmt_int(
            VER
          ),
          EAST = fmt_int(
            EAST
          ),
          XGEO = fmt_int(
            XGEO
          )
        ) %>%
        arrange(
          Archive_Date,
          suppressWarnings(
            as.numeric(
              OMRI_Scenario
            )
          )
        )
    }
  })
  
  
  output$omri_comparison_plot <- renderPlotly({
    
    data <- omri_comparison_results()
    
    validate(
      need(
        nrow(data) > 0,
        "No OMRI comparison results are available."
      )
    )
    
    data <- data %>%
      mutate(
        Comparison_Label = paste0(
          Archive_Date,
          " | OMRI ",
          OMRI_Scenario
        )
      )
    
    if (
      identical(
        input$omri_comparison_model,
        "PTM 7-Day Entrainment"
      )
    ) {
      
      plot_data <- data %>%
        mutate(
          DSM2_Node_Number = suppressWarnings(
            as.numeric(
              DSM2_Node
            )
          ),
          
          DSM2_Node = factor(
            DSM2_Node,
            levels = rev(
              unique(
                DSM2_Node[
                  order(
                    DSM2_Node_Number,
                    DSM2_Node
                  )
                ]
              )
            )
          ),
          
          Hover_Text = paste0(
            "<b>Archive date:</b> ",
            Archive_Date,
            "<br><b>OMRI scenario:</b> ",
            OMRI_Scenario,
            "<br><b>Node:</b> ",
            DSM2_Node,
            "<br><b>Location:</b> ",
            Location,
            "<br><b>Prediction:</b> ",
            sprintf(
              "%.0f%%",
              Prediction_Final
            )
          )
        )
      
      plot_ly(
        plot_data,
        x = ~Prediction_Final,
        y = ~DSM2_Node,
        color = ~Comparison_Label,
        type = "bar",
        orientation = "h",
        text = ~sprintf(
          "%.0f",
          Prediction_Final
        ),
        hovertext = ~Hover_Text,
        hoverinfo = "text"
      ) %>%
        layout(
          barmode = "group",
          
          title = list(
            text = "<b>OMRI Forecast PTM 7-Day Comparison</b>",
            x = 0.02,
            xanchor = "left"
          ),
          
          xaxis = list(
            title = "<b>Predicted Entrainment (%)</b>"
          ),
          
          yaxis = list(
            title = "<b>DSM2 Node</b>",
            automargin = TRUE
          ),
          
          plot_bgcolor = "#FFFFFF",
          paper_bgcolor = "#FFFFFF",
          
          font = list(
            family = "Arial, Segoe UI, sans-serif",
            size = 13,
            color = "#1F1F1F"
          ),
          
          margin = list(
            l = 110,
            r = 50,
            t = 80,
            b = 80
          )
        ) %>%
        config(
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "png",
            filename = "OMRI_PTM_7day_comparison",
            width = 1800,
            height = 1100,
            scale = 2
          )
        )
      
    } else {
      
      plot_data <- data %>%
        mutate(
          Hover_Text = paste0(
            "<b>Archive date:</b> ",
            Archive_Date,
            "<br><b>OMRI scenario:</b> ",
            OMRI_Scenario,
            "<br><b>Risk:</b> ",
            Risk_Level_Percent,
            "%",
            "<br><b>Event Horizon:</b> ",
            sprintf(
              "%.0f river miles",
              Prediction_Final
            )
          )
        )
      
      plot_ly(
        plot_data,
        x = ~Comparison_Label,
        y = ~Prediction_Final,
        color = ~Comparison_Label,
        type = "bar",
        text = ~sprintf(
          "%.0f",
          Prediction_Final
        ),
        textposition = "outside",
        hovertext = ~Hover_Text,
        hoverinfo = "text"
      ) %>%
        layout(
          title = list(
            text = "<b>OMRI Forecast Event Horizon Comparison</b>",
            x = 0.02,
            xanchor = "left"
          ),
          
          xaxis = list(
            title = "<b>Archive Date and OMRI Scenario</b>",
            tickangle = -30,
            automargin = TRUE
          ),
          
          yaxis = list(
            title = "<b>Event Horizon Distance (River Miles)</b>"
          ),
          
          showlegend = FALSE,
          
          plot_bgcolor = "#FFFFFF",
          paper_bgcolor = "#FFFFFF",
          
          font = list(
            family = "Arial, Segoe UI, sans-serif",
            size = 13,
            color = "#1F1F1F"
          ),
          
          margin = list(
            l = 90,
            r = 50,
            t = 80,
            b = 150
          )
        ) %>%
        config(
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "png",
            filename = "OMRI_Event_Horizon_comparison",
            width = 1800,
            height = 1000,
            scale = 2
          )
        )
    }
  })
  
  
  output$download_omri_comparison <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "OMRI_archive_comparison_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      
      write_csv(
        omri_comparison_results() %>%
          mutate(
            across(
              where(
                is.numeric
              ),
              ~ round(
                .x
              )
            )
          ),
        file
      )
    }
  )
  
  
  output$about_info_table <- render_gt({
    data <- data.frame(
      Name = c(
        "Adam Witt",
        "Puneet Khatavkar",
        "Laura Manuel",
        "Roja Kaveh Garna",
        "Jo Anna Beck",
        "Josh Israel"
      ),
      Role = c(
        "Activity Lead",
        "Quality review",
        "App Developer",
        "App Developer",
        "Contracting Officers Representative",
        "Reclamation Activity Oversight"
      ),
      Email = c(
        "adam.witt@stantec.com",
        "Puneet.Khatavkar@stantec.com",
        "Laura.Manuel@stantec.com",
        "Roja.KavehGarna@stantec.com",
        "jbeck@usbr.gov",
        "jaisrael@usbr.gov"
      )
    )
    about_info_table <- gt(data)
    about_info_table |>
      tab_row_group(
        label = html("<strong><em>Bureau of Reclamation,</strong><br>Bay-Delta Office</em>"), 
        rows = 5:6
      )|>
      tab_row_group(
        label = html("<strong><em>Stantec Inc.,</strong><br>Sacramento C St. Office</em>"), 
        rows = 1:4
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
          "<strong>Note:</strong> For some QA/QC data sources, only reviewed or approved stage data are available. Corresponding discharge values are calculated using a stage-discharge rating curve."
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
          "<strong>Note:</strong> For some QA/QC data sources, only reviewed or approved stage data are available. Corresponding discharge values are calculated using a stage-discharge rating curve."
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
      "{15+5k???k=0,1,???,13}"
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
      "{15+5k???k=0,1,???,13}"
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
            weight = gt::px(1)
          )
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
          "<strong>Note:</strong> For some QA/QC data sources, only reviewed or approved stage data are available. Corresponding discharge values are calculated using a stage-discharge rating curve."
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
      leaflet::setView(
        lng = -121.60,
        lat = 38.05,
        zoom = 15
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
