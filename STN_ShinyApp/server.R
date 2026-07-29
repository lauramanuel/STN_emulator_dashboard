
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

MODEL_DIR <- "STN_EMULATOR/models"
OUTPUT_DIR <- "STN_EMULATOR/Output"
SHAPE_DIR <- "STN_EMULATOR/shapefiles"
ARCHIVE_DIR <- "STN_EMULATOR/Archive_Folder"

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
      end_date
  ) {

    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)

    query_url <- paste0(
      "https://waterservices.usgs.gov/nwis/iv/",
      "?format=rdb",
      "&sites=", site_number,
      "&startDT=", format(start_date, "%Y-%m-%d"),
      "&endDT=", format(end_date + 1, "%Y-%m-%d"),
      "&parameterCd=00060",
      "&siteStatus=all"
    )

    temporary_file <- tempfile(
      fileext = ".txt"
    )

    old_timeout <- getOption("timeout")

    on.exit(
      {
        options(timeout = old_timeout)

        if (file.exists(temporary_file)) {
          unlink(temporary_file)
        }
      },
      add = TRUE
    )

    options(
      timeout = max(
        120,
        old_timeout
      )
    )

    download_status <- tryCatch(
      {
        utils::download.file(
          url = query_url,
          destfile = temporary_file,
          mode = "wb",
          method = "libcurl",
          quiet = TRUE
        )

        TRUE
      },
      error = function(e) {
        FALSE
      },
      warning = function(w) {
        invokeRestart("muffleWarning")
      }
    )

    if (
      !isTRUE(download_status) ||
      !file.exists(temporary_file) ||
      file.info(temporary_file)$size == 0
    ) {
      stop(
        paste0(
          "Could not download XGEO source data from USGS site ",
          site_number,
          ". Check the internet connection and try again."
        )
      )
    }

    text_lines <- readLines(
      temporary_file,
      warn = FALSE
    )

    data_lines <- text_lines[
      !grepl(
        "^#",
        text_lines
      ) &
      nzchar(
        trimws(
          text_lines
        )
      )
    ]

    if (length(data_lines) < 3) {
      stop(
        paste0(
          "USGS site ",
          site_number,
          " returned no usable discharge records for ",
          format(start_date),
          " through ",
          format(end_date),
          "."
        )
      )
    }

    usgs_data <- utils::read.delim(
      text = paste(
        data_lines,
        collapse = "\n"
      ),
      header = TRUE,
      sep = "\t",
      stringsAsFactors = FALSE,
      check.names = FALSE,
      na.strings = c(
        "",
        "Ice",
        "Eqp",
        "Ssn",
        "Rat",
        "Dis"
      )
    )

    # The second non-comment RDB row contains field-width/type codes.
    if (
      nrow(usgs_data) > 0 &&
      grepl(
        "^[0-9]+[a-zA-Z]$",
        as.character(
          usgs_data$agency_cd[1]
        )
      )
    ) {
      usgs_data <- usgs_data[
        -1,
        ,
        drop = FALSE
      ]
    }

    date_time_column <- names(usgs_data)[
      normalize_archive_name(
        names(usgs_data)
      ) %in% c(
        "DATETIME",
        "DATE"
      )
    ][1]

    discharge_columns <- names(usgs_data)[
      grepl(
        "00060",
        names(usgs_data),
        fixed = TRUE
      ) &
      !grepl(
        "_cd$",
        names(usgs_data),
        ignore.case = TRUE
      )
    ]

    if (
      is.na(date_time_column) ||
      length(discharge_columns) == 0
    ) {
      stop(
        paste0(
          "USGS response for site ",
          site_number,
          " did not contain the expected date-time and discharge fields."
        )
      )
    }

    discharge_column <- discharge_columns[1]

    parsed_date_time <- suppressWarnings(
      as.POSIXct(
        usgs_data[[date_time_column]],
        tz = "America/Los_Angeles"
      )
    )

    # A second parser is useful when timestamps include an explicit
    # UTC offset that the local R installation handles differently.
    missing_date_time <- is.na(
      parsed_date_time
    )

    if (any(missing_date_time)) {
      parsed_date_time[missing_date_time] <- suppressWarnings(
        as.POSIXct(
          usgs_data[[date_time_column]][missing_date_time],
          format = "%Y-%m-%d %H:%M",
          tz = "America/Los_Angeles"
        )
      )
    }

    flow_values <- suppressWarnings(
      as.numeric(
        usgs_data[[discharge_column]]
      )
    )

    daily_data <- data.frame(
      DATE = as.Date(
        parsed_date_time,
        tz = "America/Los_Angeles"
      ),
      FLOW = flow_values
    ) %>%
      filter(
        !is.na(DATE),
        !is.na(FLOW)
      ) %>%
      group_by(DATE) %>%
      summarise(
        FLOW = mean(
          FLOW,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      filter(
        DATE >= start_date,
        DATE <= end_date
      )

    validate(
      need(
        nrow(daily_data) > 0,
        paste0(
          "No daily discharge values were available from USGS site ",
          site_number,
          " for the required dates."
        )
      )
    )

    daily_data
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
        "No valid archive dates were available for downloading XGEO."
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

    missing_dates <- setdiff(
      required_dates,
      cached_xgeo$DATE
    )

    if (length(missing_dates) > 0) {

      download_start <- min(
        missing_dates
      )

      download_end <- max(
        missing_dates
      )

      xgeo_a <- download_usgs_daily_flow(
        site_number = "11447890",
        start_date = download_start,
        end_date = download_end
      ) %>%
        rename(
          XGEO_A = FLOW
        )

      xgeo_c <- download_usgs_daily_flow(
        site_number = "11447905",
        start_date = download_start,
        end_date = download_end
      ) %>%
        rename(
          XGEO_C = FLOW
        )

      downloaded_xgeo <- full_join(
        xgeo_a,
        xgeo_c,
        by = "DATE"
      ) %>%
        arrange(
          DATE
        ) %>%
        mutate(
          XGEO = XGEO_A - XGEO_C
        )

      cached_xgeo <- bind_rows(
        cached_xgeo,
        downloaded_xgeo
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

    validate(
      need(
        length(missing_after_download) == 0,
        paste0(
          "XGEO could not be calculated for these date(s): ",
          paste(
            format(
              missing_after_download
            ),
            collapse = ", "
          ),
          "."
        )
      )
    )

    result
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
        DCC = round(
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

    mean_archive_inputs(
      measured
    ) %>%
      mutate(
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
        "Current Conditions",
        scenario_name,
        archive_date,
        "Measured"
      ),

      predict_ptm_from_windows(
        measured_30,
        ptm30_model,
        "PTM 30-Day Entrainment",
        ref$nodes_30d,
        "Current Conditions",
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
      condition = "Current Conditions",
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
      "Current Conditions",
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
            " — ",
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
            "%.2f%%",
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
            "%.2f river miles",
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
                "only for Current Conditions."
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
          "_eh"
        )]],
      {

        input_method <- input[[paste0(
            prefix,
            "_input_method"
          )]]

        risk <- as.numeric(
          input[[paste0(
              prefix,
              "_eh_risk"
            )]]
        )

        if (identical(input_method, "single")) {

          result <- make_eh_result(
            condition = condition_label,
            scenario_name = input[[paste0(
                prefix,
                "_eh_name"
              )]],
            exp = input[[paste0(
                prefix,
                "_eh_exp"
              )]],
            ver = input[[paste0(
                prefix,
                "_eh_ver"
              )]],
            east = input[[paste0(
                prefix,
                "_eh_east"
              )]],
            xgeo = input[[paste0(
                prefix,
                "_eh_xgeo"
              )]],
            risk = risk
          )

          run_name <- input[[paste0(
              prefix,
              "_eh_name"
            )]]

        } else {

          archive_date <- input[[paste0(
              prefix,
              "_archive_date"
            )]]

          run_name <- input[[paste0(
              prefix,
              "_eh_archive_name"
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
            " — ",
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
          "— PTM 7-Day Rolling Predictions"
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
          "— PTM 7-Day Entrainment"
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
          "— PTM 30-Day Entrainment"
        )
      )
    })


    output[[paste0(
        prefix,
        "_ptm7_map"
      )]] <- renderLeaflet({

      data <- selected_result_window(
        ptm_result(),
        "PTM 7-Day Entrainment",
        input[[paste0(
            prefix,
            "_ptm7_map_date"
          )]]
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
            "— PTM 7-Day Entrainment Risk Map"
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
            "— PTM 30-Day Entrainment Risk Map"
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
            Window_End_Date
          } else {
            as.Date(NA)
          },

          DSM2_Node,
          Location,
          Region,

          Prediction_Percent = round(
            Prediction_Final,
            2
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
            Window_End_Date
          } else {
            as.Date(NA)
          },

          DSM2_Node,
          Location,
          Region,

          Prediction_Percent = round(
            Prediction_Final,
            2
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
              Window_Start_Date,
              "to",
              Window_End_Date
            )
          } else {
            NA_character_
          },

          Model,

          Prediction_Percent = round(
            Prediction_Final,
            2
          ),

          SAC = round(
            SAC,
            2
          ),

          YOL = round(
            YOL,
            2
          ),

          MOKE = round(
            MOKE,
            2
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
          "— Event Horizon Rolling Predictions"
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
            "_eh_map_date"
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
            Window_End_Date
          } else {
            as.Date(NA)
          },

          Scenario_Name,
          Risk_Level_Percent,

          Event_Horizon_Miles = round(
            Prediction_Final,
            2
          ),

          EXP = round(
            EXP,
            2
          ),

          VER = round(
            VER,
            2
          ),

          EAST = round(
            EAST,
            2
          ),

          XGEO = round(
            XGEO,
            2
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
            "— Event Horizon Map"
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

      make_eh_scatter(
        selected_eh_result()
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
                  .x,
                  2
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
                  .x,
                  2
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
                  .x,
                  2
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
    "Current Conditions"
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
                    sep = " — OMRI "
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
          Prediction_Percent = round(
            Prediction_Final,
            2
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
          Event_Horizon_Miles = round(
            Prediction_Final,
            2
          ),
          EXP = round(
            EXP,
            2
          ),
          VER = round(
            VER,
            2
          ),
          EAST = round(
            EAST,
            2
          ),
          XGEO = round(
            XGEO,
            2
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
              "%.2f%%",
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
          "%.2f",
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
              "%.2f river miles",
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
          "%.2f",
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
                .x,
                2
              )
            )
          ),
        file
      )
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
}
