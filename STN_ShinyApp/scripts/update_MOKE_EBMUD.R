# update_MOKE_EBMUD.R
#
# Purpose:
#   1) Read the historical EBMUD GOLF/MOKE CSV.
#   2) Find the daily reports currently listed on the EBMUD website.
#   3) For dates not already in the CSV, extract "Mokelumne River below WID".
#   4) Append only the missing dates.
#   5) Update STN_ShinyApp/data/MOKE_daily.csv for the Shiny app.
#
# Required R packages:
#   install.packages(c("httr2", "rvest"))
#
# IMPORTANT:
#   EBMUD states that daily report values are provisional and subject to change.

library(httr2)
library(rvest)

# -------------------------------------------------------------------------
# 1. FILE PATHS
# -------------------------------------------------------------------------

# Permanent master file used by the Shiny app.
# The GitHub Action runs this script with STN_ShinyApp as its working directory.
master_csv <- file.path("data", "MOKE_daily.csv")

if (!file.exists(master_csv)) {
  stop(
    "Master MOKE file was not found at: ",
    master_csv,
    ". Add the tested MOKE_daily.csv to STN_ShinyApp/data before running this script."
  )
}

input_csv <- master_csv
output_csv <- master_csv

# -------------------------------------------------------------------------
# 2. EBMUD SETTINGS
# -------------------------------------------------------------------------

ebmud_page <- paste0(
  "https://www.ebmud.com/water/about-your-water/",
  "water-supply/water-supply-reports/daily-water-supply-report"
)

target_parameter <- "Mokelumne River below WID"

# -------------------------------------------------------------------------
# 3. HELPER FUNCTIONS
# -------------------------------------------------------------------------

# Convert dates such as "8/30/2026" to Date safely.
parse_mdy <- function(x) {
  as.Date(x, format = "%m/%d/%Y")
}

# Read the special EBMUD CSV while preserving its first 4 metadata rows.
read_ebmud_csv <- function(path) {

  if (!file.exists(path)) {
    stop("Input CSV was not found: ", path)
  }

  raw_lines <- readLines(
    path,
    warn = FALSE,
    encoding = "UTF-8"
  )

  if (length(raw_lines) < 5) {
    stop("The CSV does not appear to contain the expected EBMUD structure.")
  }

  metadata_lines <- raw_lines[1:4]
  data_lines <- raw_lines[5:length(raw_lines)]

  # Split only at the first comma.
  split_line <- function(x) {
    pos <- regexpr(",", x, fixed = TRUE)

    if (pos[1] == -1) {
      return(c(x, NA_character_))
    }

    c(
      substr(x, 1, pos[1] - 1),
      substr(x, pos[1] + 1, nchar(x))
    )
  }

  parts <- t(
    vapply(
      data_lines,
      split_line,
      FUN.VALUE = c("", "")
    )
  )

  df <- data.frame(
    Time = parts[, 1],
    GOLF = suppressWarnings(as.numeric(parts[, 2])),
    stringsAsFactors = FALSE
  )

  df$Date <- as.Date(
    sub(
      " .*",
      "",
      df$Time
    ),
    format = "%m/%d/%Y"
  )

  df <- df[
    !is.na(df$Date) &
      !is.na(df$GOLF),
    ,
    drop = FALSE
  ]

  list(
    metadata = metadata_lines,
    data = df
  )
}

# Find the daily report dates currently displayed on the public EBMUD page.
get_available_report_dates <- function() {

  response <- request(ebmud_page) |>
    req_user_agent(
      "Mozilla/5.0 (compatible; MOKE data updater for Shiny app)"
    ) |>
    req_retry(max_tries = 3) |>
    req_perform()

  page <- rvest::read_html(
    httr2::resp_body_string(
      response
    )
  )

  date_text <- page |>
    html_elements("a") |>
    html_text2()

  date_text <- trimws(date_text)

  # Keep only strings that look like M/D/YYYY or MM/DD/YYYY.
  date_text <- date_text[
    grepl(
      "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$",
      date_text
    )
  ]

  dates <- parse_mdy(date_text)

  dates <- unique(
    dates[
      !is.na(dates)
    ]
  )

  sort(dates)
}

# Download one EBMUD daily report and extract "Mokelumne River below WID".
get_moke_value <- function(report_date) {

  # EBMUD's report request uses dates without leading zeroes,
  # e.g. 8/30/2026 rather than 08/30/2026.
  report_lt <- as.POSIXlt(
    as.Date(report_date)
  )

  date_text <- paste0(
    report_lt$mon + 1,
    "/",
    report_lt$mday,
    "/",
    report_lt$year + 1900
  )

  # Match the exact successful request copied from Edge DevTools:
  # https://www.ebmud.com/a?url=https://legacy.ebmud.com/if/daily-water-supply-report/WSE_DailyReport.asp?Date=8/25/2026
  report_url <- paste0(
    "https://www.ebmud.com/a?url=",
    "https://legacy.ebmud.com/if/daily-water-supply-report/",
    "WSE_DailyReport.asp?Date=",
    date_text
  )

  # First visit the public report page so EBMUD can establish the normal
  # site session/cookies before the XHR request is made.
  request(ebmud_page) |>
    req_user_agent(
      paste0(
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
        "AppleWebKit/537.36 (KHTML, like Gecko) ",
        "Chrome/152.0.0.0 Safari/537.36 Edg/152.0.0.0"
      )
    ) |>
    req_perform()

  response <- request(report_url) |>
    req_user_agent(
      paste0(
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
        "AppleWebKit/537.36 (KHTML, like Gecko) ",
        "Chrome/152.0.0.0 Safari/537.36 Edg/152.0.0.0"
      )
    ) |>
    req_headers(
      Accept = "*/*",
      `Accept-Language` = "en-US,en;q=0.9",
      Referer = ebmud_page,
      `X-Requested-With` = "XMLHttpRequest",
      `Sec-Fetch-Dest` = "empty",
      `Sec-Fetch-Mode` = "cors",
      `Sec-Fetch-Site` = "same-origin"
    ) |>
    req_retry(max_tries = 3) |>
    req_perform()

  page <- rvest::read_html(
    httr2::resp_body_string(
      response
    )
  )

  # Find the specific table cell whose text is exactly
  # "Mokelumne River below WID", then read ONLY its immediate next
  # table cell. This avoids accidentally grabbing a different Cfs value
  # from a larger parent table row.
  target_cell <- html_element(
    page,
    xpath = paste0(
      "//td[normalize-space(.)='",
      target_parameter,
      "']"
    )
  )

  if (is.na(target_cell)) {

    # Save the returned HTML for troubleshooting.
    debug_file <- paste0(
      "EBMUD_debug_",
      gsub("/", "-", date_text, fixed = TRUE),
      ".html"
    )

    writeLines(
      httr2::resp_body_string(response),
      debug_file,
      useBytes = TRUE
    )

    warning(
      "Could not find '",
      target_parameter,
      "' for ",
      date_text,
      ". Returned HTML was saved to ",
      debug_file
    )
    return(NA_real_)
  }

  flow_cell <- html_element(
    target_cell,
    xpath = "following-sibling::td[1]"
  )

  if (is.na(flow_cell)) {
    warning(
      "Found the MOKE label but could not find its adjacent flow cell for ",
      date_text
    )
    return(NA_real_)
  }

  flow_text <- html_text2(
    flow_cell
  )

  flow_text <- trimws(
    gsub(
      "\u00a0",
      " ",
      flow_text,
      fixed = TRUE
    )
  )

  value_text <- sub(
    "^.*?([-+]?[0-9][0-9,.]*).*?$",
    "\\1",
    flow_text
  )

  value <- suppressWarnings(
    as.numeric(
      gsub(
        ",",
        "",
        value_text,
        fixed = TRUE
      )
    )
  )

  if (is.na(value)) {
    warning(
      "Flow value was not numeric for ",
      date_text,
      ": ",
      flow_text
    )
  }

  value
}

# Format an R Date in the same style as the historical CSV:
# "8/30/2026 0:00"
format_csv_time <- function(x) {

  lt <- as.POSIXlt(
    as.Date(x)
  )

  paste0(
    lt$mon + 1,
    "/",
    lt$mday,
    "/",
    lt$year + 1900,
    " 0:00"
  )
}

# Write the output while preserving the original 4 metadata lines.
write_ebmud_csv <- function(metadata, df, path) {

  df <- df[
    order(df$Date),
    ,
    drop = FALSE
  ]

  # Keep only one record per date.
  df <- df[
    !duplicated(
      df$Date,
      fromLast = TRUE
    ),
    ,
    drop = FALSE
  ]

  value_text <- format(
    df$GOLF,
    scientific = FALSE,
    trim = TRUE,
    digits = 15
  )

  output_lines <- c(
    metadata,
    paste0(
      format_csv_time(df$Date),
      ",",
      value_text
    )
  )

  writeLines(
    output_lines,
    path,
    useBytes = TRUE
  )
}

# -------------------------------------------------------------------------
# 4. UPDATE THE FILE
# -------------------------------------------------------------------------

existing <- read_ebmud_csv(
  input_csv
)

moke_data <- existing$data

cat(
  "\nLatest date currently in CSV:",
  format(
    max(moke_data$Date),
    "%m/%d/%Y"
  ),
  "\n"
)

available_dates <- get_available_report_dates()

if (length(available_dates) == 0) {
  stop("No daily report dates were found on the EBMUD page.")
}

cat(
  "Dates currently available on EBMUD website:",
  paste(
    format(
      available_dates,
      "%m/%d/%Y"
    ),
    collapse = ", "
  ),
  "\n"
)

missing_dates <- setdiff(
  available_dates,
  moke_data$Date
)

missing_dates <- as.Date(
  missing_dates,
  origin = "1970-01-01"
)

if (length(missing_dates) == 0) {

  cat(
    "\nNo missing EBMUD dates were found. Nothing needs to be appended.\n"
  )

} else {

  cat(
    "\nMissing dates that will be retrieved:",
    paste(
      format(
        missing_dates,
        "%m/%d/%Y"
      ),
      collapse = ", "
    ),
    "\n\n"
  )

  new_records <- vector(
    "list",
    length(missing_dates)
  )

  for (i in seq_along(missing_dates)) {

    this_date <- missing_dates[i]

    cat(
      "Retrieving ",
      format(
        this_date,
        "%m/%d/%Y"
      ),
      "... ",
      sep = ""
    )

    this_flow <- tryCatch(
      get_moke_value(
        this_date
      ),
      error = function(e) {
        warning(
          "Request failed for ",
          format(
            this_date,
            "%m/%d/%Y"
          ),
          ": ",
          conditionMessage(e)
        )
        NA_real_
      }
    )

    if (is.na(this_flow)) {
      cat("FAILED\n")
    } else {
      cat(
        this_flow,
        " cfs\n",
        sep = ""
      )
    }

    new_records[[i]] <- data.frame(
      Time = format_csv_time(
        this_date
      ),
      GOLF = this_flow,
      Date = this_date,
      stringsAsFactors = FALSE
    )
  }

  new_records <- do.call(
    rbind,
    new_records
  )

  # Append only successfully retrieved records.
  new_records <- new_records[
    !is.na(new_records$GOLF),
    ,
    drop = FALSE
  ]

  if (nrow(new_records) > 0) {

    moke_data <- rbind(
      moke_data,
      new_records
    )

    write_ebmud_csv(
      metadata = existing$metadata,
      df = moke_data,
      path = output_csv
    )

    cat(
      "\nUpdated master MOKE CSV written to:\n",
      normalizePath(
        output_csv,
        winslash = "/",
        mustWork = FALSE
      ),
      "\n",
      sep = ""
    )

  } else {

    cat(
      "\nNo valid new records were retrieved, so no output file was written.\n"
    )
  }
}

# -------------------------------------------------------------------------
# 5. SHOW CURRENT 7-DAY AND 30-DAY AVERAGES
# -------------------------------------------------------------------------

moke_data <- moke_data[
  order(moke_data$Date),
  ,
  drop = FALSE
]

if (nrow(moke_data) >= 7) {

  latest_7 <- tail(
    moke_data,
    7
  )

  cat(
    "\nCurrent 7-day average MOKE flow:",
    round(
      mean(
        latest_7$GOLF,
        na.rm = TRUE
      ),
      1
    ),
    "cfs",
    "\n"
  )
}

if (nrow(moke_data) >= 30) {

  latest_30 <- tail(
    moke_data,
    30
  )

  cat(
    "Current 30-day average MOKE flow:",
    round(
      mean(
        latest_30$GOLF,
        na.rm = TRUE
      ),
      1
    ),
    "cfs",
    "\n"
  )
}

cat(
  "\nDone.\n"
)
