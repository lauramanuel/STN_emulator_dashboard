library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(gt)

app_version <- "1.2.2"

run_page <- function(prefix, title, theme_class) {
  
  is_current <- identical(prefix, "current")
  status_name <- if (is_current) "warning" else "info"
  
  tabItem(
    tabName = paste0("run_", prefix),
    
    div(
      class = theme_class,
      
      fluidRow(
        box(
          width = 12,
          title = paste(title, " Input Method"),
          status = status_name,
          solidHeader = TRUE,
          
          radioButtons(
            paste0(prefix, "_input_method"),
            "Input Method:",
            choices = c(
              "Enter a Single Set of Values" = "single",
              "Upload CSV or Excel File (coming soon)" = "upload",
              "Read from Archive Folder" = "folder"
            ),
            selected = if (is_current) "single" else "folder",
            inline = TRUE
          ),
          
          conditionalPanel(
            condition = sprintf(
              "input.%s_input_method == 'single' && '%s' == 'forecast'",
              prefix,
              prefix
            ),
            tags$div(
              class = "alert alert-info forecast-single-warning",
              tags$b("Forecast runs require Read from Archive Folder."),
              tags$br(),
              "Use Observed Conditions with Enter a Single Set of Values for a user-defined single-value run."
            )
          ),

          conditionalPanel(
            condition = sprintf("input.%s_input_method == 'upload'", prefix),
            tags$div(
              class = "alert alert-info",
              "The upload-file workflow will be implemented in a later phase."
            )
          ),
          
          conditionalPanel(
            condition = sprintf("input.%s_input_method == 'folder'", prefix),
            tags$div(
              class = "archive-control-panel",
              tags$h4("Archive Selection"),
              uiOutput(paste0(prefix, "_archive_date_ui")),
              
              if (!is_current) {
                uiOutput(paste0(prefix, "_archive_scenario_ui"))
              },
              
              tags$p(
                class = "figure-note",
                "Select a run date from the box above."
              )
            )
          )
        )
      ),
      
      conditionalPanel(
        condition = if (is_current) {
          sprintf(
            "input.%s_input_method == 'single' || input.%s_input_method == 'folder'",
            prefix,
            prefix
          )
        } else {
          sprintf(
            "input.%s_input_method == 'folder'",
            prefix
          )
        },
        
        tabsetPanel(
          id = paste0(prefix, "_model_tabs"),
          
          # ==========================================================
          # PTM
          # ==========================================================
          tabPanel(
            "PTM and Event Horizon Emulators",

            fluidRow(
              class = "emulator-top-row",

              box(
                width = 4,
                title = "Emulator Inputs (PTM and Event Horizon)",
                status = status_name,
                solidHeader = TRUE,

                tags$div(
                  class = "alert alert-info",
                  style = "padding:10px 12px; margin-bottom:12px; font-size:13px;",
                  tags$b("Input Averaging"),
                  tags$br(),
                  HTML("&#8226; <b>PTM 7-Day</b> and <b>Event Horizon</b> emulators use <b>7-day average flow</b> inputs."),
                  tags$br(),
                  HTML("&#8226; <b>PTM 30-Day</b> emulator uses <b>30-day average flow</b> inputs.")
                ),

                conditionalPanel(
                  condition = sprintf("input.%s_input_method == 'single'", prefix),

                  textInput(
                    paste0(prefix, "_ptm_name"),
                    "Scenario Name (User Defined):",
                    if (is_current) {
                      "Observed Conditions Run 1"
                    } else {
                      "Forecast Conditions Run 1"
                    }
                  ),

                  if (is_current) {
                    tags$div(
                      class = "alert alert-info observed-flow-intro",
                      "Input flow values are shown below in each box. Boxes are populated with observed conditions when current source data are available."
                    )
                  },

                  numericInput(
                    paste0(prefix, "_ptm_exp"),
                    "EXP: Combined CVP and SWP Export (cfs):",
                    6000
                  ),

                  if (is_current) {
                    uiOutput(paste0(prefix, "_ptm_exp_note"))
                  },

                  numericInput(
                    paste0(prefix, "_ptm_ver"),
                    "VER: San Joaquin River Flow at Vernalis Flow (cfs):",
                    3000
                  ),

                  if (is_current) {
                    uiOutput(paste0(prefix, "_ptm_ver_note"))
                  },

                  tags$div(
                    class = "alert alert-info",
                    style = "padding:8px 10px;margin-bottom:8px;font-size:12px;",
                    tags$b("PTM emulator input only: "),
                    "SAC is used by the PTM emulators and is not used by the Event Horizon emulator."
                  ),

                  numericInput(
                    paste0(prefix, "_ptm_sac"),
                    "SAC: Sacramento River Flow at Freeport Flow (cfs):",
                    18000
                  ),

                  if (is_current) {
                    uiOutput(paste0(prefix, "_ptm_sac_note"))
                  },

                  numericInput(
                    paste0(prefix, "_ptm_east"),
                    "EAST: East-Side River Flow (cfs):",
                    1500
                  ),

                  if (is_current) {
                    uiOutput(paste0(prefix, "_ptm_east_note"))
                  },

                  numericInput(
                    paste0(prefix, "_ptm_xgeo"),
                    "XGEO: Interior Delta Flow (cfs):",
                    3000
                  ),

                  if (is_current) {
                    uiOutput(paste0(prefix, "_ptm_xgeo_note"))
                  }
                ),

                conditionalPanel(
                  condition = sprintf("input.%s_input_method == 'folder'", prefix),

                  textInput(
                    paste0(prefix, "_ptm_archive_name"),
                    "Scenario Name (User Defined):",
                    if (is_current) {
                      "Observed Conditions Run 1"
                    } else {
                      "Forecast Conditions Run 1"
                    }
                  ),

                  uiOutput(paste0(prefix, "_ptm_archive_summary"))
                ),

                conditionalPanel(
                  condition = sprintf(
                    "input.%s_input_method == 'folder' && '%s' == 'forecast'",
                    prefix,
                    prefix
                  ),

                  tags$p(
                    class = "figure-note",
                    style = "margin-top:6px;font-size:12px;",
                    "Note: Forecast calculations hold XGEO constant at the latest observed value."
                  )
                ),

                selectInput(
                  paste0(prefix, "_ptm_threshold"),
                  "Entrainment Risk/Event Horizon Risk Threshold (%):",
                  choices = seq(15, 80, by = 5),
                  selected = 25
                ),

                actionButton(
                  paste0("run_", prefix, "_ptm"),
                  if (is_current) {
                    "Run Observed Conditions PTM and Event Horizon Emulators"
                  } else {
                    "Run Forecast PTM 7-Day and Event Horizon Emulators"
                  },
                  icon = icon("play"),
                  class = "btn-success",
                  width = "100%"
                ),

                br(), br(),

                downloadButton(
                  paste0("download_", prefix, "_ptm"),
                  "Download PTM Emulator Results (CSV)",
                  class = "btn-primary",
                  style = "width:100%;"
                )
              ),

              box(
                width = 8,
                title = "PTM Emulator Results: All Supported Nodes",
                status = status_name,
                solidHeader = TRUE,

                tabsetPanel(
                  id = paste0(prefix, "_ptm_result_tabs"),

                  tabPanel(
                    "7-Day: 15 Nodes",

                    tags$h4("7-Day Entrainment Risk Map"),

                    uiOutput(
                      paste0(prefix, "_eh_summary")
                    ),

                    conditionalPanel(
                      condition = sprintf(
                        "input.%s_input_method == 'folder' && '%s' == 'current'",
                        prefix,
                        prefix
                      ),
                      uiOutput(paste0(prefix, "_ptm7_map_date_ui"))
                    ),

                    tags$p(
                      class = "figure-note",
                      paste(
                        "The map contains PTM emulator results from the 7-day",
                        if (is_current) {
                          "observed conditions"
                        } else {
                          "forecast conditions"
                        },
                        "input values. Results include PTM emulator estimated",
                        "entrainment at each node, a contour polygon that",
                        "interpolates a boundary between nodes based on the",
                        "user defined entrainment risk threshold, and the",
                        "entrainment event horizon. Click a node for details."
                      )
                    ),

                    leafletOutput(
                      paste0(prefix, "_ptm7_map"),
                      height = 500
                    ),

                    downloadButton(
                      paste0("download_", prefix, "_ptm7_map"),
                      "Download Map (PNG)",
                      class = "btn-primary map-download"
                    ),

                    br(), br(),

                    conditionalPanel(
                      condition = sprintf(
                        "input.%s_input_method == 'folder' && '%s' == 'current'",
                        prefix,
                        prefix
                      ),

                      tags$h4(
                        "7-Day Rolling Entrainment Prediction"
                      ),

                      uiOutput(paste0(prefix, "_ptm7_timeseries_nodes_ui")),

                      tags$p(
                        class = "figure-note",
                        paste(
                          "Predictions are shown for seven rolling 7-day",
                          "windows ending on the last seven measured dates."
                        )
                      ),

                      plotlyOutput(
                        paste0(prefix, "_ptm7_timeseries"),
                        height = 600
                      )
                    )
                  ),

                  tabPanel(
                    "30-Day: 39 Nodes",

                    conditionalPanel(
                      condition = sprintf(
                        "input.%s_input_method == 'folder' && '%s' == 'forecast'",
                        prefix,
                        prefix
                      ),

                      tags$div(
                        class = "alert alert-info",
                        paste(
                          "The archive-folder forecast workflow runs only the",
                          "PTM Emulator 7-day model, based on the seven-day forecast average."
                        )
                      )
                    ),

                    conditionalPanel(
                      condition = sprintf(
                        "!(input.%s_input_method == 'folder' && '%s' == 'forecast')",
                        prefix,
                        prefix
                      ),

                      tags$h4("30-Day Entrainment Risk Map"),

                      tags$p(
                        class = "figure-note",
                        paste(
                          "The map contains PTM emulator results from the 30-day",
                          if (is_current) {
                            "observed conditions"
                          } else {
                            "forecast conditions"
                          },
                          "input values. Results include PTM emulator estimated",
                          "entrainment at each node and a contour polygon that",
                          "interpolates a boundary between nodes based on the",
                          "user defined entrainment risk threshold."
                        )
                      ),

                      leafletOutput(
                        paste0(prefix, "_ptm30_map"),
                        height = 500
                      ),

                      downloadButton(
                        paste0("download_", prefix, "_ptm30_map"),
                        "Download Map (PNG)",
                        class = "btn-primary map-download"
                      ),

                      br(), br(),

                      tags$h4(
                        "30-Day Estimated Entrainment Barchart by Locations (DSM2 Node)"
                      ),

                      plotlyOutput(
                        paste0(prefix, "_ptm30_plot"),
                        height = 900
                      ),

                      br(),

                      tags$h4("30-Day Estimated Entrainment Percentage by Location (DSM2 Node)"),

                      div(
                        class = "wide-table-scroll",
                        tableOutput(paste0(prefix, "_ptm30_table"))
                      )
                    )
                  )
                )
              )
            ),

            conditionalPanel(
              condition = sprintf(
                "input.%s_ptm_result_tabs == '7-Day: 15 Nodes'",
                prefix
              ),

              fluidRow(
                box(
                  width = 12,
                  status = status_name,
                  solidHeader = FALSE,
                  class = "combined-emulator-plots-box",

                  fluidRow(
                    column(
                      width = 6,

                      tags$h4(
                        class = "emulator-figure-title",

                        "7-Day Estimated Entrainment Barchart by Locations (DSM2 Node)"
                      ),

                      plotlyOutput(
                        paste0(prefix, "_ptm7_plot"),
                        height = 620
                      )
                    ),

                    column(
                      width = 6,

                      tags$h4(
                        class = "emulator-figure-title",
                        "Event Horizon: River Miles from Clifton Court Forebay"
                      ),
                      tags$p(
                        class = "figure-note",
                        paste(
                          "Historical 7-day average flows (2023-2026) were used to",
                          "generate the event horizons that shown in the background of the",
                          "scatter plot;",
                          "the selected result is highlighted in red."
                        )
                      ),
                      plotlyOutput(
                        paste0(prefix, "_eh_scatter"),
                        height = 620
                      )
                    )
                  )
                )
              ),

              fluidRow(
                box(
                  width = 8,
                  title = "7-Day Estimated Entrainment Percentage by Location (DSM2 Node)",
                  status = status_name,
                  solidHeader = TRUE,

                  tableOutput(
                    paste0(prefix, "_ptm7_table")
                  )
                ),

                box(
                  width = 4,
                  title = "7-Day Event Horizon Distance by Observed Date",
                  status = status_name,
                  solidHeader = TRUE,

                  tableOutput(
                    paste0(prefix, "_eh7_table")
                  )
                )
              )
            )
          ),
          
          # ==========================================================
          # ECO-PTM
          # ==========================================================
          tabPanel(
            "ECO-PTM Emulator",
            
            conditionalPanel(
              condition = sprintf(
                "input.%s_input_method == 'folder' && '%s' == 'forecast'",
                prefix,
                prefix
              ),
              tags$div(
                class = "alert alert-info",
                paste(
                  "ECO-PTM Emulator is not run for archive-folder forecast conditions.",
                  "Use Observed Conditions with Read from Archive Folder."
                )
              )
            ),
            
            conditionalPanel(
              condition = sprintf(
                "!(input.%s_input_method == 'folder' && '%s' == 'forecast')",
                prefix,
                prefix
              ),
              
              fluidRow(
                class = "emulator-top-row",

                box(
                  width = 4,
                  title = "ECO-PTM Emulator Inputs",
                  status = status_name,
                  solidHeader = TRUE,


                  tags$div(
                    class = "alert alert-info",
                    style = "padding:10px 12px; margin-bottom:12px; font-size:13px;",
                    tags$b("Input Averaging"),
                    tags$br(),
                    "ECO-PTM emulator uses 30-day average flow inputs."
                  ),
                  
                  conditionalPanel(
                    condition = sprintf(
                      "input.%s_input_method == 'single'",
                      prefix
                    ),
                    
                    textInput(
                      paste0(prefix, "_eco_name"),
                      "Scenario Name (User Defined):",
                      if (is_current) {
                        "Observed Conditions Run 1"
                      } else {
                        "Forecast Conditions Run 1"
                      }
                    ),

                    tags$div(
                      class = "alert alert-info",
                      style = "padding:10px 12px; margin-bottom:12px; font-size:13px;",
                      paste(
                        "Input flow values are shown in the boxes below.",
                        "The existing values are randomly generated and should",
                        "be replaced with the 30-day average flow at each site."
                      )
                    ),
                    
                    numericInput(
                      paste0(prefix, "_eco_sac"),
                      "SAC: Sacramento River Flow at Freeport Flow (cfs):",
                      18000
                    ),
                    numericInput(
                      paste0(prefix, "_eco_yol"),
                      "YOL: Yolo Bypass Flow (cfs):",
                      1000
                    ),
                    numericInput(
                      paste0(prefix, "_eco_moke"),
                      "MOKE: Mokelumne Flow (cfs):",
                      800
                    ),
                    radioButtons(
                      paste0(prefix, "_eco_dcc"),
                      "Delta Cross Channel Status:",
                      choices = c("Closed" = 0, "Open" = 1),
                      selected = 0,
                      inline = TRUE
                    )
                  ),
                  
                  conditionalPanel(
                    condition = sprintf(
                      "input.%s_input_method == 'folder'",
                      prefix
                    ),
                    
                    textInput(
                      paste0(prefix, "_eco_archive_name"),
                      "Scenario Name (User Defined):",
                      if (is_current) {
                        "Observed Conditions Run 1"
                      } else {
                        "Forecast Conditions Run 1"
                      }
                    ),
                    
                    tags$p(
                      class = "figure-note",
                      paste(
                        "The model inputs are calculated from the latest",
                        "30 measured days. DCC is read from the archive file",
                        "when available; otherwise it defaults to closed (0)."
                      )
                    )
                  ),
                  
                  actionButton(
                    paste0("run_", prefix, "_eco"),
                    "Run ECO-PTM Emulator Models",
                    icon = icon("play"),
                    class = "btn-success",
                    width = "100%"
                  ),
                  
                  br(), br(),
                  
                  downloadButton(
                    paste0("download_", prefix, "_eco"),
                    "Download ECO-PTM Emulator Results (CSV)",
                    class = "btn-primary",
                    style = "width:100%;"
                  )
                ),
                
                box(
                  width = 8,
                  title = "ECO-PTM Emulator Results",
                  status = status_name,
                  solidHeader = TRUE,
                  
                  div(
                    class = "wide-table-scroll",
                    tableOutput(paste0(prefix, "_eco_table"))
                  )
                )
              )
            )
          ),
          

        )
      )
    )
  )
}

ui <- dashboardPage(
  dashboardHeader(
    titleWidth = 300,
    title = tags$div(
      style = "display:flex;align-items:center;",
      tags$img(src = "logo.png", height = "30px", style = "margin-right:10px;"),
      tags$span("*DRAFT Entrainment Dashboard", style = "font-family:Segoe UI Semibold;font-size:16px;")
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Run Observed Conditions", tabName = "run_current", icon = icon("water")),
      menuItem("Run Forecast Conditions", tabName = "run_forecast", icon = icon("cloud-sun")),
      menuItem("Scenario Comparison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Data Access", tabName = "data", icon = icon("database"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-header .logo{width:300px!important;height:60px!important;line-height:60px!important;background:white;color:#0a7e8c;border-bottom:3px solid #0a7e8c}
        .main-header .navbar{min-height:60px;background:white;border-bottom:3px solid #0a7e8c}
        .main-sidebar{width:300px!important;background:#fbfeff;border-right:1px solid #d8edf1}
        .content-wrapper,.right-side{margin-left:300px!important;background:#f9fbfc;padding:18px}
        .sidebar-menu>li>a{color:#0a6270;font-size:14px;font-weight:600}
        .sidebar-menu li:hover>a,.sidebar-menu>li.active>a{background:#eef9fb!important;color:#075f6d!important;border-left:4px solid #0a7e8c}
        .box{border-radius:8px;box-shadow:0 2px 8px rgba(0,0,0,.06);border-top:4px solid #0a7e8c}
        .current-theme{background:#fffaf2;padding:6px;border-radius:8px}
        .current-theme .box{border-top-color:#e69f00}
        .forecast-theme{background:#f7f3fb;padding:6px;border-radius:8px}
        .forecast-theme .box{border-top-color:#7b4fa3}
        .forecast-theme .box.box-info > .box-header{
          background:#7b4fa3 !important;
          color:#ffffff !important;
        }
        .forecast-theme .box.box-info{
          border-top-color:#7b4fa3 !important;
        }
        .forecast-single-warning{
          margin-top:10px;
          margin-bottom:0;
        }
        .observed-flow-note{
          margin-top:-10px;
          margin-bottom:12px;
          padding:7px 9px;
          background:#f7f9fb;
          border-left:3px solid #5b7894;
          color:#3d4c59;
          font-size:12px;
          line-height:1.45;
        }
        .observed-flow-note a{
          font-weight:600;
        }
        /* Align the input and results panels to the height of the taller panel. */
        .emulator-top-row{
          display:flex;
          flex-wrap:wrap;
          align-items:stretch;
        }
        .emulator-top-row > [class*='col-']{
          display:flex;
        }
        .emulator-top-row > [class*='col-'] > .box{
          width:100%;
          height:100%;
          margin-bottom:15px;
        }
        @media (max-width:991px){
          .emulator-top-row{
            display:block;
          }
          .emulator-top-row > [class*='col-']{
            display:block;
          }
        }

        body{font-family:Segoe UI;color:#333}

        .figure-note {
          font-size: 14px;
          color: #4d5b61;
          margin-bottom: 10px;
        }
        .map-download {
          margin-top: 10px;
          margin-bottom: 5px;
        }
        .archive-control-panel {
          padding: 12px 14px;
          margin-top: 10px;
          background: #f4fbfc;
          border: 1px solid #b8dce2;
          border-radius: 6px;
        }

        .archive-control-panel h4 {
          margin-top: 0;
          color: #075f6d;
          font-weight: 700;
        }

        .wide-table-scroll {
          width: 100%;
          overflow-x: auto;
          white-space: nowrap;
        }
        .wide-table-scroll table {
          width: max-content !important;
          min-width: 100%;
          font-size: 14px;
        }
        .leaflet-tooltip.node-permanent-label {
          background: rgba(255,255,255,0.88);
          border: 1px solid #0a7e8c;
          color: #075f6d;
          font-size: 13px;
          font-weight: 700;
          padding: 2px 5px;
          box-shadow: none;
        }
        /* Event Horizon summary */
        .event-horizon-summary {
        margin: 10px 0 16px 0;
        padding: 14px 18px;
        background: #fff7f7;
        border: 1px solid #e1b5b5;
        border-left: 5px solid #b2182b;
        border-radius: 6px;
        }

        .event-horizon-summary-title {
        margin-bottom: 5px;
        color: #333333;
        font-size: 17px;
        font-weight: 700;
        }

        .event-horizon-summary-text {
        color: #333333;
        font-size: 15px;
        line-height: 1.55;
        }

        .event-horizon-highlight {
        color: #8b1e1e;
        font-size: 16px;
        font-weight: 800;
        }
        /* Permanent entrainment labels above map nodes */
        .entrainment-permanent-label {
        padding: 2px 5px !important;
        background: rgba(255, 255, 255, 0.92) !important;
        border: 1px solid #555555 !important;
        border-radius: 10px !important;
        box-shadow: none !important;
        color: #222222 !important;
        font-size: 12px !important;
        font-weight: 700 !important;
        }
        .combined-emulator-plots-box {
        margin-top: 14px;
          }

        .emulator-figure-title {
        margin: 4px 0 12px 0;
        color: #333333;
        font-size: 17px;
        font-weight: 700;
        }
      "))
    ),
    
    tabItems(
      # -----------------------------
      # About
      # -----------------------------
      tabItem(
        tabName = "about",
        div(style = "width: 100%;height: calc(100vh - 100px);overflow-x: auto;overflow-y: auto;box-sizing: border-box;",
            div(style = "width: 1100px;min-width: 1100px;margin: 0 auto;box-sizing: border-box;",
                fluidRow(
                  box(
                    width = 12,
                    height = 300,
                    solidHeader = FALSE,
                    div(
                      style = "display: flex; align-items: center;",
                      tags$img(src = "logo.png", height = "160px"),
                      
                      div(
                        style = "margin-left: 30px;margin-right:50px;",
                        
                        h1("*DRAFT Entrainment Dashboard"),
                        h4("Version:",
                           tags$code(style = "margin-left:20px",
                                     paste0("  ", app_version, "  ")
                           ),
                           tags$a(style = "font-style:italic;margin-left:20px;",
                                  "[Release Notes]",
                                  href = "#release-notes"
                           )
                        ),
                        h4("Date Last Updated:", weight = "bold", 
                           tags$b("2026-07-22")
                        ),
                        
                        
                        tags$hr(),
                        
                        
                        h5(style = "text-align: justify;", "Data Refresh Schedule: Some available datasets will be uploaded weekly provided by our client (DWR? CCWD? USBR?). Other data will be retrieved through API from certain USGS gauges upon request from users within the App. For the specific data information, please go to Chapter XX (link to the chapter) on the Data Access page."),
                        h5("GitHub Application Repository:",
                           tags$a("PTM Emulator Dashboard", style = "font-style: italic;",
                                  href = "https://github.com/lauramanuel/STN_emulator_dashboard")
                        ),
                        
                        h5("Data Sources:", 
                           tags$a("Historical Results for All PTM Emulator, ECO-PTM Emulator, and Event Horizon models", style = "font-style: italic;",
                                  href = "https://github.com/lauramanuel/STN_emulator_dashboard/tree/main/STN_EMULATOR/Output")
                        )
                      )
                    )
                  )
                ),
                fluidRow(
                  box(
                    width = 12,
                    height = 2000,
                    div(style = "margin-left: 60px;margin-right:120px",
                        h2("Overview"),
                        h5(style = "text-align: justify;", "This ShinyApp makes forecast and/or presents hindcast results on the particle entrainment within the Sacramento-San Joaquin Delta. The real-time simulations and predictions are used for providing quick assessment and help with the potential effects of CVP and SWP alternative operations on listed species. This interactive application is designed based on the machine learning models that were originally developed for the Contra Costa Water District (CCWD)???s",
                           tags$a("hydraulic footprint project",
                                  href = "https://github.com/cchang-ccwater/CCWD_Hydraulic_Footprints"),
                           tags$b("."),
                           "Further details on the model development, the original training datasets, and the comparative evaluation of model results are available in the CCWD report ",
                           tags$sup(
                             tags$a(
                               "(1)",
                               href = "#reference",
                               rel = "noopener noreferrer"
                             ),
                             style = "font-size:0.75em;"
                           ),
                           " and other related studies that are currently underway or have been published ",
                           tags$sup(
                             tags$a(
                               "(2, 3)",
                               href = "#reference",
                               rel = "noopener noreferrer"
                             ),
                             style = "font-size:0.75em;"
                           ),
                           "."
                        ),
                        
                        h3("Author & Contact Information"),
                        gt_output("about_info_table"),
                        h3("Technical Guidelines:"),
                        div(style = "margin-left: 60px;",
                            h4("Visual Identity Compliance:"),
                            h5(style = "text-align: justify;", "The application framework is built using the ", 
                               tags$code("shiny"), 
                               "and ", 
                               tags$code("shinydashboard"),
                               "packages. Figures are generated using the", 
                               tags$code("ggplot"),
                               "and ", 
                               tags$code("viridis"),
                               "packages. Additional packages including",
                               tags$code("leaflet"),
                               ", ", 
                               tags$code("sf"),
                               ", ",                        
                               tags$code("lwgeom"),  
                               ", and ", 
                               tags$code("dplyr"),                                 
                               ", are used for interactive mapping, spatial data processing, and geometric calculations. Most interface text uses font sizes between 13px and 16px to ensure good readability under normal viewing conditions. The application primarily uses the Segoe UI font family, with,",
                               tags$b("Regular", style = "font-family: Segoe UI"),
                               tags$b(","),
                               tags$b("Semibold", style = "font-family: Segoe UI Semibold"),
                               tags$b(","),
                               tags$b("Italic", style = "font-style: italic"),
                               "styles applied where appropriate. Additional accessibility features have been implemented or are planned for future releases to further improve accessibility:"
                            ),
                            h5(style = "margin-left: 60px; text-align: justify;","	- Adjustable text size: A text size adjustment option is available, allowing users to change the font size from small to large to improve readability. The default text size is Medium."),
                            p(style = "text-align: center;",
                              tags$span(style = "font-size: 0.83em;margin-right:20px;","Small"),
                              tags$span(style = "font-size: 1.17em;margin-right:20px;","Medium"),
                              tags$span(style = "font-size: 2.00em;","Large")
                            ),
                            h5(style = "margin-left: 60px; text-align: justify;","  - Colorblind-friendly color palette: A color palette option is available, allowing users to switch between different plot color schemes. By default, the application uses the Viridis color palette, which is designed to be perceptually uniform and accessible for users with color vision deficiency. Users may also switch to a high-contrast color palette to enhance visibility."),
                            h4("Browser Compatibility:"),
                            h5(
                              "This app can work on Edge, Chrome, Safari, and Firefox, as tested through version ",
                              tags$code(paste0("v", app_version)),
                              "."
                            ),
                            h4("Performance Standards: "),
                            h5(style = "text-align: justify;","The application provides reasonable load times under normal operating conditions. The ECO-PTM Emulator page typically loads in less than 1 second; the PTM Emulator page in approximately 2-3 seconds; and the Event Horizon page in approximately 7-9 seconds because it loads Leaflet maps, geo-spatial files, and multiple plots. Standard weekly prediction tasks are generally completed almost immediately, while large prediction requests involving long time series and many input features, e.g., 190k records, may require substantially more processing and rendering time."),
                            h5(style = "text-align: justify;","Concurrent-user capacity depends on the deployment environment, including available CPU, memory, and the number of Shiny worker processes. The application is expected to support multiple users performing normal navigation, data exploration, and standard predictions, although several simultaneous computationally intensive prediction requests may increase response times. Final concurrent-user capacity should therefore be confirmed through load testing in the production environment."),
                            h4("Mobile Responsiveness:"),
                            h5("This application is usable also on mobile devices.")
                            
                        ),
                        h3(id = "reference",  "References"),                
                        div(style = "margin-left: 60px;",
                            h5(style = "text-align: justify;", "Contra Costa Water District.(2026) Intuitive Quantitative Metrics for Rapid Assessment of Entrainment Risks and Salmonid Responses in The Delta.",
                               tags$a(
                                 "[Link]",
                                 href = "https://stantec.sharepoint.com/:w:/r/teams/LTOTechnicalSupport2025-2030/Shared%20Documents/CCWD%20Entrainment%20TM/20260419_CCWD_Entrainment_memo.docx?d=wa6ce67b163e341bd9a8e2767a75736e9&csf=1&web=1&e=b9mjqJ"
                               )),
                            h5(style = "text-align: justify;", "Chang, C.-F., et. al., (2026). XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX. Journal of XXXXX: XX-XXXXXXXXXX.",
                               tags$a(
                                 "[Link]",
                                 href = ""
                               )),
                            h5(style = "text-align: justify;", "Chang, C.-F., et. al., (2026). XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX. Journal of XXXXX: XX-XXXXXXXXXX.",
                               tags$a(
                                 "[Link]",
                                 href = ""
                               )),
                        ),
                        h3(id = "release-notes", "Release Notes:"),
                        div(style = "margin-left: 60px;",
                            tags$div(
                              style = "margin-bottom: 24px;padding-left: 12px;border-left: 4px solid #3c8dbc;",
                              h4(style = "margin-bottom: 8px;font-weight: 600;",
                                 "Version ",
                                 tags$code("1.2.0", style = "font-weight: 400;margin-left: 10px;"),
                                 tags$small(style = "margin-left: 10px;color: #777777;font-weight: normal;",
                                            "July 18, 2026"
                                 )
                              ),
                              h5(style = "line-height: 1.6;margin-top: 6px;margin-bottom: 6px;margin-left: 30px;font-weight: normal;",
                                 tags$b("-Added: "),
                                 "A new ",
                                 tags$code("Event Horizon"),
                                 " page with interactive ",
                                 tags$code("leaflet"),
                                 " maps and additional visualization tools."
                              ),
                              h5(style = "line-height: 1.6;margin-top: 6px;margin-bottom: 6px;margin-left: 30px;font-weight: normal;",
                                 tags$b("-Improved: "),
                                 "Prediction performance for standard weekly analysis periods."
                              ),
                              h5(style = "line-height: 1.6;margin-top: 6px;margin-bottom: 6px;margin-left: 30px;font-weight: normal;",
                                 tags$b("-Fixed: "),
                                 "Minor layout and data-loading issues in the ",
                                 tags$code("PTM"),
                                 " module."
                              )
                            )
                        ),
                        div(style = "margin-left: 60px;",
                            tags$div(
                              style = "margin-bottom: 24px;padding-left: 12px;border-left: 4px solid #3c8dbc;",
                              h4(style = "margin-bottom: 8px;font-weight: 600;",
                                 "Version ",
                                 tags$code("1.2.1", style = "font-weight: 400;margin-left: 10px;"),
                                 tags$small(style = "margin-left: 10px;color: #777777;font-weight: normal;",
                                            "July 22, 2026"
                                 )
                              ),
                              h5(style = "line-height: 1.6;margin-top: 6px;margin-bottom: 6px;margin-left: 30px;font-weight: normal;",
                                 tags$b("-Added: "),
                                 "A new ",
                                 tags$code("Event Horizon"),
                                 " page with interactive ",
                                 tags$code("leaflet"),
                                 " maps and additional visualization tools."
                              ),
                              h5(style = "line-height: 1.6;margin-top: 6px;margin-bottom: 6px;margin-left: 30px;font-weight: normal;",
                                 tags$b("-Improved: "),
                                 "Prediction performance for standard weekly analysis periods."
                              ),
                              h5(style = "line-height: 1.6;margin-top: 6px;margin-bottom: 6px;margin-left: 30px;font-weight: normal;",
                                 tags$b("-Fixed: "),
                                 "Minor layout and data-loading issues in the ",
                                 tags$code("PTM"),
                                 " module."
                              )
                            )
                        ),
                        div(style = "margin-left: 60px;",
                            tags$div(
                              style = "margin-bottom: 24px;padding-left: 12px;border-left: 4px solid #3c8dbc;",
                              h4(style = "margin-bottom: 8px;font-weight: 600;",
                                 "Version ",
                                 tags$code("1.2.2", style = "font-weight: 400;margin-left: 10px;"),
                                 tags$small(style = "margin-left: 10px;color: #777777;font-weight: normal;",
                                            "July 24, 2026"
                                 )
                              ),
                              h5(style = "line-height: 1.6;margin-top: 6px;margin-bottom: 6px;margin-left: 30px;font-weight: normal;",
                                 tags$b("-Added: "),
                                 "A new ",
                                 tags$code("Event Horizon"),
                                 " page with interactive ",
                                 tags$code("leaflet"),
                                 " maps and additional visualization tools."
                              ),
                              h5(style = "line-height: 1.6;margin-top: 6px;margin-bottom: 6px;margin-left: 30px;font-weight: normal;",
                                 tags$b("-Improved: "),
                                 "Prediction performance for standard weekly analysis periods."
                              ),
                              h5(style = "line-height: 1.6;margin-top: 6px;margin-bottom: 6px;margin-left: 30px;font-weight: normal;",
                                 tags$b("-Fixed: "),
                                 "Minor layout and data-loading issues in the ",
                                 tags$code("PTM"),
                                 " module."
                              )
                            )
                        )
                    )
                  )
                )
            )
        )
      ),
      
      run_page("current", "Observed Conditions", "current-theme"),
      run_page("forecast", "Forecast Conditions", "forecast-theme"),
      
      tabItem(
        tabName = "comparison",
        tabsetPanel(
          id = "comparison_sections",
          
          tabPanel(
            "General Scenario Comparison",
            fluidRow(
              box(
                width = 4,
                title = "Comparison Controls",
                status = "primary",
                solidHeader = TRUE,
                selectInput(
                  "comparison_model",
                  "Model:",
                  choices = c(
                    "PTM Emulator 7-Day Entrainment",
                    "PTM Emulator 30-Day Entrainment",
                    "ECO-PTM Emulator Survival",
                    "ECO-PTM Emulator Interior Routing",
                    "Event Horizon"
                  ),
                  selectInput(
                    "comparison_map_threshold",
                    "Map Risk Threshold (%):",
                    choices = seq(15, 80, by = 5),
                    selected = 25
                  )
                ),
                uiOutput("comparison_run_selector"),
                downloadButton(
                  "download_comparison",
                  "Download Comparison (CSV)",
                  class = "btn-primary",
                  style = "width:100%;"
                )
              ),
              box(
                width = 8,
                title = "Scenario Comparison",
                status = "primary",
                solidHeader = TRUE,
                tags$p(class = "figure-note", "Compare any compatible model runs saved during the current Shiny session."),
                plotlyOutput("comparison_plot", height = 700),
                br(),
                div(class = "wide-table-scroll", tableOutput("comparison_table"))
              )
            ),
            fluidRow(
              box(
                width = 12,
                title = "Scenario Spatial Comparison Map",
                status = "primary",
                solidHeader = TRUE,
                
                tags$p(
                  class = "figure-note",
                  paste(
                    "This map compares spatial outputs for the selected scenarios.",
                    "For PTM Emulator comparisons, high- and low-risk zones are",
                    "overlaid for each selected scenario. For Event Horizon comparisons,",
                    "the predicted channel-path reaches are overlaid in different colors."
                  )
                ),
                
                leafletOutput(
                  "comparison_spatial_map",
                  height = 650
                )
              )
            )
          ),
          
          tabPanel(
            "OMRI Archive Comparison",
            
            fluidRow(
              box(
                width = 4,
                title = "Archive Comparison Controls",
                status = "primary",
                solidHeader = TRUE,
                
                uiOutput("omri_archive_dates_ui"),
                
                selectInput(
                  "omri_comparison_model",
                  "Forecast Model:",
                  choices = c(
                    "PTM Emulator 7-Day Entrainment",
                    "Event Horizon"
                  ),
                  selected = "PTM Emulator 7-Day Entrainment"
                ),
                
                conditionalPanel(
                  condition = "input.omri_comparison_model == 'Event Horizon'",
                  selectInput(
                    "omri_comparison_risk",
                    "Event Horizon Risk Level (%):",
                    choices = seq(15, 80, by = 5),
                    selected = 25
                  )
                ),
                
                actionButton(
                  "build_omri_comparison",
                  "Run All Available OMRI Scenarios",
                  icon = icon("play"),
                  class = "btn-success",
                  width = "100%"
                ),
                
                br(), br(),
                
                downloadButton(
                  "download_omri_comparison",
                  "Download OMRI Comparison (CSV)",
                  class = "btn-primary",
                  style = "width:100%;"
                )
              ),
              
              box(
                width = 8,
                title = "OMRI Forecast Scenario Comparison",
                status = "primary",
                solidHeader = TRUE,
                
                tags$p(
                  class = "figure-note",
                  paste(
                    "For every selected archive date, the app reads each",
                    "available OMRI CSV, calculates the seven-day forecast",
                    "average, carries forward the latest measured XGEO,",
                    "and compares the forecast emulator results."
                  )
                ),
                
                uiOutput("omri_comparison_status"),
                
                plotlyOutput(
                  "omri_comparison_plot",
                  height = 750
                ),
                
                br(),
                
                div(
                  class = "wide-table-scroll",
                  tableOutput("omri_comparison_table")
                )
              )
            )
          )
        )
      ),
      
      # -----------------------------
      # Data Access
      # -----------------------------
      tabItem(
        tabName = "data",
        div(style = "width: 100%;height: calc(100vh - 100px);overflow-x: auto;overflow-y: auto;box-sizing: border-box;",
            div(style = "width: 1800px;min-width: 1800px;margin: 0 auto;box-sizing: border-box;",        
                fluidRow(
                  box(
                    style = "margin-left: 20px;",
                    width = 6,
                    h1("Data Access", style = "margin-left: 10px;"),
                    solidHeader = FALSE,
                    h3("Quick Overview", style = "margin-left: 10px;"),
                    style = "text-align: Justify;margin-left: 10px;margin-right:60px;",
                    p(style = "margin-left: 10px;", "This ShinyApp makes forecast and/or presents hindcast results on the particle entrainment within the Sacramento-San Joaquin Delta. The real-time simulations and predictions are used for providing quick assessment and help with the potential effects of CVP and SWP alternative operations on listed species. This interactive application is designed based on the machine learning models that were originally developed for the Contra Costa Water District (CCWD)???s hydraulic footprint project."),
                    h5("Here are three types of models:", style = "margin-left: 10px;"),
                    div(style = "margin-left:60px",
                        h5("- DSM2 ECO-PTM emulator models",
                           tags$a(" (part 1)", href = "#intro-eco-ptm")
                        ),
                        h5("- DSM2 PTM emulator models",
                           tags$a(" (part 2)", href = "#intro-ptm")
                        ),
                        h5("- Model for the Entrainment Event Horizon",
                           tags$a(" (part 3)", href = "#intro-event-horizon")
                        )
                    ),
                    h3("Data Availability", style = "margin-left: 10px;"),
                    
                    p(style = "margin-left: 10px;",
                      "Data and tools supporting the PTM Emulator dashboard are provided below."
                    ),
                    
                    tags$hr(),
                    
                    # ---------------------
                    # GitHub
                    # ---------------------
                    div(
                      style = "
          padding:15px;
          border:1px solid #d9d9d9;
          border-radius:6px;
          margin-bottom:15px;
          background:white;
        ",
                      
                      tags$h4("PTM Emulator GitHub Repository"),
                      
                      p(
                        "Access source code, model workflow documentation, emulator development resources, and supporting scripts."
                      ),
                      
                      tags$a(
                        class = "btn btn-success",
                        href = "https://github.com/rojkv/PTM_Emulator_Workflow",
                        target = "_blank",
                        icon("github"),
                        " View on GitHub"
                      )
                    ),
                    
                    # ---------------------
                    # SacPAS
                    # ---------------------
                    div(
                      style = "
          padding:15px;
          border:1px solid #d9d9d9;
          border-radius:6px;
          background:white;
        ",
                      
                      tags$h4("SacPAS Weekly Assessment"),
                      
                      p(
                        "Weekly Sacramento River Winter-Run assessment forecasts and supporting evaluation products."
                      ),
                      
                      tags$a(
                        class = "btn btn-primary",
                        href = "https://can01.safelinks.protection.outlook.com/?url=https%3A%2F%2Fcbr.washington.edu%2Fsacramento%2Fassessments%2Ftest%2Fforecast_sacpas.html&data=05%7C02%7CLaura.Manuel%40stantec.com%7C7d690048e1804ee4cc7408deccc5ac84%7C413c6f2c219a469297d3f2b4d80281e7%7C0%7C0%7C639173346569845423%7CUnknown%7CTWFpbGZsb3d8eyJFbXB0eU1hcGkiOnRydWUsIlYiOiIwLjAuMDAwMCIsIlAiOiJXaW4zMiIsIkFOIjoiTWFpbCIsIldUIjoyfQ%3D%3D%7C0%7C%7C%7C&sdata=OIZCfSzN%2BkO8UrHo3QV4D4Aike6mQpP7LvkmfgoCKa0%3D&reserved=0",
                        target = "_blank",
                        icon("external-link-alt"),
                        " Open SacPAS Assessment"
                      )
                    )
                  ),
                  box(style = "margin-left:20px;",width = 12,solidHeader = FALSE,
                      h3(id = "intro-eco-ptm","P1 DSM2 ECO-PTM emulator models",style = "margin-left:10px;"),
                      gt::gt_output("ecoptm_inputs_table"),
                      tags$hr(),
                      fluidRow(
                        column(width = 7,
                               box(title = "Model Visualization Tool",width = 12,height = "720px",status = "primary",solidHeader = TRUE,
                                   tags$iframe(src = "ECOPTM_path_explorer.html",width = "100%",height = "650px",style = "display:block;border:none;")
                               )
                        ),
                        column(width = 5,
                               box(title = "Model Parameters",width = 12,height = "720px",status = "primary",solidHeader = TRUE,
                                   div(style = "height:650px;overflow-y:auto;",gt::gt_output("ecoptm_parameters_table"))
                               )
                        )
                      )
                  ),
                  box(style = "margin-left:20px;",width = 12,solidHeader = FALSE,
                      h3(id = "intro-ptm","P2 DSM2 PTM emulator models",style = "margin-left:10px;"),
                      gt::gt_output("ptm_inputs_table"),
                      tags$hr(),
                      fluidRow(
                        column(width = 7,
                               box(title = "Model Visualization Tool",width = 12,height = "720px",status = "primary",solidHeader = TRUE,
                                   tags$iframe(src = "PTM_Entrainment_path_explorer.html",width = "100%",height = "650px",style = "display:block;border:none;")
                               )
                        ),
                        column(width = 5,
                               box(title = "Model Parameters",width = 12,height = "720px",status = "primary",solidHeader = TRUE,
                                   div(style = "height:650px;overflow-y:auto;",gt::gt_output("ptm_parameters_table"))
                               )
                        )
                      )
                  ),         
                  box(style = "margin-left:20px;",width = 12,solidHeader = FALSE,
                      h3(id = "intro-event-horizon","P3 Model for the Entrainment Event Horizon",style = "margin-left:10px;"),
                      gt::gt_output("horizon_inputs_table"),
                      tags$hr(),
                      fluidRow(
                        column(width = 6,
                               box(title = "Inputs Data Range",width = 12,height = "420px",status = "primary",solidHeader = TRUE,
                                   div(style = "height:650px;overflow-y:auto;",gt::gt_output("horizon_datarange_table"))
                               )
                        ),
                        column(width = 6,
                               box(title = "Model Parameters",width = 12,height = "420px",status = "primary",solidHeader = TRUE,
                                   div(style = "height:650px;overflow-y:auto;",gt::gt_output("horizon_parameter_table"))
                               )
                        )
                      )
                  ),         
                )
            )
        )
      )
    )
  )
)
=======

library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(gt)

app_version <- "1.2.2"

run_page <- function(prefix, title, theme_class) {
  
  is_current <- identical(prefix, "current")
  status_name <- if (is_current) "warning" else "info"
  
  tabItem(
    tabName = paste0("run_", prefix),
    
    div(
      class = theme_class,
      
      fluidRow(
        box(
          width = 12,
          title = paste(title, " Input Method"),
          status = status_name,
          solidHeader = TRUE,
          
          radioButtons(
            paste0(prefix, "_input_method"),
            "Input Method:",
            choices = c(
              "Enter a Single Set of Values" = "single",
              "Upload CSV or Excel File (coming soon)" = "upload",
              "Read from Archive Folder" = "folder"
            ),
            selected = "single",
            inline = TRUE
          ),
          
          conditionalPanel(
            condition = sprintf("input.%s_input_method == 'upload'", prefix),
            tags$div(
              class = "alert alert-info",
              "The upload-file workflow will be implemented in a later phase."
            )
          ),
          
          conditionalPanel(
            condition = sprintf("input.%s_input_method == 'folder'", prefix),
            tags$div(
              class = "archive-control-panel",
              tags$h4("Archive Selection"),
              uiOutput(paste0(prefix, "_archive_date_ui")),
              
              if (!is_current) {
                uiOutput(paste0(prefix, "_archive_scenario_ui"))
              },
              
              tags$p(
                class = "figure-note",
                if (is_current) {
                  paste(
                    "Observed-condition calculations use measured rows.",
                    "The PTM Emulator 7-day and Event Horizon models use seven rolling",
                    "7-day windows ending on the last seven measured dates.",
                    "The PTM Emulator 30-day and ECO-PTM Emulator models use the most recent",
                    "30-day measured average."
                  )
                } else {
                  paste(
                    "Forecast calculations use the first seven forecast days",
                    "for the selected OMRI scenario. Forecast XGEO is held",
                    "constant at the value from the latest measured date."
                  )
                }
              )
            )
          )
        )
      ),
      
      conditionalPanel(
        condition = sprintf(
          "input.%s_input_method == 'single' || input.%s_input_method == 'folder'",
          prefix,
          prefix
        ),
        
        tabsetPanel(
          id = paste0(prefix, "_model_tabs"),
          
          # ==========================================================
          # PTM
          # ==========================================================
          tabPanel(
            "PTM and Event Horizon Emulators",

            fluidRow(
              box(
                width = 4,
                title = "Emulator Inputs (PTM and Event Horizon)",
                status = status_name,
                solidHeader = TRUE,

                tags$div(
                  class = "alert alert-info",
                  style = "padding:10px 12px; margin-bottom:12px; font-size:13px;",
                  tags$b("Input Averaging"),
                  tags$br(),
                  HTML("&#8226; <b>PTM 7-Day</b> and <b>Event Horizon</b> emulators use <b>7-day average flow</b> inputs."),
                  tags$br(),
                  HTML("&#8226; <b>PTM 30-Day</b> emulator uses <b>30-day average flow</b> inputs.")
                ),

                conditionalPanel(
                  condition = sprintf("input.%s_input_method == 'single'", prefix),

                  textInput(
                    paste0(prefix, "_ptm_name"),
                    "Scenario Name (User Defined):",
                    paste(title, "PTM Run Single Values")
                  ),

                  numericInput(
                    paste0(prefix, "_ptm_exp"),
                    "EXP: Combined CVP and SWP Export (cfs):",
                    6000
                  ),

                  numericInput(
                    paste0(prefix, "_ptm_ver"),
                    "VER: San Joaquin River Flow at Vernalis Flow (cfs):",
                    3000
                  ),

                  tags$div(
                    class = "alert alert-info",
                    style = "padding:8px 10px;margin-bottom:8px;font-size:12px;",
                    tags$b("PTM emulator input only: "),
                    "SAC is used by the PTM emulators and is not used by the Event Horizon emulator."
                  ),

                  numericInput(
                    paste0(prefix, "_ptm_sac"),
                    "SAC: Sacramento River Flow at Freeport Flow (cfs):",
                    18000
                  ),

                  numericInput(
                    paste0(prefix, "_ptm_east"),
                    "EAST: East-Side River Flow (cfs):",
                    1500
                  ),

                  numericInput(
                    paste0(prefix, "_ptm_xgeo"),
                    "XGEO: Interior Delta Flow (cfs):",
                    3000
                  )
                ),

                conditionalPanel(
                  condition = sprintf("input.%s_input_method == 'folder'", prefix),

                  textInput(
                    paste0(prefix, "_ptm_archive_name"),
                    "Scenario Name (Input Method: Archive Folder):",
                    paste(title, "PTM Emulator Run: Archive Folder")
                  ),

                  uiOutput(paste0(prefix, "_ptm_archive_summary"))
                ),

                conditionalPanel(
                  condition = sprintf(
                    "input.%s_input_method == 'folder' && '%s' == 'forecast'",
                    prefix,
                    prefix
                  ),

                  tags$p(
                    class = "figure-note",
                    style = "margin-top:6px;font-size:12px;",
                    "Note: Forecast calculations hold XGEO constant at the latest observed value."
                  )
                ),

                selectInput(
                  paste0(prefix, "_ptm_threshold"),
                  "Entrainment Risk/Event Horizon Risk Threshold (%):",
                  choices = seq(15, 80, by = 5),
                  selected = 25
                ),

                actionButton(
                  paste0("run_", prefix, "_ptm"),
                  if (is_current) {
                    "Run Observed Conditions PTM and Event Horizon Emulators"
                  } else {
                    "Run Forecast PTM 7-Day and Event Horizon Emulators"
                  },
                  icon = icon("play"),
                  class = "btn-success",
                  width = "100%"
                ),

                br(), br(),

                downloadButton(
                  paste0("download_", prefix, "_ptm"),
                  "Download PTM Emulator Results (CSV)",
                  class = "btn-primary",
                  style = "width:100%;"
                )
              ),

              box(
                width = 8,
                title = "PTM Emulator Results: All Supported Nodes",
                status = status_name,
                solidHeader = TRUE,

                tabsetPanel(
                  id = paste0(prefix, "_ptm_result_tabs"),

                  tabPanel(
                    "7-Day: 15 Nodes",

                    tags$h4("PTM 7-Day Entrainment and Event Horizon Map"),

                    uiOutput(
                      paste0(prefix, "_eh_summary")
                    ),

                    conditionalPanel(
                      condition = sprintf(
                        "input.%s_input_method == 'folder' && '%s' == 'current'",
                        prefix,
                        prefix
                      ),
                      uiOutput(paste0(prefix, "_ptm7_map_date_ui"))
                    ),

                    tags$p(
                      class = "figure-note",
                      paste(
                        "The archive-current map can be stepped through the",
                        "seven rolling windows. Click a node for details.",
                        "Risk-zone polygons are an approximation based on emulator node availability."
                      )
                    ),

                    leafletOutput(
                      paste0(prefix, "_ptm7_map"),
                      height = 500
                    ),

                    downloadButton(
                      paste0("download_", prefix, "_ptm7_map"),
                      "Download Map (PNG)",
                      class = "btn-primary map-download"
                    ),

                    br(), br(),

                    conditionalPanel(
                      condition = sprintf(
                        "input.%s_input_method == 'folder' && '%s' == 'current'",
                        prefix,
                        prefix
                      ),

                      tags$h4(
                        "PTM Emulator 7-Day Rolling Prediction Time Series"
                      ),

                      uiOutput(paste0(prefix, "_ptm7_timeseries_nodes_ui")),

                      tags$p(
                        class = "figure-note",
                        paste(
                          "Predictions are shown for seven rolling 7-day",
                          "windows ending on the last seven measured dates."
                        )
                      ),

                      plotlyOutput(
                        paste0(prefix, "_ptm7_timeseries"),
                        height = 600
                      )
                    )
                  ),

                  tabPanel(
                    "30-Day: 39 Nodes",

                    conditionalPanel(
                      condition = sprintf(
                        "input.%s_input_method == 'folder' && '%s' == 'forecast'",
                        prefix,
                        prefix
                      ),

                      tags$div(
                        class = "alert alert-info",
                        paste(
                          "The archive-folder forecast workflow runs only the",
                          "PTM Emulator 7-day model, based on the seven-day forecast average."
                        )
                      )
                    ),

                    conditionalPanel(
                      condition = sprintf(
                        "!(input.%s_input_method == 'folder' && '%s' == 'forecast')",
                        prefix,
                        prefix
                      ),

                      tags$h4("PTM Emulator 30-Day Entrainment Risk Map"),

                      tags$p(
                        class = "figure-note",
                        paste(
                          "Archive-current results use the most recent",
                          "30 measured days."
                        )
                      ),

                      leafletOutput(
                        paste0(prefix, "_ptm30_map"),
                        height = 500
                      ),

                      downloadButton(
                        paste0("download_", prefix, "_ptm30_map"),
                        "Download Map (PNG)",
                        class = "btn-primary map-download"
                      ),

                      br(), br(),

                      tags$h4(
                        "PTM Emulator 30-Day Entrainment by DSM2 Node"
                      ),

                      plotlyOutput(
                        paste0(prefix, "_ptm30_plot"),
                        height = 900
                      ),

                      br(),

                      tags$h4("PTM Emulator 30-Day Node Results"),

                      div(
                        class = "wide-table-scroll",
                        tableOutput(paste0(prefix, "_ptm30_table"))
                      )
                    )
                  )
                )
              )
            ),

            conditionalPanel(
              condition = sprintf(
                "input.%s_ptm_result_tabs == '7-Day: 15 Nodes'",
                prefix
              ),

              fluidRow(
                box(
                  width = 12,
                  status = status_name,
                  solidHeader = FALSE,
                  class = "combined-emulator-plots-box",

                  fluidRow(
                    column(
                      width = 6,

                      tags$h4(
                        class = "emulator-figure-title",

                        if (is_current) {
                          "Observed PTM 7-Day Entrainment by DSM2 Node"
                        } else {
                          "Forecast PTM 7-Day Entrainment by DSM2 Node"
                        }
                      ),

                      plotlyOutput(
                        paste0(prefix, "_ptm7_plot"),
                        height = 620
                      )
                    ),

                    column(
                      width = 6,

                      tags$h4(
                        class = "emulator-figure-title",
                        "Event Horizon: River Miles from Clifton Court Forebay"
                      ),
                      tags$p(
                        class = "figure-note",
                        paste(
                          "Historical 7-day average flows (2023-2026) were used to",
                          "generate the event horizons that shown in the background of the",
                          "scatter plot;",
                          "the selected result is highlighted in red."
                        )
                      ),
                      plotlyOutput(
                        paste0(prefix, "_eh_scatter"),
                        height = 620
                      )
                    )
                  )
                )
              ),

              fluidRow(
                box(
                  width = 12,
                  title = "PTM Emulator 7-Day Node Results",
                  status = status_name,
                  solidHeader = TRUE,

                  tableOutput(
                    paste0(prefix, "_ptm7_table")
                  )
                )
              )
            )
          ),
          
          # ==========================================================
          # ECO-PTM
          # ==========================================================
          tabPanel(
            "ECO-PTM Emulator",
            
            conditionalPanel(
              condition = sprintf(
                "input.%s_input_method == 'folder' && '%s' == 'forecast'",
                prefix,
                prefix
              ),
              tags$div(
                class = "alert alert-info",
                paste(
                  "ECO-PTM Emulator is not run for archive-folder forecast conditions.",
                  "Use Observed Conditions with Read from Archive Folder."
                )
              )
            ),
            
            conditionalPanel(
              condition = sprintf(
                "!(input.%s_input_method == 'folder' && '%s' == 'forecast')",
                prefix,
                prefix
              ),
              
              fluidRow(
                box(
                  width = 4,
                  title = "ECO-PTM Emulator Inputs (30-day average flows)",
                  status = status_name,
                  solidHeader = TRUE,
                  
                  conditionalPanel(
                    condition = sprintf(
                      "input.%s_input_method == 'single'",
                      prefix
                    ),
                    
                    textInput(
                      paste0(prefix, "_eco_name"),
                      "Scenario Name (User Defined):",
                      paste(title, "ECO-PTM Emulator Run: Single Values")
                    ),
                    
                    numericInput(
                      paste0(prefix, "_eco_sac"),
                      "SAC: Freeport Flow (cfs):",
                      18000
                    ),
                    numericInput(
                      paste0(prefix, "_eco_yol"),
                      "YOL: Yolo Bypass Flow (cfs):",
                      1000
                    ),
                    numericInput(
                      paste0(prefix, "_eco_moke"),
                      "MOKE: Mokelumne Flow (cfs):",
                      800
                    ),
                    radioButtons(
                      paste0(prefix, "_eco_dcc"),
                      "Delta Cross Channel Status:",
                      choices = c("Closed" = 0, "Open" = 1),
                      selected = 0,
                      inline = TRUE
                    )
                  ),
                  
                  conditionalPanel(
                    condition = sprintf(
                      "input.%s_input_method == 'folder'",
                      prefix
                    ),
                    
                    textInput(
                      paste0(prefix, "_eco_archive_name"),
                      "Scenario Name (Input Method: Archive Folder):",
                      paste(title, "ECO-PTM Emulator Run: Archive Folder")
                    ),
                    
                    tags$p(
                      class = "figure-note",
                      paste(
                        "The model inputs are calculated from the latest",
                        "30 measured days. DCC is read from the archive file",
                        "when available; otherwise it defaults to closed (0)."
                      )
                    )
                  ),
                  
                  actionButton(
                    paste0("run_", prefix, "_eco"),
                    "Run ECO-PTM Emulator Models",
                    icon = icon("play"),
                    class = "btn-success",
                    width = "100%"
                  ),
                  
                  br(), br(),
                  
                  downloadButton(
                    paste0("download_", prefix, "_eco"),
                    "Download ECO-PTM Emulator Results (CSV)",
                    class = "btn-primary",
                    style = "width:100%;"
                  )
                ),
                
                box(
                  width = 8,
                  title = "ECO-PTM Emulator Results",
                  status = status_name,
                  solidHeader = TRUE,
                  
                  div(
                    class = "wide-table-scroll",
                    tableOutput(paste0(prefix, "_eco_table"))
                  )
                )
              )
            )
          ),
          

        )
      )
    )
  )
}

ui <- dashboardPage(
  dashboardHeader(
    titleWidth = 300,
    title = tags$div(
      style = "display:flex;align-items:center;",
      tags$img(src = "logo.png", height = "30px", style = "margin-right:10px;"),
      tags$span("*DRAFT Entrainment Dashboard", style = "font-family:Segoe UI Semibold;font-size:16px;")
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Run Observed Conditions", tabName = "run_current", icon = icon("water")),
      menuItem("Run Forecast Conditions", tabName = "run_forecast", icon = icon("cloud-sun")),
      menuItem("Scenario Comparison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Data Access", tabName = "data", icon = icon("database"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-header .logo{width:300px!important;height:60px!important;line-height:60px!important;background:white;color:#0a7e8c;border-bottom:3px solid #0a7e8c}
        .main-header .navbar{min-height:60px;background:white;border-bottom:3px solid #0a7e8c}
        .main-sidebar{width:300px!important;background:#fbfeff;border-right:1px solid #d8edf1}
        .content-wrapper,.right-side{margin-left:300px!important;background:#f9fbfc;padding:18px}
        .sidebar-menu>li>a{color:#0a6270;font-size:14px;font-weight:600}
        .sidebar-menu li:hover>a,.sidebar-menu>li.active>a{background:#eef9fb!important;color:#075f6d!important;border-left:4px solid #0a7e8c}
        .box{border-radius:8px;box-shadow:0 2px 8px rgba(0,0,0,.06);border-top:4px solid #0a7e8c}
        .current-theme{background:#fffaf2;padding:6px;border-radius:8px}
        .current-theme .box{border-top-color:#e69f00}
        .forecast-theme{background:#f4fbff;padding:6px;border-radius:8px}
        .forecast-theme .box{border-top-color:#56b4e9}
        body{font-family:Segoe UI;color:#333}

        .figure-note {
          font-size: 14px;
          color: #4d5b61;
          margin-bottom: 10px;
        }
        .map-download {
          margin-top: 10px;
          margin-bottom: 5px;
        }
        .archive-control-panel {
          padding: 12px 14px;
          margin-top: 10px;
          background: #f4fbfc;
          border: 1px solid #b8dce2;
          border-radius: 6px;
        }

        .archive-control-panel h4 {
          margin-top: 0;
          color: #075f6d;
          font-weight: 700;
        }

        .wide-table-scroll {
          width: 100%;
          overflow-x: auto;
          white-space: nowrap;
        }
        .wide-table-scroll table {
          width: max-content !important;
          min-width: 100%;
          font-size: 14px;
        }
        .leaflet-tooltip.node-permanent-label {
          background: rgba(255,255,255,0.88);
          border: 1px solid #0a7e8c;
          color: #075f6d;
          font-size: 13px;
          font-weight: 700;
          padding: 2px 5px;
          box-shadow: none;
        }
        /* Event Horizon summary */
        .event-horizon-summary {
        margin: 10px 0 16px 0;
        padding: 14px 18px;
        background: #fff7f7;
        border: 1px solid #e1b5b5;
        border-left: 5px solid #b2182b;
        border-radius: 6px;
        }

        .event-horizon-summary-title {
        margin-bottom: 5px;
        color: #333333;
        font-size: 17px;
        font-weight: 700;
        }

        .event-horizon-summary-text {
        color: #333333;
        font-size: 15px;
        line-height: 1.55;
        }

        .event-horizon-highlight {
        color: #8b1e1e;
        font-size: 16px;
        font-weight: 800;
        }
        /* Permanent entrainment labels above map nodes */
        .entrainment-permanent-label {
        padding: 2px 5px !important;
        background: rgba(255, 255, 255, 0.92) !important;
        border: 1px solid #555555 !important;
        border-radius: 10px !important;
        box-shadow: none !important;
        color: #222222 !important;
        font-size: 12px !important;
        font-weight: 700 !important;
        }
        .combined-emulator-plots-box {
        margin-top: 14px;
          }

        .emulator-figure-title {
        margin: 4px 0 12px 0;
        color: #333333;
        font-size: 17px;
        font-weight: 700;
        }
      "))
    ),
    
    tabItems(
      # -----------------------------
      # About
      # -----------------------------
      tabItem(
        tabName = "about",
        div(style = "width: 100%;height: calc(100vh - 100px);overflow-x: auto;overflow-y: auto;box-sizing: border-box;",
            div(style = "width: 1100px;min-width: 1100px;margin: 0 auto;box-sizing: border-box;",
                fluidRow(
                  box(
                    width = 12,
                    height = 300,
                    solidHeader = FALSE,
                    div(
                      style = "display: flex; align-items: center;",
                      tags$img(src = "logo.png", height = "160px"),
                      
                      div(
                        style = "margin-left: 30px;margin-right:50px;",
                        
                        h1("*DRAFT Entrainment Dashboard"),
                        h4("Version:",
                           tags$code(style = "margin-left:20px",
                                     paste0("  ", app_version, "  ")
                           ),
                           tags$a(style = "font-style:italic;margin-left:20px;",
                                  "[Release Notes]",
                                  href = "#release-notes"
                           )
                        ),
                        h4("Date Last Updated:", weight = "bold", 
                           tags$b("2026-07-22")
                        ),
                        
                        
                        tags$hr(),
                        
                        
                        h5(style = "text-align: justify;", "Data Refresh Schedule: Some available datasets will be uploaded weekly provided by our client (DWR? CCWD? USBR?). Other data will be retrieved through API from certain USGS gauges upon request from users within the App. For the specific data information, please go to Data Access page."),
                        h5("GitHub Application Repository:",
                           tags$a("PTM Emulator Dashboard", style = "font-style: italic;",
                                  href = "https://github.com/lauramanuel/STN_emulator_dashboard")
                        ),
                        
                        h5("Data Sources:", 
                           tags$a("Historical Results for All PTM Emulator, ECO-PTM Emulator, and Event Horizon models", style = "font-style: italic;",
                                  href = "https://github.com/lauramanuel/STN_emulator_dashboard/tree/main/STN_EMULATOR/Output")
                        )
                      )
                    )
                  )
                ),
                fluidRow(
                  box(
                    width = 12,
                    height = 2000,
                    div(style = "margin-left: 60px;margin-right:120px",
                        h2("Overview"),
                        h5(style = "text-align: justify;", "This ShinyApp makes forecast and/or presents hindcast results on the particle entrainment within the Sacramento-San Joaquin Delta. The real-time simulations and predictions are used for providing quick assessment and help with the potential effects of CVP and SWP alternative operations on listed species. This interactive application is designed based on the machine learning models that were originally developed for the Contra Costa Water District (CCWD)'s",
                           tags$a("hydraulic footprint project",
                                  href = "https://github.com/cchang-ccwater/CCWD_Hydraulic_Footprints"),
                           tags$b("."),
                           "Further details on the model development, the original training datasets, and the comparative evaluation of model results are available in the CCWD report ",
                           tags$sup(
                             tags$a(
                               "(1)",
                               href = "#reference",
                               rel = "noopener noreferrer"
                             ),
                             style = "font-size:0.75em;"
                           ),
                           " and other related studies that are currently underway or have been published ",
                           tags$sup(
                             tags$a(
                               "(2, 3)",
                               href = "#reference",
                               rel = "noopener noreferrer"
                             ),
                             style = "font-size:0.75em;"
                           ),
                           "."
                        ),
                        
                        h3("Author & Contact Information"),
                        gt_output("about_info_table"),
                        h3("Technical Guidelines:"),
                        div(style = "margin-left: 60px;",
                            h4("Visual Identity Compliance:"),
                            h5(style = "text-align: justify;", "The application framework is built using the ", 
                               tags$code("shiny"), 
                               "and ", 
                               tags$code("shinydashboard"),
                               "packages. Figures are generated using the", 
                               tags$code("ggplot"),
                               "and ", 
                               tags$code("viridis"),
                               "packages. Additional packages including",
                               tags$code("leaflet"),
                               ", ", 
                               tags$code("sf"),
                               ", ",                        
                               tags$code("lwgeom"),  
                               ", and ", 
                               tags$code("dplyr"),                                 
                               ", are used for interactive mapping, spatial data processing, and geometric calculations. Most interface text uses font sizes between 13px and 16px to ensure good readability under normal viewing conditions. The application primarily uses the Segoe UI font family, with,",
                               tags$b("Regular", style = "font-family: Segoe UI"),
                               tags$b(","),
                               tags$b("Semibold", style = "font-family: Segoe UI Semibold"),
                               tags$b(","),
                               tags$b("Italic", style = "font-style: italic"),
                               "styles applied where appropriate. Additional accessibility features have been implemented or are planned for future releases to further improve accessibility:"
                            ),
                            h5(style = "margin-left: 60px; text-align: justify;","	- Adjustable text size: A text size adjustment option is available, allowing users to change the font size from small to large to improve readability. The default text size is Medium."),
                            p(style = "text-align: center;",
                              tags$span(style = "font-size: 0.83em;margin-right:20px;","Small"),
                              tags$span(style = "font-size: 1.17em;margin-right:20px;","Medium"),
                              tags$span(style = "font-size: 2.00em;","Large")
                            ),
                            h5(style = "margin-left: 60px; text-align: justify;","  - Colorblind-friendly color palette: A color palette option is available, allowing users to switch between different plot color schemes. By default, the application uses the Viridis color palette, which is designed to be perceptually uniform and accessible for users with color vision deficiency. Users may also switch to a high-contrast color palette to enhance visibility."),
                            h4("Browser Compatibility:"),
                            h5(
                              "This app can work on Edge, Chrome, Safari, and Firefox, as tested through version ",
                              tags$code(paste0("v", app_version)),
                              "."
                            ),
                            h4("Performance Standards: "),
                            h5(style = "text-align: justify;","The application provides reasonable load times under normal operating conditions. The ECO-PTM Emulator page typically loads in less than 1 second; the PTM Emulator page in approximately 2-3 seconds; and the Event Horizon page in approximately 7-9 seconds because it loads Leaflet maps, geo-spatial files, and multiple plots. Standard weekly prediction tasks are generally completed almost immediately, while large prediction requests involving long time series and many input features, e.g., 190k records, may require substantially more processing and rendering time."),
                            h5(style = "text-align: justify;","Concurrent-user capacity depends on the deployment environment, including available CPU, memory, and the number of Shiny worker processes. The application is expected to support multiple users performing normal navigation, data exploration, and standard predictions, although several simultaneous computationally intensive prediction requests may increase response times. Final concurrent-user capacity should therefore be confirmed through load testing in the production environment."),
                            h4("Mobile Responsiveness:"),
                            h5("This application is usable also on mobile devices.")
                            
                        ),
                        h3(id = "reference",  "References"),                
                        div(style = "margin-left: 60px;",
                            h5(style = "text-align: justify;", "Contra Costa Water District.(2026) Intuitive Quantitative Metrics for Rapid Assessment of Entrainment Risks and Salmonid Responses in The Delta.",
                               tags$a(
                                 "[Link]",
                                 href = "https://stantec.sharepoint.com/:w:/r/teams/LTOTechnicalSupport2025-2030/Shared%20Documents/CCWD%20Entrainment%20TM/20260419_CCWD_Entrainment_memo.docx?d=wa6ce67b163e341bd9a8e2767a75736e9&csf=1&web=1&e=b9mjqJ"
                               )),
                            h5(style = "text-align: justify;", "Chang, C.-F., et. al., (2026). XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX. Journal of XXXXX: XX-XXXXXXXXXX.",
                               tags$a(
                                 "[Link]",
                                 href = ""
                               )),
                            h5(style = "text-align: justify;", "Chang, C.-F., et. al., (2026). XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX. Journal of XXXXX: XX-XXXXXXXXXX.",
                               tags$a(
                                 "[Link]",
                                 href = ""
                               )),
                        ),
                        h3(id = "release-notes", "Release Notes:"),
                        div(style = "margin-left: 60px;",
                            tags$div(
                              style = "margin-bottom: 24px;padding-left: 12px;border-left: 4px solid #3c8dbc;",
                              h4(style = "margin-bottom: 8px;font-weight: 600;",
                                 "Version ",
                                 tags$code("1.2.0", style = "font-weight: 400;margin-left: 10px;"),
                                 tags$small(style = "margin-left: 10px;color: #777777;font-weight: normal;",
                                            "July 18, 2026"
                                 )
                              ),
                              h5(style = "line-height: 1.6;margin-top: 6px;margin-bottom: 6px;margin-left: 30px;font-weight: normal;",
                                 tags$b("-Added: "),
                                 "A new ",
                                 tags$code("Event Horizon"),
                                 " page with interactive ",
                                 tags$code("leaflet"),
                                 " maps and additional visualization tools."
                              ),
                              h5(style = "line-height: 1.6;margin-top: 6px;margin-bottom: 6px;margin-left: 30px;font-weight: normal;",
                                 tags$b("-Improved: "),
                                 "Prediction performance for standard weekly analysis periods."
                              ),
                              h5(style = "line-height: 1.6;margin-top: 6px;margin-bottom: 6px;margin-left: 30px;font-weight: normal;",
                                 tags$b("-Fixed: "),
                                 "Minor layout and data-loading issues in the ",
                                 tags$code("PTM"),
                                 " module."
                              )
                            )
                        ),
                        div(style = "margin-left: 60px;",
                            tags$div(
                              style = "margin-bottom: 24px;padding-left: 12px;border-left: 4px solid #3c8dbc;",
                              h4(style = "margin-bottom: 8px;font-weight: 600;",
                                 "Version ",
                                 tags$code("1.2.1", style = "font-weight: 400;margin-left: 10px;"),
                                 tags$small(style = "margin-left: 10px;color: #777777;font-weight: normal;",
                                            "July 22, 2026"
                                 )
                              ),
                              h5(style = "line-height: 1.6;margin-top: 6px;margin-bottom: 6px;margin-left: 30px;font-weight: normal;",
                                 tags$b("-Added: "),
                                 "A new ",
                                 tags$code("Event Horizon"),
                                 " page with interactive ",
                                 tags$code("leaflet"),
                                 " maps and additional visualization tools."
                              ),
                              h5(style = "line-height: 1.6;margin-top: 6px;margin-bottom: 6px;margin-left: 30px;font-weight: normal;",
                                 tags$b("-Improved: "),
                                 "Prediction performance for standard weekly analysis periods."
                              ),
                              h5(style = "line-height: 1.6;margin-top: 6px;margin-bottom: 6px;margin-left: 30px;font-weight: normal;",
                                 tags$b("-Fixed: "),
                                 "Minor layout and data-loading issues in the ",
                                 tags$code("PTM"),
                                 " module."
                              )
                            )
                        ),
                        div(style = "margin-left: 60px;",
                            tags$div(
                              style = "margin-bottom: 24px;padding-left: 12px;border-left: 4px solid #3c8dbc;",
                              h4(style = "margin-bottom: 8px;font-weight: 600;",
                                 "Version ",
                                 tags$code("1.2.2", style = "font-weight: 400;margin-left: 10px;"),
                                 tags$small(style = "margin-left: 10px;color: #777777;font-weight: normal;",
                                            "July 24, 2026"
                                 )
                              ),
                              h5(style = "line-height: 1.6;margin-top: 6px;margin-bottom: 6px;margin-left: 30px;font-weight: normal;",
                                 tags$b("-Added: "),
                                 "A new ",
                                 tags$code("Event Horizon"),
                                 " page with interactive ",
                                 tags$code("leaflet"),
                                 " maps and additional visualization tools."
                              ),
                              h5(style = "line-height: 1.6;margin-top: 6px;margin-bottom: 6px;margin-left: 30px;font-weight: normal;",
                                 tags$b("-Improved: "),
                                 "Prediction performance for standard weekly analysis periods."
                              ),
                              h5(style = "line-height: 1.6;margin-top: 6px;margin-bottom: 6px;margin-left: 30px;font-weight: normal;",
                                 tags$b("-Fixed: "),
                                 "Minor layout and data-loading issues in the ",
                                 tags$code("PTM"),
                                 " module."
                              )
                            )
                        )
                    )
                  )
                )
            )
        )
      ),
      
      run_page("current", "Observed Conditions", "current-theme"),
      run_page("forecast", "Forecast Conditions", "forecast-theme"),
      
      tabItem(
        tabName = "comparison",
        tabsetPanel(
          id = "comparison_sections",
          
          tabPanel(
            "General Scenario Comparison",
            fluidRow(
              box(
                width = 4,
                title = "Comparison Controls",
                status = "primary",
                solidHeader = TRUE,
                selectInput(
                  "comparison_model",
                  "Model:",
                  choices = c(
                    "PTM Emulator 7-Day Entrainment",
                    "PTM Emulator 30-Day Entrainment",
                    "ECO-PTM Emulator Survival",
                    "ECO-PTM Emulator Interior Routing",
                    "Event Horizon"
                  )
                ),
                uiOutput("comparison_run_selector"),
                downloadButton(
                  "download_comparison",
                  "Download Comparison (CSV)",
                  class = "btn-primary",
                  style = "width:100%;"
                )
              ),
              box(
                width = 8,
                title = "Scenario Comparison",
                status = "primary",
                solidHeader = TRUE,
                tags$p(class = "figure-note", "Compare any compatible model runs saved during the current Shiny session."),
                plotlyOutput("comparison_plot", height = 700),
                br(),
                div(class = "wide-table-scroll", tableOutput("comparison_table"))
              )
            )
          ),
          
          tabPanel(
            "OMRI Archive Comparison",
            
            fluidRow(
              box(
                width = 4,
                title = "Archive Comparison Controls",
                status = "primary",
                solidHeader = TRUE,
                
                uiOutput("omri_archive_dates_ui"),
                
                selectInput(
                  "omri_comparison_model",
                  "Forecast Model:",
                  choices = c(
                    "PTM Emulator 7-Day Entrainment",
                    "Event Horizon"
                  ),
                  selected = "PTM Emulator 7-Day Entrainment"
                ),
                
                conditionalPanel(
                  condition = "input.omri_comparison_model == 'Event Horizon'",
                  selectInput(
                    "omri_comparison_risk",
                    "Event Horizon Risk Level (%):",
                    choices = seq(15, 80, by = 5),
                    selected = 25
                  )
                ),
                
                actionButton(
                  "build_omri_comparison",
                  "Run All Available OMRI Scenarios",
                  icon = icon("play"),
                  class = "btn-success",
                  width = "100%"
                ),
                
                br(), br(),
                
                downloadButton(
                  "download_omri_comparison",
                  "Download OMRI Comparison (CSV)",
                  class = "btn-primary",
                  style = "width:100%;"
                )
              ),
              
              box(
                width = 8,
                title = "OMRI Forecast Scenario Comparison",
                status = "primary",
                solidHeader = TRUE,
                
                tags$p(
                  class = "figure-note",
                  paste(
                    "For every selected archive date, the app reads each",
                    "available OMRI CSV, calculates the seven-day forecast",
                    "average, carries forward the latest measured XGEO,",
                    "and compares the forecast emulator results."
                  )
                ),
                
                uiOutput("omri_comparison_status"),
                
                plotlyOutput(
                  "omri_comparison_plot",
                  height = 750
                ),
                
                br(),
                
                div(
                  class = "wide-table-scroll",
                  tableOutput("omri_comparison_table")
                )
              )
            )
          )
        )
      ),
      
      # -----------------------------
      # Data Access
      # -----------------------------
      tabItem(
        tabName = "data",
        div(style = "width: 100%;height: calc(100vh - 100px);overflow-x: auto;overflow-y: auto;box-sizing: border-box;",
            div(style = "width: 1800px;min-width: 1800px;margin: 0 auto;box-sizing: border-box;",        
                fluidRow(
                  box(
                    style = "margin-left: 20px;",
                    width = 6,
                    h1("Data Access", style = "margin-left: 10px;"),
                    solidHeader = FALSE,
                    h3("Quick Overview", style = "margin-left: 10px;"),
                    style = "text-align: Justify;margin-left: 10px;margin-right:60px;",
                    p(style = "margin-left: 10px;", "This ShinyApp makes forecast and/or presents hindcast results on the particle entrainment within the Sacramento-San Joaquin Delta. The real-time simulations and predictions are used for providing quick assessment and help with the potential effects of CVP and SWP alternative operations on listed species. This interactive application is designed based on the machine learning models that were originally developed for the Contra Costa Water District (CCWD)'s hydraulic footprint project."),
                    h5("Here are three types of models:", style = "margin-left: 10px;"),
                    div(style = "margin-left:60px",
                        h5("- DSM2 ECO-PTM emulator models",
                           tags$a(" (part 1)", href = "#intro-eco-ptm")
                        ),
                        h5("- DSM2 PTM emulator models",
                           tags$a(" (part 2)", href = "#intro-ptm")
                        ),
                        h5("- Model for the Entrainment Event Horizon",
                           tags$a(" (part 3)", href = "#intro-event-horizon")
                        )
                    ),
                    h5(style = "margin-left:10px;",
                      tags$a("Model Visualization Tools",href = "#vis-tool"),
                      " are also provided for the ECO-PTM and PTM entrainment models to help users interpret LightGBM decision logic, trace the hydrologic conditions and sample support associated with specific nodes and leaves, compare decision pathways across trees and time horizons, and interactively investigate model structure, thresholds, and outputs."),
                    h3("Data Availability", style = "margin-left: 10px;"),
                    
                    p(style = "margin-left: 10px;",
                      "Data and tools supporting the PTM Emulator dashboard are provided below."
                    ),
                    
                    tags$hr(),
                    
                    # ---------------------
                    # GitHub
                    # ---------------------
                    div(
                      style = "
          padding:15px;
          border:1px solid #d9d9d9;
          border-radius:6px;
          margin-bottom:15px;
          background:white;
        ",
                      
                      tags$h4("PTM Emulator GitHub Repository"),
                      
                      p(
                        "Access source code, model workflow documentation, emulator development resources, and supporting scripts."
                      ),
                      
                      tags$a(
                        class = "btn btn-success",
                        href = "https://github.com/rojkv/PTM_Emulator_Workflow",
                        target = "_blank",
                        icon("github"),
                        " View on GitHub"
                      )
                    ),
                    
                    # ---------------------
                    # SacPAS
                    # ---------------------
                    div(
                      style = "
          padding:15px;
          border:1px solid #d9d9d9;
          border-radius:6px;
          background:white;
        ",
                      
                      tags$h4("SacPAS Weekly Assessment"),
                      
                      p(
                        "Weekly Sacramento River Winter-Run assessment forecasts and supporting evaluation products."
                      ),
                      
                      tags$a(
                        class = "btn btn-primary",
                        href = "https://can01.safelinks.protection.outlook.com/?url=https%3A%2F%2Fcbr.washington.edu%2Fsacramento%2Fassessments%2Ftest%2Fforecast_sacpas.html&data=05%7C02%7CLaura.Manuel%40stantec.com%7C7d690048e1804ee4cc7408deccc5ac84%7C413c6f2c219a469297d3f2b4d80281e7%7C0%7C0%7C639173346569845423%7CUnknown%7CTWFpbGZsb3d8eyJFbXB0eU1hcGkiOnRydWUsIlYiOiIwLjAuMDAwMCIsIlAiOiJXaW4zMiIsIkFOIjoiTWFpbCIsIldUIjoyfQ%3D%3D%7C0%7C%7C%7C&sdata=OIZCfSzN%2BkO8UrHo3QV4D4Aike6mQpP7LvkmfgoCKa0%3D&reserved=0",
                        target = "_blank",
                        icon("external-link-alt"),
                        " Open SacPAS Assessment"
                      )
                    )
                  ),
                  box(style = "margin-left:20px;",width = 12,solidHeader = FALSE,
                      h3(id = "intro-eco-ptm","P1 DSM2 ECO-PTM emulator models",style = "margin-left:10px;"),
                      gt::gt_output("ecoptm_inputs_table"),
                      tags$hr(),
                      fluidRow(
                        column(width = 7,
                               box(title = "Model Visualization Tool",width = 12,height = "720px",status = "primary",solidHeader = TRUE,
                                   leaflet::leafletOutput("eco_ptm_map", width = "100%", height = "650px")
                               )
                        ),
                        column(width = 5,
                               box(title = "Model Parameters",width = 12,height = "720px",status = "primary",solidHeader = TRUE,
                                   div(style = "height:650px;overflow-y:auto;",gt::gt_output("ecoptm_parameters_table"))
                               )
                        )
                      )
                  ),
                  box(style = "margin-left:20px;",width = 12,solidHeader = FALSE,
                      h3(id = "intro-ptm","P2 DSM2 PTM emulator models",style = "margin-left:10px;"),
                      gt::gt_output("ptm_inputs_table"),
                      tags$hr(),
                      fluidRow(
                        column(width = 7,
                               box(title = "DSM2 Nodes and Model Input Stations",width = 12,height = "720px",status = "primary",solidHeader = TRUE,
                                   tabsetPanel(id = "ptm_map_tabs",type = "tabs",
                                   tabPanel(title = "7-Day",leaflet::leafletOutput("node_station_map_7day",width = "100%",height = "620px")),
                                   tabPanel(title = "30-Day",leaflet::leafletOutput("node_station_map_30day",width = "100%",height = "620px"))
                                  )
                                )
                              ),
                        column(width = 5,
                               box(title = "Model Parameters",width = 12,height = "720px",status = "primary",solidHeader = TRUE,
                               div(style = "height:650px;overflow-y:auto;",
                               gt::gt_output("ptm_parameters_table")
                                  )
                                )
                              )
                    )
                  ),        
                  box(style = "margin-left:20px;",width = 12,solidHeader = FALSE,
                      h3(id = "intro-event-horizon","P3 Model for the Entrainment Event Horizon",style = "margin-left:10px;"),
                      gt::gt_output("horizon_inputs_table"),
                      tags$hr(),
                      fluidRow(
                        column(width = 7,
                          box(title = "Model Input Locations",width = 12,height = "740px",status = "primary",solidHeader = TRUE,
                            leaflet::leafletOutput("horizon_map",width = "100%",height = "670px")
                                )
                              ),
                        
                        column(width = 5,
                          box(title = "Inputs Data Range",width = 12,height = "320px",status = "primary",solidHeader = TRUE,
                            div(style = "height:350px;overflow-y:auto;",
                              gt::gt_output("horizon_datarange_table")
                                )
                              ),
                          
                          box(title = "Model Parameters",width = 12,height = "400px",status = "primary",solidHeader = TRUE,
                            div(style = "height:350px;overflow-y:auto;",
                              gt::gt_output("horizon_parameter_table")
                                )
                              )
                        )
                      )
                  ),  
                  box(style = "margin-left:20px;",width = 12,solidHeader = FALSE,
                      h3(id = "vis-tool","Model Visualization Tools",style = "margin-left:10px;"),
                      h5("This tool visualizes the model structure and helps users:",style = "margin-left:30px;"),
                      tags$ul(
                        class = "vis-bullet-list",
                        tags$li("Understand how the LightGBM models make internal decisions."),
                        tags$li("Identify the combination of hydrologic conditions associated with a particular leaf."),
                        tags$li("Compare decision logic across trees, model outcomes, and time horizons."),
                        tags$li("Number of training samples at the node"),
                        tags$li("Investigate unusual thresholds, feature usage, or leaf outputs."),
                        tags$li("Communicate and review model structure through an interactive visualization."),
                        style = "margin-left:60px;"
                      ),
                      h5("Important limitations:",style = "margin-left:30px;"),
                      tags$ul(
                        class = "vis-bullet-list",
                        tags$li("Each diagram represents only one tree and one selected leaf, not the complete ensemble prediction."),
                        tags$li("The displayed leaf value is the output or contribution of that individual tree. It should not be interpreted as the final model prediction, which requires combining contributions from all trees."),
                        tags$li("Users enter Tree and Leaf indices rather than actual hydrologic inputs. These pages are therefore model-explanation tools, not scenario-prediction calculators."),
                        tags$li("The gray “Other condition” nodes are conceptual context only and are not full representations of the actual alternative subtrees."),
                        tags$li("The model data are embedded in the HTML files, but the network-visualization library is loaded from an external CDN."),
                        style = "margin-left:60px;"
                      ),                      
                      h5("Clicking a node or edge displays information such as:",style = "margin-left:30px;"),
                      tags$ul(
                        class = "vis-bullet-list",
                        tags$li("Feature used at the split"),
                        tags$li("Numeric split threshold"),
                        tags$li("Direction taken by missing values"),
                        tags$li("Number of training samples at the node"),
                        tags$li("Leaf depth"),
                        tags$li("Output value of the selected tree leaf"),
                        style = "margin-left:60px;"
                      ),                      
                      tags$style(HTML(".vis-bullet-list {padding-left: 25px;}.vis-bullet-list li {margin-bottom: 5px;padding-left: 5px;}.vis-bullet-list li::marker {font-size: 1.2em;}")),
 
                      tags$hr(),
                      
                      fluidRow(
                        column(width = 6,
                               box(title = "ECO-PTM Model Visualization Tool",width = 12,height = "720px",status = "primary",solidHeader = TRUE,
                                   h5("This tool visualizes the internal decision path of an individual tree within the ECO-PTM LightGBM models. DSM2 ECO-PTM emulator models for end-of-150-day combined through-Delta survival and routing into the inteior Delta (Georgiana Slough and Delta Cross Channel) of salmon particles.",style = "margin-left:10px;"),  
                                   tags$iframe(src = "ECOPTM_path_explorer.html",width = "100%",height = "650px",style = "display:block;border:none;")
                               )
                        ),
                        column(width = 6,
                               box(title = "PTM entrainment Model Visualization Tool",width = 12,height = "720px",status = "primary",solidHeader = TRUE,
                                   h5("This tool visualizes decision paths within the PTM entrainment emulator models. DSM2 PTM emulator models for end-of-7-day and 30-day percent entainment of surface-oriented particles into the CVP and SWP export facilities (Cliftonr Court Forebay and Jones Pumping Plant).",style = "margin-left:10px;"),
                                   tags$iframe(src = "PTM_Entrainment_path_explorer.html",width = "100%",height = "650px",style = "display:block;border:none;")
                               )
                        )
                      )
                  ),
                )
            )
        )
      )
    )
  )
)
>>>>>>> f61d0ad (data access page maps added)
