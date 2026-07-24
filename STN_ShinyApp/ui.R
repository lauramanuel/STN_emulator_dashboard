library(shiny)
library(shinydashboard)
library(leaflet)
library(gt)

# -----------------------------
# Helper UI Functions
# -----------------------------

timeseries_tab <- function(tab_name, title, theme_class, output_id, summary_id) {
  tabItem(
    tabName = tab_name,
    
    div(
      class = theme_class,
      
      fluidRow(
        box(
          width = 12,
          title = title,
          status = NULL,
          solidHeader = FALSE,
          plotOutput(output_id, height = "900px")
        )
      ),
      
      fluidRow(
        box(
          width = 12,
          title = "Selected Node Detail",
          status = NULL,
          solidHeader = FALSE,
          tableOutput(summary_id)
        )
      )
    )
  )
}

ecoptm_tab <- function(tab_name, title, theme_class, table_id) {
  tabItem(
    tabName = tab_name,
    
    div(
      class = theme_class,
      
      fluidRow(
        box(
          width = 12,
          title = title,
          status = NULL,
          solidHeader = FALSE,
          tableOutput(table_id)
        )
      )
    )
  )
}

event_horizon_tab <- function(
    tab_name,
    title,
    theme_class,
    map_id,
    scatter25_id,
    scatter50_id,
    scatter75_id
) {
  
  tabItem(
    
    tabName = tab_name,
    
    div(
      
      class = theme_class,
      
      fluidRow(
        
        box(
          width = 12,
          title = paste(title, "- Map"),
          leafletOutput(map_id, height = 500)
        )
        
      ),
      
      fluidRow(
        
        box(
          width = 4,
          title = paste(title, "- 25% Risk"),
          plotOutput(scatter25_id, height = 400)
        ),
        
        box(
          width = 4,
          title = paste(title, "- 50% Risk"),
          plotOutput(scatter50_id, height = 400)
        ),
        
        box(
          width = 4,
          title = paste(title, "- 75% Risk"),
          plotOutput(scatter75_id, height = 400)
        )
        
      )
    )
  )
}

# -----------------------------
# UI
# -----------------------------

ui <- dashboardPage(
  
  # -----------------------------
  # Header
  # -----------------------------
  dashboardHeader(
    titleWidth = 300,
    
    title = tags$div(
      style = "display:flex; align-items:center;",
      
      tags$img(
        src = "logo.png",
        height = "30px",
        style = "margin-right:10px;"
      ),
      
      tags$span(
        "PTM Emulator",
        style = "font-family: Segoe UI Semibold; font-size: 16px;"
      )
    )
  ),
  
  # -----------------------------
  # Sidebar
  # -----------------------------
  dashboardSidebar(
    
    sidebarMenu(
      id = "tabs",
      
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      
      menuItem(
        "Current 7d Average Flow",
        icon = icon("water"),
        startExpanded = TRUE,
        menuSubItem("PTM 7d Entrain", tabName = "current7_ptm7", icon = icon("chart-line")),
        menuSubItem("PTM 30d Entrain", tabName = "current7_ptm30", icon = icon("chart-line")),
        menuSubItem("ECO PTM", tabName = "current7_ecoptm", icon = icon("table")),
        menuSubItem("Event Horizon", tabName = "current7_event", icon = icon("map"))
      ),
      
      menuItem(
        "Current 30d Average Flow",
        icon = icon("droplet"),
        startExpanded = FALSE,
        menuSubItem("PTM 7d Entrain", tabName = "current30_ptm7", icon = icon("chart-line")),
        menuSubItem("PTM 30d Entrain", tabName = "current30_ptm30", icon = icon("chart-line")),
        menuSubItem("ECO PTM", tabName = "current30_ecoptm", icon = icon("table")),
        menuSubItem("Event Horizon", tabName = "current30_event", icon = icon("map"))
      ),
      
      menuItem(
        "Forecast 7d Average Flow",
        icon = icon("cloud-sun"),
        startExpanded = FALSE,
        menuSubItem("PTM 7d Entrain", tabName = "forecast7_ptm7", icon = icon("chart-line")),
        menuSubItem("PTM 30d Entrain", tabName = "forecast7_ptm30", icon = icon("chart-line")),
        menuSubItem("ECO PTM", tabName = "forecast7_ecoptm", icon = icon("table")),
        menuSubItem("Event Horizon", tabName = "forecast7_event", icon = icon("map"))
      ),
      
      menuItem("Data Access", tabName = "data", icon = icon("database"))
    ),
    
    br(),
    
    div(
      class = "sidebar-controls",
      
      # Selects which dated run folder's master xlsx drives the app,
      # e.g. "20260623" -> Jun 23, 2026. Choices are populated from
      # the folder names found under STN_EMULATOR/Output/.
      selectInput("run_date", "Model Run Date:", choices = NULL),
      
      # Informational display of the active scenario's date window
      # (e.g. "Jun 15 - Jun 21, 2026 (7-day average)"). Not a filter --
      # each averaging window is a fixed snapshot in the master file.
      uiOutput("date_window"),
      
      uiOutput("scenario_control"),
      
      conditionalPanel(
        condition = "
          input.tabs == 'current7_ptm7' ||
          input.tabs == 'current7_ptm30' ||
          input.tabs == 'current7_ecoptm' ||
          input.tabs == 'current30_ptm7' ||
          input.tabs == 'current30_ptm30' ||
          input.tabs == 'current30_ecoptm' ||
          input.tabs == 'forecast7_ptm7' ||
          input.tabs == 'forecast7_ptm30' ||
          input.tabs == 'forecast7_ecoptm'
        ",
        selectInput("node", "Node:", choices = NULL)
      ),
      
      conditionalPanel(
        condition = "
          input.tabs == 'current7_event' ||
          input.tabs == 'current30_event' ||
          input.tabs == 'forecast7_event'
        ",
        selectInput(
          "eh_risk",
          "Event Horizon Risk:",
          choices = c(25,50,75),
          selected = 25
        ),
        
        radioButtons(
          "event_ptm_model",
          "Risk Layer:",
          choices = c(
            "7-Day PTM" = "PTM 7-Day Entrainment",
            "30-Day PTM" = "PTM 30-Day Entrainment"
          ),
          selected = "PTM 7-Day Entrainment",
          inline = TRUE
        )
      )
    )
  ),
  
  # -----------------------------
  # Body
  # -----------------------------
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        
        /* =========================
           HEADER / SIDEBAR WIDTH
        ========================= */
        .main-header .logo {
          width: 300px !important;
          overflow: visible !important;
          height: 60px !important;
          line-height: 60px !important;
          padding: 5px 10px;
          background-color: white;
          color: #0a7e8c;
          font-family: Segoe UI Semibold;
          border-bottom: 3px solid #0a7e8c;
        }
        
        .main-header .logo img {
          max-height: 45px;
          height: auto;
          width: auto;
          vertical-align: middle;
        }
        
        .main-header .navbar {
          min-height: 60px;
          background-color: white;
          border-bottom: 3px solid #0a7e8c;
        }
        
        .main-sidebar {
          width: 300px !important;
          background-color: #ffffff;
          border-right: 1px solid #e0e0e0;
        }
        
        .content-wrapper, .right-side {
          margin-left: 300px !important;
          background-color: #f9fbfc;
          padding: 18px;
        }
        
        /* =========================
           SIDEBAR - BRIGHT RECLAMATION STYLE
        ========================= */
        
        /* Sidebar background */
        .main-sidebar {
          width: 300px !important;
          background-color: #fbfeff;
          border-right: 1px solid #d8edf1;
        }
        
        /* Main sidebar menu items */
        .sidebar-menu > li > a {
          color: #0a6270;
          font-size: 14px;
          font-weight: 600;
          letter-spacing: 0.1px;
        }
        
        /* Main sidebar icons */
        .sidebar-menu > li > a > .fa,
        .sidebar-menu > li > a > .glyphicon,
        .sidebar-menu > li > a > .ion {
          color: #16889a;
        }
        
        /* Nested submenu background */
        .sidebar-menu .treeview-menu {
          background-color: #f4fbfc !important;
          padding-top: 4px;
          padding-bottom: 4px;
        }
        
        /* Nested submenu items before hover */
        .sidebar-menu .treeview-menu > li > a {
          color: #2f7f8d;
          font-size: 13px;
          font-weight: 500;
        }
        
        /* Active nested submenu item */
        .sidebar-menu .treeview-menu > li.active > a {
          background-color: #d9f1f5 !important;
          color: #075f6d !important;
          border-left: 4px solid #0a7e8c;
        }
        
        /* Hover behavior */
        .sidebar-menu li:hover > a {
          background-color: #eef9fb !important;
          color: #075f6d !important;
        }
        
        /* Sidebar controls section */
        .sidebar-controls {
          padding: 8px 16px 24px 16px;
          border-top: 1px solid #d8edf1;
          margin-top: 10px;
        }
        
        /* Spacing between controls */
        .sidebar .form-group {
          margin-bottom: 18px;
        }
        
        /* Date Range / Scenario / Node labels */
        .sidebar .control-label {
          color: #0a7e8c;
          font-weight: 700;
          font-size: 13px;
          letter-spacing: 0.2px;
        }
        
        /* Input boxes */
        .sidebar .form-control,
        .sidebar .selectize-input {
          border: 1px solid #b8dce2;
          border-radius: 5px;
          color: #334;
          background-color: #ffffff;
        }
        
        /* Input focus */
        .sidebar .form-control:focus,
        .sidebar .selectize-input.focus {
          border-color: #0a7e8c;
          box-shadow: 0 0 4px rgba(10, 126, 140, 0.25);
        }
        
        /* =========================
           CARD STYLE
        ========================= */
        .box {
          border-radius: 8px;
          box-shadow: 0px 2px 8px rgba(0,0,0,0.06);
          border-top: 4px solid #0a7e8c;
        }
        
        .box-header {
          font-weight: 600;
          font-size: 16px;
        }
        
        .box-title {
          font-weight: 600;
        }
        
        /* =========================
           SECTION THEMES
        ========================= */
        .current-theme .box {
          border-top-color: #e69f00;
        }
        
        .current-theme {
          background-color: #fffaf2;
          padding: 6px;
          border-radius: 8px;
        }
        
        .forecast-theme .box {
          border-top-color: #56b4e9;
        }
        
        .forecast-theme {
          background-color: #f4fbff;
          padding: 6px;
          border-radius: 8px;
        }
        
        /* =========================
           TYPOGRAPHY
        ========================= */
        h1, h2, h3, h4 {
          font-family: Segoe UI Semibold;
          color: #333;
        }
        
        body {
          font-family: Segoe UI;
          color: #333;
        }
        
        hr {
          border-top: 1px solid #0a7e8c;
          opacity: 0.4;
          margin-top: 10px;
          margin-bottom: 10px;
        }
        
      "))
    ),
    
    tabItems(
      
      # -----------------------------
      # About
      # -----------------------------
      tabItem(
        tabName = "about",
        
        fluidRow(
          box(
            width = 6,
            height = 300,
            solidHeader = FALSE,
            div(
              style = "display: flex; align-items: center;",
              tags$img(src = "logo.png", height = "160px"),
              
              div(
                style = "margin-left: 30px;margin-right:50px;",
                
                h1("PTM Emulator Dashboard",),
                h4("Version:", weight = "bold",
                   tags$code("  1.2.2  ", style = "margin-left:20px"),
                   tags$a("[Release Notes]", style = "font-style: italic; margin-left:20px;", href = "#release-notes")
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
                   tags$a("Historical Results for All PTM, ECO-PTM, and Event Horizon models", style = "font-style: italic;",
                          href = "https://github.com/lauramanuel/STN_emulator_dashboard/tree/main/STN_EMULATOR/Output")
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 8,
            height = 2000,
            div(style = "margin-left: 60px;margin-right:120px",
                h2("Overview"),
                h5(style = "text-align: justify;", "This ShinyApp makes forecast and/or presents hindcast results on the particle entrainment within the Sacramento-San Joaquin Delta. The real-time simulations and predictions are used for providing quick assessment and help with the potential effects of CVP and SWP alternative operations on listed species. This interactive application is designed based on the machine learning models that were originally developed for the Contra Costa Water District (CCWD)’s",
                   tags$a("hydraulic footprint project",
                          href = "https://github.com/cchang-ccwater/CCWD_Hydraulic_Footprints"),
                   tags$b(".")
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
                    h5("This app can work on Edge, Chrome, Safari, and Firefox, as tested till the version v1.2."),
                    h4("Performance Standards: "),
                    h5(style = "text-align: justify;","The application provides reasonable load times under normal operating conditions. The ECO-PTM page typically loads in less than 1 second; the PTM page in approximately 2-3 seconds; and the Event Horizon page in approximately 7-9 seconds because it loads Leaflet maps, geo-spatial files, and multiple plots. Standard weekly prediction tasks are generally completed almost immediately, while large prediction requests involving long time series and many input features, e.g., 190k records, may require substantially more processing and rendering time."),
                    h5(style = "text-align: justify;","Concurrent-user capacity depends on the deployment environment, including available CPU, memory, and the number of Shiny worker processes. The application is expected to support multiple users performing normal navigation, data exploration, and standard predictions, although several simultaneous computationally intensive prediction requests may increase response times. Final concurrent-user capacity should therefore be confirmed through load testing in the production environment."),
                    h4("Mobile Responsiveness:"),
                    h5("This application is usable also on mobile devices."),

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
      ),

      # -----------------------------
      # Current 7d Average Flow
      # -----------------------------
      timeseries_tab(
        "current7_ptm7",
        "Current 7d Average Flow - PTM 7d Entrainment",
        "current-theme",
        "current7_ptm7_plot",
        "current7_ptm7_summary"
      ),
      
      timeseries_tab(
        "current7_ptm30",
        "Current 7d Average Flow - PTM 30d Entrainment",
        "current-theme",
        "current7_ptm30_plot",
        "current7_ptm30_summary"
      ),
      
      ecoptm_tab(
        "current7_ecoptm",
        "Current 7d Average Flow - ECO PTM",
        "current-theme",
        "current7_ecoptm_table"
      ),
      
      event_horizon_tab(
        "current7_event",
        "Current 7d Average Flow - Event Horizon",
        "current-theme",
        "current7_event_map",
        "current7_event_scatter25",
        "current7_event_scatter50",
        "current7_event_scatter75"
      ),
      
      # -----------------------------
      # Current 30d Average Flow
      # -----------------------------
      timeseries_tab(
        "current30_ptm7",
        "Current 30d Average Flow - PTM 7d Entrainment",
        "current-theme",
        "current30_ptm7_plot",
        "current30_ptm7_summary"
      ),
      
      timeseries_tab(
        "current30_ptm30",
        "Current 30d Average Flow - PTM 30d Entrainment",
        "current-theme",
        "current30_ptm30_plot",
        "current30_ptm30_summary"
      ),
      
      ecoptm_tab(
        "current30_ecoptm",
        "Current 30d Average Flow - ECO PTM",
        "current-theme",
        "current30_ecoptm_table"
      ),
      
      event_horizon_tab(
        "current30_event",
        "Current 30d Average Flow - Event Horizon",
        "current-theme",
        "current30_event_map",
        "current30_event_scatter25",
        "current30_event_scatter50",
        "current30_event_scatter75"
      ),
      
      # -----------------------------
      # Forecast 7d Average Flow
      # -----------------------------
      timeseries_tab(
        "forecast7_ptm7",
        "Forecast 7d Average Flow - PTM 7d Entrainment",
        "forecast-theme",
        "forecast7_ptm7_plot",
        "forecast7_ptm7_summary"
      ),
      
      timeseries_tab(
        "forecast7_ptm30",
        "Forecast 7d Average Flow - PTM 30d Entrainment",
        "forecast-theme",
        "forecast7_ptm30_plot",
        "forecast7_ptm30_summary"
      ),
      
      ecoptm_tab(
        "forecast7_ecoptm",
        "Forecast 7d Average Flow - ECO PTM",
        "forecast-theme",
        "forecast7_ecoptm_table"
      ),
      
      event_horizon_tab(
        "forecast7_event",
        "Forecast 7d Average Flow - Event Horizon",
        "forecast-theme",
        "forecast7_event_map",
        "forecast7_event_scatter25",
        "forecast7_event_scatter50",
        "forecast7_event_scatter75"
      ),
      
      # -----------------------------
      # Data Access
      # -----------------------------
      tabItem(
        tabName = "data",
        
        fluidRow(
          box(
            width = 12,
            title = "Data Access",
            solidHeader = FALSE,
            
            h3("Data Availability"),
            
            p(
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
          )
        )
      )
    )
  )
)