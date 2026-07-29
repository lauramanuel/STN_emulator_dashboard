
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(gt)

app_version <- "1.2.2"

run_page <- function(prefix, title, theme_class) {
  tabItem(
    tabName = paste0("run_", prefix),
    div(
      class = theme_class,

      fluidRow(
        box(
          width = 12,
          title = paste(title, "— Input Method"),
          status = if (prefix == "current") "warning" else "info",
          solidHeader = TRUE,

          radioButtons(
            paste0(prefix, "_input_method"),
            "Input Method:",
            choices = c(
              "Enter a Single Set of Values" = "single",
              "Upload CSV or Excel File (coming soon)" = "upload",
              "Read from Archive Folder (coming soon)" = "folder"
            ),
            selected = "single",
            inline = TRUE
          ),

          conditionalPanel(
            condition = sprintf("input.%s_input_method != 'single'", prefix),
            tags$div(
              class = "alert alert-info",
              "This input method is reserved for the next phase. Select “Enter a Single Set of Values” for now."
            )
          )
        )
      ),

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
    scatter_id
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
          width = 12,
          title = paste(title, "- Event Horizon Scatter"),
          plotlyOutput(scatter_id, height = 600)
        )
      )
    )
  )
}

# -----------------------------
# UI
# -----------------------------
app_version <- "1.2.2"

ui <- dashboardPage(
  dashboardHeader(
    titleWidth = 300,
    title = tags$div(
      style = "display:flex;align-items:center;",
      tags$img(src = "logo.png", height = "30px", style = "margin-right:10px;"),
      tags$span("Entrainment Dashboard", style = "font-family:Segoe UI Semibold;font-size:16px;")
    )
  ),

  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Run Current Conditions", tabName = "run_current", icon = icon("water")),
      menuItem("Run Forecast Conditions", tabName = "run_forecast", icon = icon("cloud-sun")),
      menuItem("Scenario Comparison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Data Access", tabName = "data", icon = icon("database"))
    )
  ),

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
                
                h1("PTM Emulator Dashboard"),
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
                   tags$a("Historical Results for All PTM, ECO-PTM, and Event Horizon models", style = "font-style: italic;",
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
                h5(style = "text-align: justify;", "This ShinyApp makes forecast and/or presents hindcast results on the particle entrainment within the Sacramento-San Joaquin Delta. The real-time simulations and predictions are used for providing quick assessment and help with the potential effects of CVP and SWP alternative operations on listed species. This interactive application is designed based on the machine learning models that were originally developed for the Contra Costa Water District (CCWD)’s",
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
                    h5(style = "text-align: justify;","The application provides reasonable load times under normal operating conditions. The ECO-PTM page typically loads in less than 1 second; the PTM page in approximately 2-3 seconds; and the Event Horizon page in approximately 7-9 seconds because it loads Leaflet maps, geo-spatial files, and multiple plots. Standard weekly prediction tasks are generally completed almost immediately, while large prediction requests involving long time series and many input features, e.g., 190k records, may require substantially more processing and rendering time."),
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

      # -----------------------------
      # Current 7d Average Flow
      # -----------------------------
      timeseries_tab(
        "current7_ptm7",
        "Current 7d Average Flow - PTM 7d Entrainment",
        "current-theme",
        "current7_ptm7_plot",
        "current7_ptm7_summary",
        "current7_ptm7_map"
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
            p(style = "margin-left: 10px;", "This ShinyApp makes forecast and/or presents hindcast results on the particle entrainment within the Sacramento-San Joaquin Delta. The real-time simulations and predictions are used for providing quick assessment and help with the potential effects of CVP and SWP alternative operations on listed species. This interactive application is designed based on the machine learning models that were originally developed for the Contra Costa Water District (CCWD)’s hydraulic footprint project."),
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
