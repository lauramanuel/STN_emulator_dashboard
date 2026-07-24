library(shiny)
library(shinydashboard)
library(leaflet)

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
            width = 12,
            solidHeader = FALSE,
            
            div(
              style = "display: flex; align-items: center;",
              
              tags$img(src = "logo.png", height = "80px"),
              
              div(
                style = "margin-left: 20px;",
                
                h2("PTM Emulator Dashboard"),
                h4("Version 1.1.0", style = "font-style: italic;"),
                
                tags$hr(),
                
                h5("Model Workflow: DSM2 → PTM → Emulator"),
                
                h5(
                  "GitHub Repository:",
                  tags$a(
                    "PTM Emulator Workflow",
                    href = "https://github.com/rojkv/PTM_Emulator_Workflow"
                  )
                ),
                
                h5("Data Sources:", "All_PTM_ECOPTM_Event_Horizon_Results.xlsx (DSM2, SACPAS, PTM outputs)")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Overview",
            p("This dashboard provides real-time and forecasted PTM emulator outputs."),
            p("Includes survival, entrainment, routing, ECO PTM, and Event Horizon outputs."),
            p("Supports planning-level decision making and model QA/QC.")
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
        
        h3("Data Access"),
        
        tags$ul(
          tags$li(
            tags$a(
              "PTM Emulator GitHub Repo",
              href = "https://github.com/rojkv/PTM_Emulator_Workflow"
            )
          ),
          tags$li("Master results file: All_PTM_ECOPTM_Event_Horizon_Results.xlsx"),
          tags$li("Model input datasets"),
          tags$li("Emulator output files"),
          tags$li("Machine learning notebooks")
        )
      )
    )
  )
)