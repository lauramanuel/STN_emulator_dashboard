library(shiny)
library(shinydashboard)
library(leaflet)

ui <- dashboardPage(
  
  # -----------------------------
  # Header with Logo + Title
  # -----------------------------
  dashboardHeader(
    titleWidth = 300,   # ✅ THIS IS THE KEY FIX
    
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
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Current Conditions", tabName = "current", icon = icon("chart-line")),
      menuItem("Forecast Conditions", tabName = "forecast", icon = icon("cloud")),
      menuItem("Validation", tabName = "validation", icon = icon("check")),
      menuItem("7-Day Entrainment", tabName = "entrainment", icon = icon("calendar")),
      menuItem("Event Horizon", tabName = "map", icon = icon("map")),
      menuItem("Data Access", tabName = "data", icon = icon("database"))
    ),
    
    br(),
    
    selectInput("scenario", "Scenario:", choices = NULL),
    selectInput("node", "Node:", choices = NULL),
    
    dateRangeInput("dates",
                   "Date Range:",
                   start = NULL,
                   end = NULL)
  ),
  
  # -----------------------------
  # Body with Custom Theme
  # -----------------------------
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
      
      /* Fix logo container width */
    .main-header .logo {
      width: 300px !important;
    }
    
    /* Expand sidebar width to match */
    .main-sidebar {
      width: 300px !important;
    }
    
    /* Adjust content to match */
    .content-wrapper, .right-side {
      margin-left: 300px !important;
    }
    
      
      /* =========================
       FIX HEADER LOGO CUTTING
    ========================= */
    
    /* Allow logo container to show full image */
    .main-header .logo {
      overflow: visible !important;
      height: 60px !important;
      line-height: 60px !important;
      padding: 5px 10px;
    }
    
    /* Fix image sizing and alignment */
    .main-header .logo img {
      max-height: 45px;
      height: auto;
      width: auto;
      vertical-align: middle;
    }
    
    /* Fix navbar height to match */
    .main-header .navbar {
      min-height: 60px;
    }

    /* =========================
       PAGE BACKGROUND
    ========================= */
    .content-wrapper, .right-side {
      background-color: #f9fbfc;
      padding: 15px;
    }

    /* =========================
       HEADER (TOP BAR)
    ========================= */
    .main-header .navbar {
      background-color: white;
      border-bottom: 3px solid #0a7e8c;
    }

    .main-header .logo {
      background-color: white;
      color: #0a7e8c;
      font-family: Segoe UI Semibold;
      border-bottom: 3px solid #0a7e8c;
    }

    /* =========================
       SIDEBAR (LIGHT STYLE)
    ========================= */
    .main-sidebar {
      background-color: #ffffff;
      border-right: 1px solid #e0e0e0;
    }

    /* Sidebar text */
    .sidebar-menu > li > a {
      color: #444;
      font-size: 14px;
    }

    /* Active tab */
    .sidebar-menu .active > a {
      background-color: #e6f4f6 !important;
      color: #0a7e8c !important;
      border-left: 4px solid #0a7e8c;
    }

    /* Hover */
    .sidebar-menu li:hover > a {
      background-color: #f2f9fa !important;
      color: #0a7e8c !important;
    }

    /* =========================
       SIDEBAR INPUTS
    ========================= */
    .sidebar .form-group {
      margin-bottom: 15px;
    }

    /* =========================
       BOX (CARD) STYLE
    ========================= */
    .box {
      border-top: 3px solid #0a7e8c;
      border-radius: 6px;
      box-shadow: 0px 2px 6px rgba(0,0,0,0.05);
    }

    .box-header {
      font-weight: 600;
      font-size: 16px;
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

    /* =========================
       DIVIDERS
    ========================= */
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
      # About Tab
      # -----------------------------
      tabItem(tabName = "about",
              
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
                      h4("Version 1.0.0",
                         style = "font-style: italic;"),
                      
                      tags$hr(),
                      
                      h5("Model Workflow: DSM2 → PTM → Emulator"),
                      
                      h5("GitHub Repository:",
                         tags$a("PTM Emulator Workflow",
                                href = "https://github.com/rojkv/PTM_Emulator_Workflow")),
                      
                      h5("Data Sources:",
                         "DSM2, SACPAS, PTM outputs")
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  title = "Overview",
                  
                  p("This dashboard provides real-time and forecasted PTM emulator outputs."),
                  p("Includes survival, entrainment, routing, and validation metrics."),
                  p("Supports planning-level decision making and model QA/QC.")
                )
              )
      ),
      
      # -----------------------------
      # Current Conditions
      # -----------------------------
      tabItem(tabName = "current",
              
              fluidRow(
                box(width = 12, title = "Survival Time Series",
                    plotOutput("survival_plot"))
              ),
              
              fluidRow(
                box(width = 12, title = "Entrainment Time Series",
                    plotOutput("entrainment_plot"))
              ),
              
              fluidRow(
                box(width = 12, title = "Route Distribution",
                    plotOutput("route_plot"))
              )
      ),
      
      # -----------------------------
      # Forecast
      # -----------------------------
      tabItem(tabName = "forecast",
              h3("Forecast Conditions (Structure Mirrors Current Conditions)")
      ),
      
      # -----------------------------
      # Validation
      # -----------------------------
      tabItem(tabName = "validation",
              
              fluidRow(
                box(width = 12,
                    title = "PTM vs Emulator Comparison",
                    plotOutput("validation_plot"))
              )
      ),
      
      # -----------------------------
      # 7-Day Entrainment
      # -----------------------------
      tabItem(tabName = "entrainment",
              
              fluidRow(
                box(width = 12,
                    title = "7-Day Entrainment",
                    plotOutput("entrainment_7day_plot"))
              )
      ),
      # -----------------------------
      # map
      # -----------------------------
      tabItem(tabName = "map",
              
              fluidRow(
                box(
                  width = 12,
                  title = "Entrainment Zone Risk",
                  leafletOutput("map", height = 600)
                )
              )
      ),
      # -----------------------------
      # Data Access
      # -----------------------------
      tabItem(tabName = "data",
              
              h3("Data Access"),
              
              tags$ul(
                tags$li(tags$a("PTM Emulator GitHub Repo",
                               href = "https://github.com/rojkv/PTM_Emulator_Workflow")),
                tags$li("Model input datasets"),
                tags$li("Emulator output files"),
                tags$li("Machine learning notebooks")
              )
      )
    )
  )
)