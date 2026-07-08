library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  # -----------------------------
  # Header with Logo + Title
  # -----------------------------
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      
      tags$img(src = "logo.png", height = "35px",
               style = "margin-right: 10px;"),
      
      tags$span("PTM Emulator Dashboard",
                style = "font-family: Segoe UI Semibold; font-size: 18px;")
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
      menuItem("Map View", tabName = "map", icon = icon("map")),
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
        
        /* Main background */
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
        }
        
        /* Header */
        .main-header .navbar {
          background-color: #0a7e8c;
        }
        
        .main-header .logo {
          background-color: #0a7e8c;
          font-family: Segoe UI Semibold;
        }
        
        /* Sidebar */
        .main-sidebar {
          background-color: #2c3e50;
        }
        
        /* Active tab */
        .sidebar-menu .active > a {
          background-color: #0a7e8c !important;
          color: white !important;
        }
        
        /* Hover effect */
        .sidebar-menu li:hover > a {
          background-color: #0a7e8c !important;
          color: white !important;
        }
        
        /* Box styling */
        .box {
          border-top: 3px solid #0a7e8c;
        }
        
        /* Typography */
        h1, h2, h3, h4 {
          font-family: Segoe UI Semibold;
        }
        
        body {
          font-family: Segoe UI;
        }
        
        /* Divider */
        hr {
          border-top: 1px solid #0a7e8c;
          opacity: 0.6;
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
                  title = "Spatial Entrainment Map",
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