# options(repos = BiocManager::repositories())
# getOption("repos")

source("helpers.R")

dashboardPage(
  
  dashboardHeader(
    
  title = "Covid-19 Explorer",
  
  tags$li(a(href = 'http://www.ueb.vhir.org',
            img(src = 'ueb.png',
                title = "UEB", height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown"),
  
  tags$li(a(href = 'https://sites.google.com/view/estbioinfo',
            img(src = 'ub.png',
                title = "UB", height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown"),
  
  tags$li(a(href = 'https://grbio.upc.edu/en',
            img(src = 'grbio.png',
                title = "GRBIO", height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown")
  ),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Input Data", tabName = "inputdata", icon = icon("upload")),
    menuItem("Descriptive Analysis", tabName = "density", icon = icon("search"), startExpanded = FALSE,
             menuSubItem("Raw Data", tabName = "density_raw"),
             menuSubItem("Processed Data", tabName = "density_proc")),
    menuItem("Time Plots", tabName = "timeplots", icon = icon("chart-line")),
    menuItem("Correlations", tabName = "correlations", icon = icon("chart-line"), startExpanded = FALSE,
             menuSubItem("Scatterplot", tabName = "scatter"),
             menuSubItem("Global Correlation Plot", tabName = "correlogram"),
             menuSubItem("Network Visualization", tabName = "network")),
    menuItem("Principal Component Analysis", tabName = "pca", icon = icon("object-group")),
    # menuItem("XXXX", tabName = "XXXX", icon = icon("XXXX")),
    menuItem("Help", tabName = "help", icon = icon("question"))
    
    )),
  
  dashboardBody(
      
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "mycss.css")
    ),
    
      shinyDashboardThemes(
        theme = "onenote"),

      tabItems(
        tabItem(tabName = "inputdata",
                source("ui-tab-inputdata.R",local=TRUE)$value),
        tabItem(tabName = "density_raw",
                source("ui-tab-density_raw.R",local=TRUE)$value),
        tabItem(tabName = "density_proc",
                source("ui-tab-density_proc.R",local=TRUE)$value),
        tabItem(tabName = "timeplots",
                source("ui-tab-timeplots.R",local=TRUE)$value),
        tabItem(tabName = "scatter",
                source("ui-tab-scatter.R",local=TRUE)$value),
        tabItem(tabName = "correlogram",
                source("ui-tab-correlogram.R",local=TRUE)$value),
        tabItem(tabName = "network",
                source("ui-tab-network.R",local=TRUE)$value),
        tabItem(tabName = "pca",
                source("ui-tab-pca.R",local=TRUE)$value),
        tabItem("help",
                source("ui-tab-help.R",local=TRUE)$value)
      ),
      
      tags$hr(),
      
      ## FOOTER
      
      tags$footer(p(strong("Pol Castellano Escuder, Paco Carmona Pontaque and Alex Sánchez Pla"), align="center", width=3),
                  p("Statistics and Bioinformatics Research Group", align="center", width=3),
                  p(("University of Barcelona"), align="center", width=3),
                  p(("Copyright (C) 2020, app licensed under GPLv3"), align="center", width=4), align="center", width=4,
                  p(("Code available on Github:"),a("https://github.com/pcastellanoescuder/shiny_covid19",
                                                    href="https://github.com/pcastellanoescuder/shiny_covid19"))),
      
      ## GOOGLE ANALYTICS
      
      tags$head(includeScript("google-analytics.js"))
    )
)

