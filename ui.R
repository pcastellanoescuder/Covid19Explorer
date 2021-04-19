
source("helpers.R")

dashboardPage(
  
  dashboardHeader(
    
  title = "Covid19Explorer",
  
  tags$li(a(href = 'http://www.ueb.vhir.org',
            img(src = 'ueb.png',
                title = "UEB", height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown"),
  
  tags$li(a(href = 'https://sites.google.com/view/estbioinfo',
            img(src = 'eib.png',
                title = "EIB", height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown"),
  
  tags$li(a(href = 'http://vhir.org/portal1/',
            img(src = 'vhir.png',
                title = "VHIR", height = "30px"),
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
             menuSubItem("Correlogram", tabName = "correlogram"),
             menuSubItem("Network Visualization", tabName = "network"),
             menuSubItem("Correlation Table", tabName = "corrtable")),
    menuItem("Principal Component Analysis", tabName = "pca", icon = icon("object-group")),
    menuItem("Univariate Analysis", tabName = "univariate", icon = icon("sliders")),
    menuItem("Prediction Models", tabName = "prediction", icon = icon("chart-line"), startExpanded = FALSE,
             menuSubItem("Lasso Regression", tabName = "lasso"),
             menuSubItem("Random Forest", tabName = "rf")),
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
        tabItem(tabName = "corrtable",
                source("ui-tab-corrtable.R",local=TRUE)$value),
        tabItem(tabName = "pca",
                source("ui-tab-pca.R",local=TRUE)$value),
        tabItem(tabName = "univariate",
                source("ui-tab-univariate.R",local=TRUE)$value),
        tabItem(tabName = "lasso",
                source("ui-tab-lasso.R",local=TRUE)$value),
        tabItem(tabName = "rf",
                source("ui-tab-rf.R",local=TRUE)$value),
        tabItem(tabName = "help",
                source("ui-tab-help.R",local=TRUE)$value)
      ),
      
      tags$hr(),
      
      ## FOOTER
      
      tags$footer(p(strong(a("Pol Castellano Escuder", href = "https://pcastellanoescuder.github.io"), ",",
                           a("Francesc Carmona Pontaque", href = "https://sites.google.com/view/estbioinfo/home?authuser=0"), "and",
                           a("Alex Sánchez Pla", href = "https://webgrec.ub.edu/webpages/000011/cat/asanchez.ub.edu.html")), align="center", width=3),
                  p("Statistics and Bioinformatics Research Group", align="center", width=3),
                  p(("University of Barcelona"), align="center", width=3),
                  p(("Copyright (C) 2021, app licensed under GPLv3"), align="center", width=4), align="center", width=4,
                  p(("Code available on Github:"),a("https://github.com/pcastellanoescuder/Covid19Explorer",
                                                    href="https://github.com/pcastellanoescuder/Covid19Explorer"))),
      
      ## GOOGLE ANALYTICS
      
      tags$head(includeScript("google-analytics.js"))
    )
)

