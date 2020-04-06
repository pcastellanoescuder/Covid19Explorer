options(repos = BiocManager::repositories())
getOption("repos")

source("helpers.R")

dashboardPage(
              
  dashboardHeader(
    
    title = "Covid-19 Explorer"
    ),
  
    dashboardSidebar(sidebarMenu(
      menuItem("Input Data", tabName = "inputdata", icon = icon("upload")),
      menuItem("Plots by Time", tabName = "timeplots", icon = icon("chart-line")),
      menuItem("Correlations", tabName = "correlations", icon = icon("chart-line"))
      # menuItem("Multivariate", tabName = "multivariate", icon = icon("layer-group"))
      
    )),
    
    dashboardBody(
      
      shinyDashboardThemes(
        theme = "onenote"),

      tabItems(
        tabItem(tabName = "inputdata",
                source("ui-tab-inputdata.R",local=TRUE)$value),
        tabItem(tabName = "timeplots",
                source("ui-tab-timeplots.R",local=TRUE)$value),
        tabItem(tabName = "correlations",
                source("ui-tab-correlations.R",local=TRUE)$value)
        # tabItem(tabName = "multivariate",
        #         source("ui-tab-multivariate.R",local=TRUE)$value)
      ),
      
      tags$hr(),
      
      ## FOOTER
      
      tags$footer(p(strong("Pol Castellano Escuder, Paco Carmona Pontaque and Alex Sánchez Pla"), align="center", width=3),
                  p("Statistics and Bioinformatics Research Group", align="center", width=3),
                  p(("University of Barcelona"), align="center", width=3),
                  p(("Copyright (C) 2020, app licensed under GPLv3"), align="center",width=4), align="center", width=4) #,
      
      ## GOOGLE ANALYTICS
      
      # tags$head(includeScript("google-analytics.js"))
    )
)

