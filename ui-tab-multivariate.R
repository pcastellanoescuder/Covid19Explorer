
fluidRow(
  column(width = 3,
                wellPanel(
                  
                  selectInput("groups", label = "Select your factor:", choices = NULL),
                  selectInput("mult_vars", label = "Select numerical variables:", choices = NULL, multiple = TRUE)
  )),
  
  column(width = 8,
         
         fluidPage(
           tabsetPanel(
             tabPanel("Principal Component Analysis", 
                      plotlyOutput("pca_plot"))
             # tabPanel("Table of Correlations", dataTableOutput("corr_table"))
           ))

  ))

