fluidRow(
  column(width = 3,
         wellPanel(

           selectizeInput("dens_feat_raw", label = "Select your variable:", choices = NULL)

  )),

  column(width = 9,
         
         tabsetPanel(
           
           tabPanel("Table", dataTableOutput("descriptive_raw")),
           tabPanel("Plots", plotOutput("densityplots_raw", height = "500px"))
         )
         )
  )

