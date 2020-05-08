fluidRow(
  column(width = 3,
         wellPanel(

           selectizeInput("dens_feat_raw", label = "Select a numeric variable:", choices = NULL)

  )),

  column(width = 9,
         
         tabsetPanel(
           
           tabPanel("Summary Table", dataTableOutput("descriptive_raw")),
           tabPanel("Distribution Plots for Numerical Variables", plotOutput("densityplots_raw", height = "500px"))
         )
         )
  )

