fluidRow(
  column(width = 3,
         wellPanel(
           
           selectizeInput("dens_feat_proc", label = "Select your variable:", choices = NULL)
           
         )),
  
  column(width = 9,
         
         tabsetPanel(
           
           tabPanel("Table", dataTableOutput("descriptive_proc")),
           tabPanel("Plots", plotOutput("densityplots_proc", height = "500px"))
         )
  )
)