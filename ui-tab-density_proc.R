fluidRow(
  column(width = 3,
         wellPanel(
           
           selectizeInput("dens_feat_proc", label = "Select a numeric variable:", choices = NULL),
           selectizeInput("dens_fact", label = "Select a factor variable:", choices = NULL)
           
         )),
  
  column(width = 9,
         
         tabsetPanel(
           
           tabPanel("Summary Table", dataTableOutput("descriptive_proc")),
           tabPanel("Distribution Plots", plotOutput("densityplots_proc", height = "500px")),
           tabPanel("Boxplots with Tests", plotOutput("densityplots_proc_boxplot"))
         )
  )
)