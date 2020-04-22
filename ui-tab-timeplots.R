fluidRow(
  column(width = 3,
         wellPanel(
           
           selectizeInput("features", label = "Select your features:", choices = NULL, multiple = TRUE),
           
           checkboxInput("wrap_into", "Split by ID", TRUE),
           
           checkboxInput("plot_lines", "Show lines", TRUE)
  
  )),
  
  column(width = 9,
         
         plotOutput("timeplots", height = "500px")
         
         )
  )

