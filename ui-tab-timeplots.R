fluidRow(
  column(width = 3,
         wellPanel(
           
           selectizeInput("features", label = "Select your features:", choices = NULL, multiple = TRUE),
           
           selectizeInput("time_fact", label = "Select a factor variable (optional):", choices = NULL),
           
           checkboxInput("wrap_into", "Split by ID", TRUE),
           
           checkboxInput("plot_lines", "Show lines", TRUE),
           
           selectInput("my_factor_time", label = "Select a factor as Date:", choices = NULL)
           
           )
         ),
  
  column(width = 9,
         
         tabsetPanel(
           
           tabPanel("Over Time Plot", plotOutput("timeplots", height = "500px")),
           tabPanel("Two-Time Plot", plotOutput("twotimes", height = "500px"))
           )
         )
  )

