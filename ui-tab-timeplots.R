fluidRow(
  column(width = 3,
         wellPanel(
           
           selectizeInput("features", label = "Select your features:", choices = NULL, multiple = TRUE),
           
           radioButtons("wrap_into", "Split by:",
                        c("No split" = "none",
                          "Gender" = "gender",
                          "Subject" = "subject",
                          "Service" = "service"),
                        selected = "subject"),
           
           checkboxInput("plot_lines", "Show lines", TRUE)
  
  )),
  
  column(width = 9,
         
         plotlyOutput("timeplots", height = "500px")
         
         )
  )

