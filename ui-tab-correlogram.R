
fluidRow(
  column(width = 3,
                wellPanel(
                  
                  selectInput("method_corrplot", label = "Select method:", choices = c("circle", "square")),
                  
                  selectInput("type_corrplot", label = "Select type:", choices = c("full", "lower")),
                
                  textInput("color_one", "Color max:", value = "blue"),
                  textInput("color_outline", "Outline color:", value = "white"),
                  textInput("color_two", "Color min:", value = "red"),
                  
                  checkboxInput("labels_corrplot", "Show labels")

  )),
  
  column(width = 8,
         
         fluidPage(
           
           plotOutput("correlogram_plot", height = "600px", width = "120%")
           
           )

  ))

