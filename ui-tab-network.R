
fluidRow(
  column(width = 3,
                wellPanel(
                  
                  radioButtons("corr_type", label = "Select type of network:", 
                               choices = c("Correlation network" = 'cor', 
                                           "Partial correlation network" = 'pcor')),
                  
                  selectInput("layout", label = "Select layout:", choices = c("spring", "circle")),
                  
                  sliderInput("threshold", "Threshold:",  value = 0.4, min = 0, max = 1, step = 0.1),

                  checkboxInput("network_labels", "Show labels", FALSE)
  )),
  
  column(width = 8,
         
         fluidPage(
           
           plotOutput("network_plot", height = "600px")
           
           )

  ))

