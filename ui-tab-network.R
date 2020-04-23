
fluidRow(
  column(width = 3,
                wellPanel(
                  
                  radioButtons("corr_type", label = "Select type of network:", 
                               choices = c("Correlation network" = 'cor', 
                                           "Partial correlation network" = 'pcor',
                                           "Gaussian graphical model" = 'glasso')),
                  
                  conditionalPanel(condition = "input.corr_type == 'glasso'",
                                   sliderInput("regularization", "Regularization parameter:", min = 0.1, max = 1, value = 0.5)),
                  
                  conditionalPanel(condition = "input.corr_type != 'glasso'",
                                   sliderInput("threshold", "Threshold:",  value = 0.4, min = 0, max = 1, step = 0.1)),

                  selectInput("layout", label = "Select layout:", choices = c("spring", "circle")),
                  
                  numericInput("number_char_nod", label = "Select the number of characters to display in each node:", value = 5),
                  
                  checkboxInput("network_labels", "Show edge labels", FALSE)
  )),
  
  column(width = 8,
         
         fluidPage(
           
           plotOutput("network_plot", height = "600px")
           
           )

  ))

