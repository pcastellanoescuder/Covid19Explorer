
fluidPage(column(width = 3,
                 
                 wellPanel(
                   
                   sliderInput("test_rf", "Test Partition", min = 0, max = 100, step = 5, value = 20),
                   
                   selectInput("my_factor_rf", label = "Dependent Variable (Y):", choices = NULL),
                   
                   selectInput("my_variables_rf", label = "Independent Variables (X):", choices = NULL, multiple = T),
                   
                   actionButton("run_rf", "Run", icon("step-forward"),
                                style="color: #fff; background-color: #00b300; border-color: #009900")

                   )
                 ),
          
          column(width = 9,
                 
                 fluidRow(
                   valueBoxOutput("accuracy_rf", width = 5),
                 ),
                 
                 tabsetPanel(
                   tabPanel("Feature Importance", DT::dataTableOutput("coeff_rf")),
                   tabPanel("Confusion Matrix", DT::dataTableOutput("cm_rf"))
                   )
                 
                 )
          )

