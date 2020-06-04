
fluidPage(column(width = 3,
                 
                 wellPanel(
                   
                   sliderInput("alpha_lasso", "Elasticnet Mixing Parameter", min = 0, max = 1, step = 0.1, value = 1),
                   
                   sliderInput("test_lasso", "Test Partition", min = 0, max = 100, step = 5, value = 20),
                   
                   numericInput("lasso_folds", "Cross-Validation Folds Number", value = 10),
                   
                   selectInput("my_factor_lasso", label = "Dependent Variable (Y):", choices = NULL),
                   
                   selectInput("my_variables_lasso", label = "Independent Variables (X):", choices = NULL, multiple = T)

                   )
                 ),
          
          column(width = 9,
                 
                 textOutput("accuracy_lasso"),
                 
                 br(),
                 
                 tabsetPanel(
                   tabPanel("Selected Features", DT::dataTableOutput("coeff_lasso")),
                   tabPanel("Confusion Matrix", DT::dataTableOutput("cm_lasso"))
                   )
                 
                 )
          )
