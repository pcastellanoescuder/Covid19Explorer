
fluidPage(column(width = 3,
                 
                 wellPanel(
                   
                   selectInput("my_factor_pca", label = "Select a factor:", choices = NULL),
                   
                   radioButtons("labs_pca", "Show labels:", choices = c("Yes" = 'yes', 
                                                                        "No" = 'no'), 
                                selected = 'yes')
                   )
                 ),
          
          column(width = 9,
                 
                 plotOutput("pcaplot")
                 
                 )
          )

