
fluidPage(column(width = 3,
                 
                 wellPanel(
                   
                   selectInput("my_factor_pca", label = "Select a factor (optional):", choices = NULL),
                   
                   selectInput("my_factor_pca2", label = "Select a second factor (optional):", choices = NULL),
                   
                   selectInput("my_variables_pca", label = "Select main variables:", choices = NULL, multiple = T),
                   
                   selectInput("my_variables_pca2", label = "Select secondary variables (optional):", choices = NULL, multiple = T),
                   
                   radioButtons("dims_pca", "Show dimensions:", choices = c("1 and 2", "2 and 3", "1 and 3"), selected = "1 and 2"),
                   
                   checkboxInput("load_pca", "Show loadings", FALSE),
                   
                   checkboxInput("labs_pca", "Show labels", FALSE),
                   
                   checkboxInput("ellipse_pca", "Plot ellipses", FALSE)
                   )
                 ),
          
          column(width = 9,
                 
                 plotOutput("pcaplot", height = "600px")
                 
                 )
          )

