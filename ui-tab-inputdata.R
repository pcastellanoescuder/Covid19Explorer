
tabPanel("Upload Data", 
         fluidRow(
           column(width = 3,
                  
                  wellPanel(
                    
                    fileInput("dataset","Upload your data (.xlsx):"),
                    
                    sliderInput("removeNA", "Allowed % missing values in each column:", min = 0, max = 100, step = 1, value = 70),
                    
                    numericInput("replaceZeros", "Replace zeros by:", value = 0.01),
                    
                    selectizeInput("transformation_type", "Transformation type:", choices = c("log", "log2", "log10", "sqrt")),
                    
                    checkboxInput("first_last", "Show only the first and last observation of each subject", FALSE),
                    
                    helpText(strong("Select the desired observations in",
                             "the 'Processed Data' panel")))
         ),
           
           column(9,
                  
                  tabsetPanel(
                    tabPanel("Processed Data", DT::dataTableOutput("contents_proc")),
                    tabPanel("Raw Data", DT::dataTableOutput("contents"))
                  ))
         )
)
