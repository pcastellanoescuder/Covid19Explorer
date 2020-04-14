
tabPanel("Upload Data", 
         fluidRow(
           column(width = 3,
                  
                  wellPanel(
                    
                    fileInput("dataset","Upload your data (.xlsx):"),
                    
                    radioButtons("summarize_points", "Summarize data for each subject:", 
                                 choices = c("Show all observations" = 'all',
                                             "Show the first and last observation" = 'first_last',
                                             "Show the first observation" = 'first',
                                             "Show the last observation" = 'last'),
                                 selected = 'all'),
                    
                    prettySwitch("showadvanced", "Advanced settings", fill = T, status = "warning"),
                    
                    conditionalPanel(condition = "input.showadvanced",
                                     
                                     sliderInput("removeNA", "Allowed % missing values in each column:", min = 0, max = 100, step = 1, value = 70),
                                     
                                     numericInput("replaceZeros", "Replace zeros by:", value = 0.01),
                                     
                                     selectizeInput("transformation_log", "log transformation:", choices = NULL, multiple = TRUE),
                                     selectizeInput("transformation_log2", "log2 transformation:", choices = NULL, multiple = TRUE),
                                     selectizeInput("transformation_log10", "log10 transformation:", choices = NULL, multiple = TRUE),
                                     selectizeInput("transformation_sqrt", "sqrt transformation:", choices = NULL, multiple = TRUE)
                                     
                                     ),
                    
                    actionButton("process","Process", icon("step-forward"),
                                 style="color: #fff; background-color: #00b300; border-color: #009900") %>% helper(type = "markdown",
                                                                                                                   title = "Process Data Helper",
                                                                                                                   content = "process",
                                                                                                                   icon = "question",
                                                                                                                   colour = "green")
                    )
         ),
           
           column(9,
                  
                  tabsetPanel(
                    tabPanel("Processed Data", DT::dataTableOutput("contents_proc")),
                    tabPanel("Raw Data", DT::dataTableOutput("contents"))
                  ))
         )
)
