
fluidRow(
  column(width = 3,
         
         wellPanel(
           
           fileInput("dataset","Upload your data:", accept = c(".xlsx", ".csv")),
           
           radioButtons("readCSV", "Data format:", choices = c("xlsx", "csv"), selected = "xlsx"),
           
           conditionalPanel(condition = "input.readCSV == 'csv'",
                            radioButtons("separator", "Separator:", choices = c(",", ";"), selected = ";", inline = T),
                            helpText("Decimal mark should be a dot")),
           
           radioButtons("summarize_points", "Summarize data for each subject:", 
                        choices = c("Show all observations" = 'all',
                                    "Show the first and last observation" = 'first_last',
                                    "Show the first observation" = 'first',
                                    "Show the last observation" = 'last'),
                        selected = 'all'),
           
           prettySwitch("showadvanced", "Advanced settings", fill = T, status = "warning"),
           
           conditionalPanel(condition = "input.showadvanced",
                            
                            checkboxInput("remove_first", "Remove first row", FALSE),
                            
                            checkboxInput("trans_data", "Format your date column (from xlsx to date)", TRUE),
                            
                            conditionalPanel(condition = "!input.trans_data",
                                             radioButtons("date_format", "Select your date column format:",
                                                          choices = c("yyyy/mm/dd" = 'ymd',
                                                                      "dd/mm/yyyy" = 'dmy',
                                                                      "mm/dd/yyyy" = 'mdy'),
                                                          selected = 'dmy')),
                            
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
         
         bsCollapse(id = "input_collapse_panel", open = "raw_panel", multiple = FALSE,
                    
                    bsCollapsePanel(title = "Raw Data", value = "raw_panel", DT::dataTableOutput("contents")),
                    bsCollapsePanel(title = "Processed Data", value = "proc_panel", DT::dataTableOutput("contents_proc"))
                    
         )
         )
)

