
tabPanel("Upload Data", 
         fluidRow(
           column(width = 3,
                  
                  fileInput("dataset","Upload your data (.xlsx):"),
                  helpText("Select the desired observations in",
                           "the 'Raw Data' panel")),
           
           column(9,
                  
                  tabsetPanel(
                    tabPanel("Raw Data", DT::dataTableOutput("contents")),
                    tabPanel("Processed Data", DT::dataTableOutput("contents_proc"))
                  
                  ))
           )
)
