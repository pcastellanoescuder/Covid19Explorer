
tabPanel("Upload Data", 
         fluidRow(
           column(width = 3,
                  
                  fileInput("dataset","Upload your data (.xlsx):"),
                  helpText("Select the desired observations in",
                           "the 'Raw Data' panel")),
           
           column(9,
                  
                  tabsetPanel(
                    tabPanel("Raw Data", 
                             div(style = 'overflow-x: scroll', DT::dataTableOutput("contents"), width = NULL, status = "primary")),
                    tabPanel("Processed Data",
                             div(style = 'overflow-x: scroll', DT::dataTableOutput("contents_proc"), width = NULL, status = "primary"))
                  
                  ))
           )
)
