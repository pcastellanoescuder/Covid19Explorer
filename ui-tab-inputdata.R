
tabPanel("Upload Data", 
         fluidRow(
           column(width = 3,
                  
                  fileInput("dataset","Upload your data (.xlsx):")),
           column(9,
                  div(style = 'overflow-x: scroll', DT::dataTableOutput("contents"), width = NULL, status = "primary")
                  
                  )
           )
)

