
datasetInput <- reactive({
  
  infile <- input$dataset
  
  if(is.null(infile)){
    return(NULL)
    }
  else {
    data <- readxl::read_excel(infile$datapath)
    colnames(data) <- janitor::make_clean_names(colnames(data))
    data <- as.data.frame(data)
    return(data)}
})

####

output$contents <- DT::renderDataTable(datasetInput(), class = 'cell-border stripe', rownames = FALSE, server = TRUE)

