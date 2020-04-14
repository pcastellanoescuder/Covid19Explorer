# 
# observe({
#   if(!is.null(processedInput())){
# 
#     data_subset <- processedInput() %>%
#       select(vars(ends_with("_proc")))
#     
#     data_subset2 <- processedInput() %>%
#       select_if(is.factor)
# 
#     x <- colnames(data_subset)
#     y <- colnames(data_subset2)
#     
#     updateSelectInput(session, "dens_feat", choices = x, selected = x[1])
#     updateSelectInput(session, "my_factor2", choices = c("None", y), selected = "None")
#   }
# })
# 
# ##
# 
# output$densityplots <- renderPlot({
# 
#   if(is.null(processedInput())){
#     return(NULL)
#   }
#   else{
#     
#     data_subset <- processedInput()
#     
#     if(!is.null(input$contents_proc_rows_selected)){
#       data_subset <- data_subset[input$contents_proc_rows_selected ,]
#     }
# 
#     
#     
#     
#   }
#   
# }}
# })




