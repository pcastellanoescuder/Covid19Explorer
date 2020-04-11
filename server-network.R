
output$network_plot <- renderPlot({
  
  data_subset <- processedInput() %>%
    select(-complete_vars, -time_points)
  
  if(!is.null(input$contents_proc_rows_selected)){
    data_subset <- data_subset[input$contents_proc_rows_selected ,]
  } 
  else{
    data_subset <- data_subset
  }
  
  data_numeric <- data_subset %>%
    select_if(is.numeric)
  
  cor_matrix2 <- round(cor(data_numeric, use = "pairwise.complete.obs"), 3)
  
  qgraph(cor_matrix2, graph = input$corr_type, layout = input$layout, threshold = input$threshold, 
         edge.labels = input$network_labels)
    
})