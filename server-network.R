
output$network_plot <- renderPlot({
  
  data_subset <- processedInput() %>%
    dplyr::select(-time_points)
  
  if(!is.null(input$contents_proc_rows_selected)){
    data_subset <- data_subset[input$contents_proc_rows_selected ,]
  } 
  
  data_numeric <- data_subset %>%
    select_if(is.numeric)
  
  cor_matrix2 <- round(cor(data_numeric, use = "pairwise.complete.obs"), 3)
  
  node_names <- gsub("tn_", "", rownames(cor_matrix2))
  node_names <- gsub("n_", "", node_names)
  
  if(input$corr_type != "glasso"){
    
    qgraph(cor_matrix2, 
           graph = input$corr_type, 
           layout = input$layout, 
           threshold = input$threshold, 
           labels = substr(node_names, start = 1, stop = input$number_char_nod),
           edge.labels = input$network_labels)
  } else{
    
    data_glasso <- glasso(cor_matrix2, input$regularization)
    qgraph(data_glasso, 
           layout = input$layout, 
           labels = substr(node_names, start = 1, stop = input$number_char_nod),
           edge.labels = input$network_labels)
  }
    
})
