
output$correlogram_plot <- renderPlot({
  
  data_subset <- processedInput() %>%
    dplyr::select(-time_points)
  
  if(!is.null(input$contents_proc_rows_selected)){
    data_subset <- data_subset[input$contents_proc_rows_selected ,]
  } 
  
  data_numeric <- data_subset %>%
    select_if(is.numeric)
  
  cor_matrix <- round(cor(data_numeric, use = "pairwise.complete.obs"), 3)
    
  my_cols <- c(input$color_two, input$color_outline, input$color_one)
    
  ggcorrplot(cor_matrix, method = input$method_corrplot, lab = input$labels_corrplot, type = input$type_corrplot,    
             ggtheme = ggplot2::theme_bw,
             colors = my_cols, legend.title = "Correlation")

})
