
output$network_plot <- renderPlot({
  
  data_numeric <- processedInput() %>% 
    select_if(is.numeric) 

  ## Imputation
  
  # data_numeric <- data.frame(t(impute::impute.knn(t(data_numeric), colmax = 100)$data))
  
  data_numeric <- data_numeric %>%
    select(-starts_with("troponi")) %>%
    select(-starts_with("vsg")) %>%
    select(-starts_with("s_cd25")) %>%
    select(-starts_with("srm_trig")) # too much NA's
    
  ##
  
  cor_matrix2 <- round(cor(data_numeric, use = "pairwise.complete.obs"), 3)
  
  qgraph(cor_matrix2, graph = input$corr_type, layout = input$layout, threshold = input$threshold, 
         edge.labels = input$network_labels)
    
})
