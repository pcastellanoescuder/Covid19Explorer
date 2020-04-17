
observe({
  if(!is.null(processedInput())){
    
    data_subset <- processedInput() %>%
      select_if(is.factor)
    
    x <- colnames(data_subset)
    
    updateSelectInput(session, "my_factor_pca", choices = x, selected = x[1])
  }
})

##

output$pcaplot <- renderPlot({
  
  if(!is.null(processedInput())){
    
    my_factor <- input$my_factor_pca
    
    data_subset <- processedInput() %>%
      select_at(vars(ends_with("_proc")))
    
    data_factor <- processedInput() %>%
      select_at(vars(matches(my_factor)))
    
    data_subset <- bind_cols(data_factor, data_subset)
    
    if(!is.null(input$contents_proc_rows_selected)){
      data_subset <- data_subset[input$contents_proc_rows_selected ,]
    }
    
    data_subset <- data_subset %>%
      select_at(vars(ends_with("_proc")))
    
    data_factor <- data_subset %>%
      select_at(vars(matches(my_factor)))
    
    res_pca1 <- mixOmics::pca(data_subset)
    res_pca <- bind_cols(data_factor, res_pca1$x)
    
    ggplot(res_pca) +
      geom_point(aes(PC1, PC2, color = my_factor, shape = my_factor), size = 3, alpha = 0.8) +
      {if(input$labs_pca == 'yes')geom_label(aes(label = rownames(res_pca)), show.legend = F)} +
      theme_bw() +
      xlab(paste0("PC1 (", round(100*(res_pca1$explained_variance)[1], 2), "%)")) +
      ylab(paste0("PC2 (", round(100*(res_pca1$explained_variance)[2], 2), "%)"))
    
  } 
  else{
    return(NULL)
  }
  
})

