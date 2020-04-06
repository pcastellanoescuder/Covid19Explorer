
observe({
  if(!is.null(datasetInput())){
    x <- colnames(datasetInput())
    updateSelectInput(session, "groups", choices = x, selected = x[grepl("gende", x)])
    updateSelectInput(session, "mult_vars", choices = x, selected = x[grepl("pcr", x) | grepl("ig6", x) | grepl("neutr", x)])
  }
})

output$pca_plot <- renderPlotly({
  
  if(is.null(datasetInput())){
    return(NULL)
  } 
  else{
    
    data_subset <- datasetInput() 
    
    if(!is.null(input$contents_rows_selected)){
      data_subset <- data_subset[input$contents_rows_selected ,]
    }
    
    data_subset <- data_subset %>%
      mutate(data_calendar = dmy(data_calendar)) %>%
      mutate_at(c("edad_number", "numero", "codi_extern"), as.character) %>%
      mutate_if(is.numeric, log) 

    data_subset1 <- as.data.frame(data_subset[, colnames(data_subset) == as.character(input$groups)])
    data_subset2 <- as.data.frame(data_subset[, colnames(data_subset) %in% as.character(input$mult_vars)])
    
    res_pca <- prcomp(na.omit(data_subset2), scale = TRUE, center = TRUE) 
    
    res_pca2 <- data.frame(res_pca$x) %>% select(PC1, PC2)
    # res_pca2 <- bind_cols(res_pca2, data_subset1)
    # colnames(res_pca2)[3] <- "Group"
    
    #####
    
    my_pca_plot <- ggplot(res_pca2, aes(x = PC1, y = PC2, color = Group, shape = Group))+
      geom_point(size = 3, alpha = 0.5) + 
      xlab(paste0("PC1 (", round(100*((res_pca$sdev/sum(res_pca$sdev))[1]), 2), "%)")) +
      ylab(paste0("PC2 (", round(100*((res_pca$sdev/sum(res_pca$sdev))[2]), 2), "%)")) +
      theme_bw()
    
    plotly::ggplotly(my_pca_plot)
    
  }
  
})

####

# output$corr_table <- renderDataTable({
#   
#   if(is.null(datasetInput())){
#     return(NULL)
#   } 
#   else{
#     
#     data_subset <- datasetInput() 
#     
#     if(!is.null(input$contents_rows_selected)){
#       data_subset <- data_subset[input$contents_rows_selected ,]
#     }
#     
#     data_subset <- data_subset %>%
#       mutate(data_calendar = dmy(data_calendar)) %>%
#       mutate_at(c("edad_number", "numero", "codi_extern"), as.character) %>%
#       mutate_if(is.numeric, log) %>%
#       select_if(is.numeric)
# 
#     M <- cor(data_subset, method = input$corr_method, use = "complete.obs")
#     ## to pairwise
#     pairwiseC <- M
#     pairwiseC[lower.tri(pairwiseC, diag = TRUE)] = NA # put NA
#     pairwiseC <- as.data.frame(as.table(pairwiseC)) # as a dataframe
#     pairwiseC <- na.omit(pairwiseC) # remove NA
#     pairwiseC <- pairwiseC[with(pairwiseC, order(-Freq)), ] # order by correlation
#     colnames(pairwiseC)[3] <- "R"
#   
#   }
# 
# })

