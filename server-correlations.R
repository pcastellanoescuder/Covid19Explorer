
observe({
  if(!is.null(processedInput())){
    x <- colnames(processedInput())
    idx <- map(processedInput(), is.numeric) %>% unlist()
    updateSelectInput(session, "one", choices = x[idx], selected = x[grepl("il6", x)])
    updateSelectInput(session, "two", choices = x[idx], selected = x[grepl("pcr", x)])
  }
})

output$cor_plot <- renderPlotly({
  
  if(is.null(processedInput())){
    return(NULL)
  } 
  else{
    
    data_subset <- processedInput()
    
    data_subset1 <- as.data.frame(data_subset[, colnames(data_subset) == as.character(input$one)])
    data_subset2 <- as.data.frame(data_subset[, colnames(data_subset) == as.character(input$two)])
    
    data_subset <- bind_cols(data_subset1, data_subset2)
    colnames(data_subset) <- c("Variable1", "Variable2")
    
    my_corr_plot <- ggplot(data_subset, aes(x = Variable1, y = Variable2)) + 
      geom_point(alpha = 0.8, size = 1.5) +
      xlab(paste0("log(", as.character(input$one), ")")) + 
      ylab(paste0("log(", as.character(input$two), ")")) + 
      ggtitle(paste0("R = ", round(cor(data_subset$Variable1, data_subset$Variable2, method = input$corr_method, use = "complete.obs"), 2))) +
      # "pvalue = ", round(cor.test(data_subset$Variable1, data_subset$Variable1,method = input$corr_method)$p.value,3)))
      theme_bw() + 
      {if(isTRUE(input$smooth))geom_smooth(method = lm, color = input$smooth_color, na.rm = TRUE)} +
      theme(legend.position = "none") 
    
    plotly::ggplotly(my_corr_plot)
    
  }
  
})

####

# output$corr_table <- renderDataTable({
#   
#   if(is.null(processedInput())){
#     return(NULL)
#   } 
#   else{
#     
#     data_subset <- processedInput() 
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

