
observe({
  if(!is.null(processedInput())){
    
    data_factors <- processedInput() %>%
      select_if(is.factor)
    
    x <- colnames(data_factors)
    
    updateSelectInput(session, "my_factor_pca", choices = c(x, "None"), selected = "None")
    updateSelectInput(session, "my_factor_pca2", choices = c(x, "None"), selected = "None")
    
    ##
    
    data_variables <- processedInput() %>%
      select(starts_with("tn_"), starts_with("n_"))
    
    y <- colnames(data_variables)
    
    updateSelectInput(session, "my_variables_pca", choices = y, selected = y[1:2])
    updateSelectInput(session, "my_variables_pca2", choices = c(y, "None"), selected = "None")
    
  }
})

##

output$pcaplot <- renderPlot({
  
  if(!is.null(processedInput())){
    
    validate(need(length(input$my_variables_pca) > 1, "You need more than one variable to compute the PCA!"))
    
    my_factor <- input$my_factor_pca
    my_factor2 <- input$my_factor_pca2
    
    total_fac <- c(my_factor, my_factor2)
    
    my_vars <- input$my_variables_pca
    my_vars2 <- input$my_variables_pca2
    
    total_vars <- c(my_vars, my_vars2)
    
    ##
    
    data_subset <- processedInput() %>%
      select_at(vars(matches(total_vars)))
    
    data_factor <- processedInput() %>%
      select_at(vars(matches(total_fac)))
    
    data_names <- processedInput() %>%
      select_at(vars(matches("id")))
      
    data_subset <- bind_cols(data_factor, data_subset, data_names)
    
    if(!is.null(input$contents_proc_rows_selected)){
      data_subset <- data_subset[input$contents_proc_rows_selected ,]
    }
    
    ##
    
    data_names <- data_subset %>%
      select_at(vars(matches("id")))
    
    data_subset <- data_subset %>%
      select_at(vars(-matches("id")))
    
    ##
    
    idx_fac <- c(which(colnames(data_subset) %in% my_factor))
    idx_fac2 <- c(which(colnames(data_subset) %in% my_factor2))

    idx_fac_total <- c(idx_fac, idx_fac2)
    if(length(idx_fac_total) == 0){idx_fac_total <- NULL}
    
    idx_var <- c(which(colnames(data_subset) %in% my_vars))
    idx_var2 <- c(which(colnames(data_subset) %in% my_vars2))
    if(length(idx_var2) == 0){idx_var2 <- NULL}
    
    res_pca <- PCA(data_subset, ind.sup = NULL, quanti.sup = idx_var2, quali.sup = idx_fac_total, graph = F) 

    data_names <- bind_cols(data_names, as.data.frame(res_pca$ind$coord))
    
    if(input$dims_pca == "1 and 2"){
      dims <- c(1,2)
      
      data_names <- data_names %>% select(1,2,3)
      colnames(data_names) <- c("ID", "X", "Y")
    }
    else if(input$dims_pca == "2 and 3"){
      dims <- c(2,3)

      data_names <- data_names %>% select(1,3,4)
      colnames(data_names) <- c("ID", "X", "Y")
    }
    else if(input$dims_pca == "1 and 3"){
      dims <- c(1,3)

      data_names <- data_names %>% select(1,2,4)
      colnames(data_names) <- c("ID", "X", "Y")
    }
    
    ##
    
    if(is.null(idx_fac_total)){
      
      fviz_pca_biplot(res_pca, 
                      axes = dims,
                      repel = FALSE, 
                      title = "", 
                      addEllipses = input$ellipse_pca,
                      label = "var",
                      col.var = "red",
                      # palette = input$pca_palette,
                      alpha.ind = 0.8) +
        theme_bw() +
        {if(input$labs_pca)geom_text(data = data_names, aes(x = X, y = Y, label = ID))} +
        theme(legend.position = "top") 

    } 
    else{
      
      fviz_pca_biplot(res_pca, 
                      axes = dims,
                      repel = FALSE, 
                      title = "", 
                      habillage = idx_fac_total, 
                      addEllipses = input$ellipse_pca,
                      label = "var",
                      col.var = "red",
                      # palette = input$pca_palette,
                      alpha.ind = 0.8) +
        theme_bw() +
        {if(input$labs_pca)geom_text(data = data_names, aes(x = X, y = Y, label = ID))} +
        theme(legend.position = "top") 
    }
    
  } 
  else{
    return(NULL)
  }
  
})

