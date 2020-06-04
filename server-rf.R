
observe({
  if(!is.null(processedInput())){
    
    data_factors <- processedInput() %>%
      select_if(is.factor)
    
    x <- colnames(data_factors)
    
    updateSelectInput(session, "my_factor_rf", choices = x, selected = x[1])
    
    ##
    
    data_variables <- processedInput() %>%
      select_if(is.numeric) %>%
      dplyr::select(-time_points)
    
    y <- colnames(data_variables)
    
    updateSelectInput(session, "my_variables_rf", choices = y, selected = y)
    
  }
})

##

RandomF <- reactive({
  
  if(!is.null(processedInput())){
    
    validate(need(length(input$my_variables_rf) > 1, "You need more than one variable to compute a random forest!"))
    
    my_factor <- input$my_factor_rf
    my_vars <- input$my_variables_rf
    
    ##
    
    data_subset <- processedInput() %>%
      select_at(vars(matches(my_vars))) %>% 
      mutate_all(~ replace(., is.na(.), median(., na.rm = TRUE)))
    
    data_factor <- processedInput() %>%
      select_at(vars(matches(my_factor)))
      
    data_subset <- bind_cols(data_factor, data_subset)
    
    if(!is.null(input$contents_proc_rows_selected)){
      data_subset <- data_subset[input$contents_proc_rows_selected ,]
    }
    
    ## SIZE
    
    n <- nrow(data_subset)
    
    ## TEST
    
    idx_test <- sample(1:n, (input$test_rf/100)*n, replace = FALSE)
    
    test <- data_subset[idx_test ,]
    test_x <- test[,-1] %>% mutate_all(as.numeric)
    test_y <- test[,1]
    
    ## TRAIN
    
    train <- data_subset[-idx_test ,]
    train_x <- train[,-1] %>% mutate_all(as.numeric)
    train_y <- train[,1]
    
    ## RANDOM FOREST
    
    model <- randomForest::randomForest(train_x, as.factor(dplyr::pull(train_y, var = 1)))
    rf_pred <- predict(model, newdata = test_x, type = "class")
    
    final_coef <- as.data.frame(randomForest::importance(model, scale = TRUE)) %>%
      rownames_to_column("feature") %>%
      mutate(MeanDecreaseGini = round(MeanDecreaseGini, 3)) %>%
      arrange(desc(MeanDecreaseGini))

    cm <- caret::confusionMatrix(as.factor(rf_pred), as.factor(dplyr::pull(test_y, var = 1)))
    overall <- cm$overall[1]
    
    return(list(coeffs = final_coef, accuracy = overall, confusionMatrix = cm$table))
    
  } 
  
  else{
    
    return(NULL)
    
    }
  
})

##

output$coeff_rf <- renderDataTable({
  
  DT::datatable(RandomF()$coeffs, 
                filter = 'none', extensions = 'Buttons',
                escape = FALSE,  rownames = TRUE, 
                class = 'cell-border stripe',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons =
                    list("copy", "print", list(
                      extend="collection",
                      buttons=list(list(extend = "csv",
                                        filename = paste0(Sys.Date(), "_randomForest")),
                                   list(extend = "excel",
                                        filename = paste0(Sys.Date(), "_randomForest")),
                                   list(extend = "pdf",
                                        filename = paste0(Sys.Date(), "_randomForest"))),
                      text = "Dowload")),
                  order = list(list(2, "desc")),
                  pageLength = nrow(RandomF()$coeffs)
                ))
  
})

##

output$cm_rf <- renderDataTable({
  
  DT::datatable(RandomF()$confusionMatrix, rownames = FALSE, class = 'cell-border stripe')
  
})

##

output$accuracy_rf <- renderText({
  
  paste("Current Model Accuracy:", round(RandomF()$accuracy, 3))
  
})

