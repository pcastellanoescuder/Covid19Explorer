
observe({
  if(!is.null(processedInput())){
    
    data_factors <- processedInput() %>%
      select_if(is.factor)
    
    x <- colnames(data_factors)
    
    updateSelectInput(session, "my_factor_lasso", choices = x, selected = x[1])
    
    ##
    
    data_variables <- processedInput() %>%
      select_if(is.numeric) %>%
      dplyr::select(-time_points)
    
    y <- colnames(data_variables)
    
    updateSelectInput(session, "my_variables_lasso", choices = y, selected = y)
    
  }
})

##

LASSO <- reactive({
  
  if(!is.null(processedInput())){
    
    validate(need(length(input$my_variables_lasso) > 1, "You need more than one variable to compute lasso!"))
    
    my_factor <- input$my_factor_lasso
    my_vars <- input$my_variables_lasso
    
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
    
    idx_test <- sample(1:n, (input$test_lasso/100)*n, replace = FALSE)
    
    test <- data_subset[idx_test ,]
    test_x <- test[,-1]
    test_y <- test[,1]
    
    ## TRAIN
    
    train <- data_subset[-idx_test ,]
    train_x <- train[,-1]
    train_y <- train[,1]
    
    ## LASSO
    
    if(length(table(data_factor[,1])) > 2){
    family <- "multinomial"
    } else{
    family <- "binomial"
    }
    
    validate(need(family == "binomial", "Dependent variable (Y) must have two levels"))
    
    cv_fit <- glmnet::cv.glmnet(data.matrix(train_x), 
                                as.matrix(train_y), 
                                alpha = input$alpha_lasso, 
                                family = "binomial", 
                                nfolds = input$lasso_folds)
    
    tmp_coeffs <- coef(cv_fit, s = "lambda.min")
    final_coef <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
    
    lasso_pred <- predict(cv_fit, s = cv_fit$lambda.min, newx = data.matrix(test_x), type = "class")

    cm <- caret::confusionMatrix(as.factor(lasso_pred), as.factor(dplyr::pull(test_y, var = 1)))
    overall <- cm$overall[1]
    
    return(list(coeffs = final_coef, accuracy = overall, confusionMatrix = cm$table))
    
  } 
  
  else{
    
    return(NULL)
    
    }
  
})

##

output$coeff_lasso <- renderDataTable({
  
  DT::datatable(LASSO()$coeffs, 
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
                                        filename = paste0(Sys.Date(), "_lasso")),
                                   list(extend = "excel",
                                        filename = paste0(Sys.Date(), "_lasso")),
                                   list(extend = "pdf",
                                        filename = paste0(Sys.Date(), "_lasso"))),
                      text = "Dowload")),
                  order = list(list(2, "desc")),
                  pageLength = nrow(LASSO()$coeffs)
                ))
  
})

##

output$cm_lasso <- renderDataTable({
  
  DT::datatable(LASSO()$confusionMatrix, rownames = FALSE, class = 'cell-border stripe')
  
})

##

output$accuracy_lasso <- renderText({
  
  paste("Current Model Accuracy:", round(LASSO()$accuracy, 3))
  
})

