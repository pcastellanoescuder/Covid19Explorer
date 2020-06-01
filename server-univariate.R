observe_helpers(help_dir = "help_mds")

observe({
  if(!is.null(processedInput())){
    
    # numeric
    data_num <- processedInput() %>%
      dplyr::select(-time_points) %>%
      select_if(is.numeric)
    
    x <- colnames(data_num)

    updateSelectInput(session, "feat_uni", choices = x, selected = x[1])
    
    # factor
    data_fac <- processedInput() %>%
      dplyr::select(-time_points) %>%
      select_if(is.factor)
    
    y <- colnames(data_fac)
    
    updateSelectInput(session, "fact_uni", choices = y, selected = y[1])
  }
})

##

PREPARE_UNI <- reactive({
  
  data_subset <- processedInput() %>%
    dplyr::select(-time_points) %>%
    select_if(is.numeric)

  data_fact <- processedInput() %>%
    select_at(vars(matches(input$fact_uni)))
  colnames(data_fact) <- "my_uni_factor"
  
  data_subset <- bind_cols(data_fact, data_subset)
  
  if(!is.null(input$contents_proc_rows_selected)){
    data_subset <- data_subset[input$contents_proc_rows_selected ,]
  } 

  return(data_subset)
  
})

##

Univ_analisis <- 
  eventReactive(input$play_test,
                ignoreNULL = TRUE, {
                  withProgress(message = "Please wait",{
                    
                    data <- PREPARE_UNI()
                    
                    target <- data %>%
                      select(my_uni_factor) %>%
                      rownames_to_column("id")
                    
                    features <- data %>%
                      select(-my_uni_factor)
                    
                    data <- POMA::PomaMSnSetClass(target = target, features = features)
                    
                    ##
                    
                    if (input$univariate_test == "ttest"){
                      
                      param_ttest <- POMA::PomaUnivariate(data, method = "ttest", 
                                                          paired = input$paired_ttest, 
                                                          var_equal = input$var_ttest)
                      return(list(param_ttest = param_ttest))
                    }
                    
                    ##
                    
                    else if (input$univariate_test == "anova"){
                      
                      param_anova <- POMA::PomaUnivariate(data, method = "anova", covariates = FALSE)
                      return(list(param_anova = param_anova))

                    }
                    
                    ##
                    
                    else if (input$univariate_test == "mann"){
                      
                      non_param_mann <- POMA::PomaUnivariate(data, method = "mann", paired = input$paired_mann)
                      return(list(non_param_mann = non_param_mann))
                      
                    }
                    
                    ##
                    
                    else if (input$univariate_test == "kruskal"){
                      
                      non_param_kru <- POMA::PomaUnivariate(data, method = "kruskal")
                      return(list(non_param_kru = non_param_kru))
                    }
                    })
                  })

####

output$matriu_anova <- DT::renderDataTable({
  
  param_anova <- Univ_analisis()$param_anova
  DT::datatable(param_anova,
                filter = 'top',extensions = 'Buttons',
                escape=FALSE,  rownames=TRUE, class = 'cell-border stripe',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = 
                    list("copy", "print", list(
                      extend="collection",
                      buttons=list(list(extend="csv",
                                        filename="anova_results"),
                                   list(extend="excel",
                                        filename="anova_results"),
                                   list(extend="pdf",
                                        filename="anova_results")),
                      text="Dowload")),
                  order=list(list(2, "desc")),
                  pageLength = nrow(param_anova)))
})

output$matriu_ttest <- DT::renderDataTable({

  param_ttest <- Univ_analisis()$param_ttest
                
        DT::datatable(param_ttest,
                filter = 'top',extensions = 'Buttons',
                escape=FALSE,  rownames=TRUE, class = 'cell-border stripe',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = 
                    list("copy", "print", list(
                      extend="collection",
                      buttons=list(list(extend="csv",
                                        filename="ttest_results"),
                                   list(extend="excel",
                                        filename="ttest_results"),
                                   list(extend="pdf",
                                        filename="ttest_results")),
                      text="Dowload")),
                  order=list(list(2, "desc")),
                  pageLength = nrow(param_ttest)))
})

###

output$matriu_mann <- DT::renderDataTable({
  
  non_param_mann <- Univ_analisis()$non_param_mann
    
  DT::datatable(non_param_mann, 
                filter = 'top',extensions = 'Buttons',
                escape=FALSE,  rownames=TRUE, class = 'cell-border stripe',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = 
                    list("copy", "print", list(
                      extend="collection",
                      buttons=list(list(extend="csv",
                                        filename="mann_whitney_results"),
                                   list(extend="excel",
                                        filename="mann_whitney_results"),
                                   list(extend="pdf",
                                        filename="mann_whitney_results")),
                      text="Dowload")),
                  order=list(list(2, "desc")),
                  pageLength = nrow(non_param_mann)))
})

###

output$matriu_kruskal <- DT::renderDataTable({
  
  non_param_kru <- Univ_analisis()$non_param_kru
  
  DT::datatable(non_param_kru, 
                filter = 'top',extensions = 'Buttons',
                escape=FALSE,  rownames=TRUE, class = 'cell-border stripe',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = 
                    list("copy", "print", list(
                      extend="collection",
                      buttons=list(list(extend="csv",
                                        filename="kruskal_wallis_results"),
                                   list(extend="excel",
                                        filename="kruskal_wallis_results"),
                                   list(extend="pdf",
                                        filename="kruskal_wallis_results")),
                      text="Dowload")),
                  order=list(list(2, "desc")),
                  pageLength = nrow(non_param_kru)))
})

##

output$densityplots_proc_boxplot <- renderPlot({
  
  mynum <- input$feat_uni
  myfac <- input$fact_uni
  
  ##
  
  data_fac <- processedInput() %>%
    dplyr::select_at(vars(matches(myfac)))
  data_num <- processedInput() %>%
    dplyr::select_at(vars(matches(mynum)))
  data_subset <- cbind(data_fac, data_num)
  colnames(data_subset) <- c("my_factor", "my_numeric")
  
  if(!is.null(input$contents_proc_rows_selected)){
    data_subset <- data_subset[input$contents_proc_rows_selected ,]
  } 
  
  if(input$univariate_test %in% c("ttest", "anova")){
    type_boxplot <- "parametric"
  } else{
    type_boxplot <- "nonparametric"
  }
  
  ##
  
  if(input$var_ttest){
    
    ggstatsplot::ggbetweenstats(
      data = data_subset,
      x = my_factor,
      y = my_numeric,
      title = "",
      messages = FALSE,
      type = type_boxplot,
      var.equal = TRUE,
      pairwise.display = "all",
      pairwise.comparisons = TRUE,
      p.adjust.method = "holm",
      results.subtitle = TRUE,
      xlab = myfac,
      ylab = mynum)
    
  } else{
    
    ggstatsplot::ggbetweenstats(
      data = data_subset,
      x = my_factor,
      y = my_numeric,
      title = "",
      messages = FALSE,
      type = type_boxplot,
      var.equal = FALSE,
      pairwise.display = "all",
      pairwise.comparisons = TRUE,
      p.adjust.method = "holm",
      results.subtitle = TRUE,
      xlab = myfac,
      ylab = mynum)
  }
  
})

