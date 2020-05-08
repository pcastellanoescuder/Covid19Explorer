
observe({
  if(!is.null(processedInput())){

    # numeric
    data_num <- processedInput() %>%
      dplyr::select(-time_points) %>%
      select_if(is.numeric)
    
    x <- colnames(data_num)
    
    updateSelectInput(session, "dens_feat_proc", choices = c("All features", x), selected = "All features")
    
    # factor
    data_fac <- processedInput() %>%
      dplyr::select(-time_points) %>%
      select_if(is.factor)
    
    y <- colnames(data_fac)
    
    updateSelectInput(session, "dens_fact", choices = c("None", y), selected = "None")
  }
})

##

# observe({
#   if(!is.null(processedInput()) & input$dens_fact != "none"){
#     
#     # factor levels
#     data_fac <- processedInput() %>%
#       dplyr::select(input$dens_fact)
#     colnames(data_fac) <- "selected_factor"
#     
#     z <- levels(data_fac$selected_factor)
#     
#     updateSelectInput(session, "dens_fact_levels", choices = c("All", z), selected = "All")
#   }
# })

##

DESFUN_PROC <- reactive({
  
  data_subset <- processedInput() %>%
    dplyr::select(-time_points)
  
  if(!is.null(input$contents_proc_rows_selected)){
    data_subset <- data_subset[input$contents_proc_rows_selected ,]
  } 

  my_table <- data_subset %>%
    skim() %>%
    dplyr::rename(type = skim_type, variable = skim_variable, percent_complete = complete_rate) %>%
    mutate(percent_complete = percent_complete*100) %>%
    filter(variable != "id") %>%
    filter(variable != "date") %>%
    mutate_if(is.numeric, round, 3) %>%
    dplyr::select(-c(character.min:factor.n_unique)) %>%
    dplyr::rename(Freq = factor.top_counts, mean = numeric.mean, sd = numeric.sd, min = numeric.p0, 
                  p25 = numeric.p25, median = numeric.p50, p75 = numeric.p75,
                  max = numeric.p100, histogram = numeric.hist)
  
  my_table_num <- data_subset %>%
    select_if(is.numeric)
  
  return(list(my_table = my_table, my_table_num = my_table_num))
  
})

##

output$descriptive_proc <- renderDataTable({
  
  my_table <- DESFUN_PROC()$my_table
  datatable(my_table, rownames = FALSE, options = list(scrollX = TRUE))
  
})

##

output$densityplots_proc <- renderPlot({
  
  data_subset <- DESFUN_PROC()$my_table_num
  
  myvar <- input$dens_feat_proc
  
  if(myvar == "All features"){
    
    data_subset <- stack(data_subset)
    
    ggplot(data_subset, aes(x = values, color = ind)) +
      geom_line(stat = "density") +
      theme_bw() +
      xlab("Value") +
      ylab("Density") +
      facet_wrap(~ ind, scales = "free") +
      theme(legend.position = "none",
            axis.title.x = element_text(size = 12))
    
  }
  else{
    
    data_subset <- data_subset %>%
      select_at(vars(matches(myvar)))
    colnames(data_subset) <- "Variable"
    
    p1 <- ggplot(data_subset, aes(Variable)) +
      geom_line(stat = "density") +
      theme_bw() +
      ggtitle(myvar) +
      xlab("Value") +
      ylab("Density") +
      theme(legend.position = "none",
            axis.title.x = element_text(size = 12))
    
    p2 <- ggplot(data_subset) +
      geom_boxplot(aes(x = "Variable", y = Variable)) +
      theme_bw() +
      ggtitle(myvar) +
      xlab("") +
      ylab("Value") +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
    
    p1 + p2
    
  }
  
})

##

output$densityplots_proc_boxplot <- renderPlot({
  
  validate(need(input$dens_fact != "None" & input$dens_feat_proc != "All features", 
                "Please, select one factor and one numeric variable"))
  
  mynum <- input$dens_feat_proc
  myfac <- input$dens_fact
  
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
  
  ggstatsplot::ggbetweenstats(
    data = data_subset,
    x = my_factor,
    y = my_numeric,
    title = "",
    messages = FALSE,
    type = "parametric",
    pairwise.display = "all",
    pairwise.comparisons = TRUE,
    p.adjust.method = "holm",
    results.subtitle = TRUE,
    xlab = myfac,
    ylab = mynum)
  
})

