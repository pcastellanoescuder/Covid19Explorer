
observe({
  if(!is.null(processedInput())){
    
    # numeric
    data_subset <- processedInput() %>%
      select(starts_with("tn_"), starts_with("n_"))
    
    x <- colnames(data_subset)

    updateSelectInput(session, "features", choices = x, selected = x[1:2])
    
    # factor
    data_fac <- processedInput() %>%
      select(starts_with("f_"))
    
    y <- colnames(data_fac)
    
    updateSelectInput(session, "time_fact", choices = c("None", y), selected = "None")
    
  }
})

##

output$timeplots <- renderPlot({
  
  if(is.null(processedInput())){
    return(NULL)
  } 
  else{
  
  validate(need(!is.null(input$contents_proc_rows_selected), "No rows selected in the 'Input Data' panel"))
    
  if(is.null(input$contents_proc_rows_selected)){
    return(NULL)
  }
  
  else{
  
    data_subset <- processedInput() %>%
      dplyr::select(-time_points)
    
    if(!is.null(input$contents_proc_rows_selected)){
      data_subset <- data_subset[input$contents_proc_rows_selected ,]
    } 
  
  data_subset <- data_subset %>%
    pivot_longer(cols = starts_with("tn_") | starts_with("n_")) %>% 
    dplyr::rename(variable = name) %>%
    dplyr::filter(variable %in% input$features)

  ggplot(data_subset) +
    {if(isTRUE(input$plot_lines))geom_line(aes(date, value, color = variable, shape = variable, label = id), 
                                               size = 2, alpha = 0.75)} +
    geom_point(aes(date, value, color = variable, shape = variable, label = id), size = 3, alpha = 0.75) +
    ylab("Value") +
    xlab("") +
    theme_bw() +
    theme(legend.position = "top",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15)) +
    # {if(input$wrap_into == "gender")facet_wrap(vars(gender))} +
    # {if(input$wrap_into == "department")facet_wrap(vars(department))} +
    {if(input$wrap_into)facet_wrap(vars(id))} +
    labs(color = "", shape = "") 
    # scale_x_continuous(labels = function(x)format(as.Date(as.character(x), "%j"), "%d-%b"))
  
  # plotly::ggplotly(my_time_plot)
  
  }
    }
  
  })

##

output$twotimes <- renderPlot({
  
  if(is.null(processedInput())){
    return(NULL)
  } 
  else{
    
    validate(need(!is.null(input$contents_proc_rows_selected), "No rows selected in the 'Input Data' panel"))
    validate(need(input$time_fact != "None", "Select a factor"))
    
    if(is.null(input$contents_proc_rows_selected)){
      return(NULL)
    }
    
    else{
      
      data_subset <- processedInput() %>%
        dplyr::select(-time_points)
      
      if(!is.null(input$contents_proc_rows_selected)){
        data_subset <- data_subset[input$contents_proc_rows_selected ,]
      }
      
      features <- input$features[1]
      
      data_subset <- data_subset %>%
        pivot_longer(cols = starts_with("tn_") | starts_with("n_")) %>% 
        dplyr::rename(variable = name) %>%
        select_at(vars(dplyr::matches(input$time_fact) | dplyr::matches("id") | dplyr::matches("date") | dplyr::matches("variable") | dplyr::matches("value"))) %>%
        dplyr::rename("Factor" = 1) %>%
        dplyr::filter(variable %in% features) %>%
        dplyr::arrange(id, date, variable) %>%
        mutate(time_to_comp = as.factor(ifelse(duplicated(id), 2, 1)))
      
      ggplot(data_subset, aes(x = time_to_comp, y = value)) + 
        geom_line(aes(group = id, color = Factor)) +
        geom_point(size = 3, alpha = 0.8) +
        theme_bw() +
        xlab("") +
        ylab(paste0(features, " value")) +
        theme(legend.position = "top",
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 15),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15)) 
        
    }
  }
  
})

