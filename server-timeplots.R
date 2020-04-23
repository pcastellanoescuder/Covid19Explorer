
observe({
  if(!is.null(processedInput())){
    
    data_subset <- processedInput() %>%
      select(starts_with("tn_"), starts_with("n_"))
    
    x <- colnames(data_subset)

    updateSelectInput(session, "features", choices = x, selected = x[1:2])
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
    ylab("Variables") +
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
  
  }}
  
  })

