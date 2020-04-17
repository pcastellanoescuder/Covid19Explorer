
observe({
  if(!is.null(processedInput())){
    
    data_subset <- processedInput() %>%
      select_at(vars(ends_with("_proc")))
    
    x <- colnames(data_subset)

    updateSelectInput(session, "features", choices = x, selected = x[grepl("il6", x)])
  }
})

##

output$timeplots <- renderPlotly({
  
  if(is.null(processedInput())){
    return(NULL)
  } 
  else{
    
  if(is.null(input$contents_proc_rows_selected)){
    return(NULL)
  }
  
  else{
  
    data_subset <- processedInput() %>%
      dplyr::select(-complete_vars, -time_points)
    
    if(!is.null(input$contents_proc_rows_selected)){
      data_subset <- data_subset[input$contents_proc_rows_selected ,]
    } 
  
  data_subset <- data_subset %>%
    pivot_longer(cols = ends_with("_proc")) %>% 
    dplyr::rename(variable = name) %>%
    dplyr::filter(variable %in% input$features)

  my_time_plot <- ggplot(data_subset) +
    {if(isTRUE(input$plot_lines))geom_line(aes(date, value, color = variable, shape = variable, label = record_num_hvh), 
                                               size = 1, alpha = 0.6)} +
    geom_point(aes(date, value, color = variable, shape = variable, label = record_num_hvh), size = 1.2) +
    ylab("variables") +
    xlab("") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    {if(input$wrap_into == "gender")facet_wrap(vars(gender))} +
    {if(input$wrap_into == "department")facet_wrap(vars(department))} +
    {if(input$wrap_into == "subject")facet_wrap(vars(record_num_hvh))} +
    labs(color = "", shape = "") 
    # scale_x_continuous(labels = function(x)format(as.Date(as.character(x), "%j"), "%d-%b"))
  
  plotly::ggplotly(my_time_plot)
  
  }}
  
  })

