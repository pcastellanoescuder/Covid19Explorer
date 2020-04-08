
observe({
  if(!is.null(processedInput())){
    x <- colnames(processedInput())
    idx <- map(processedInput(), is.numeric) %>% unlist()
    updateSelectInput(session, "features", choices = x[idx], selected = x[grepl("il6", x)])
  }
})

output$timeplots <- renderPlotly({
  
  if(is.null(processedInput())){
    return(NULL)
  } 
  else{
    
  if(is.null(input$contents_rows_selected)){
    return(NULL)
  }
  
  else{
  
  data_subset <- processedInput()
  
  data_subset <- data_subset %>%
    pivot_longer(cols = ends_with("_log")) %>% 
    dplyr::rename(variable = name) %>%
    dplyr::filter(variable %in% input$features)

  my_time_plot <- ggplot(data_subset) +
    {if(isTRUE(input$plot_lines))geom_line(aes(data_calendar, value, color = variable, shape = variable, label = codi_extern), 
                                               size = 1, alpha = 0.6)} +
    geom_point(aes(data_calendar, value, color = variable, shape = variable, label = codi_extern), size = 1.2) +
    ylab("log(variables)") +
    xlab("") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    {if(input$wrap_into == "gender")facet_wrap(vars(gender))} +
    {if(input$wrap_into == "service")facet_wrap(vars(servei))} +
    {if(input$wrap_into == "subject")facet_wrap(vars(codi_extern))} +
    labs(color = "", shape = "")
  
  plotly::ggplotly(my_time_plot)
  
  }}
  
  })

