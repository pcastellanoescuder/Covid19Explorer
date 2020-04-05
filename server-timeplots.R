
observe({
  if(!is.null(datasetInput())){
  x <- colnames(datasetInput())
  updateSelectInput(session, "features", choices = x[8:ncol(datasetInput())], selected = x[grepl("il6", x)])
  }
})

output$timeplots <- renderPlotly({
  
  if(is.null(datasetInput())){
    return(NULL)
  } 
  else{
  
  data_subset <- datasetInput() 
  
  if(!is.null(input$contents_rows_selected)){
    data_subset <- data_subset[input$contents_rows_selected ,]
  } 
  else{
    data_subset <- data_subset[1:30 ,]
  }
  
  data_subset <- data_subset %>%
    mutate(data_calendar = dmy(data_calendar)) %>%
    mutate_at(c("edad_number", "numero", "codi_extern"), as.character) %>%
    mutate_if(is.numeric, log) %>%
    reshape2::melt(id.vars = c("codi_extern", "numero", "data_calendar", "edad_number", "gender", "servei")) %>%
    filter(variable %in% input$features) %>%
    arrange(desc(data_calendar))
  
  # if(isTRUE(input$remove_rep)){
  #   data_subset <- data_subset %>%
  #     group_by(codi_extern, data_calendar, variable) %>% 
  #     slice(n(), 1) %>% # last measuse of each subject
  #     ungroup()
  # }

  my_time_plot <- ggplot(data_subset) +
    geom_line(aes(data_calendar, value, color = variable, shape = variable), size = 1, alpha = 0.6) +
    geom_point(aes(data_calendar, value, color = variable, shape = variable), size = 1.2) +
    ylab("log(variables)") +
    xlab("") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    {if(input$wrap_into == "gender")facet_wrap(vars(gender))} +
    {if(input$wrap_into == "service")facet_wrap(vars(servei))} +
    {if(input$wrap_into == "subject")facet_wrap(vars(codi_extern))} +
    labs(color = "", shape = "")
  
  plotly::ggplotly(my_time_plot)
  
  }
  
  })

