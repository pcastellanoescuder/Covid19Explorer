
observe({
  if(!is.null(processedInput())){

    data_subset <- processedInput() %>%
      select_at(vars(ends_with("_proc")))

    # data_subset2 <- processedInput() %>%
    #   select_if(is.factor)

    x <- colnames(data_subset)
    # y <- colnames(data_subset2)

    updateSelectInput(session, "dens_feat", choices = x, selected = x[1])
    # updateSelectInput(session, "my_factor2", choices = c("None", y), selected = "None")
  }
})

# observe({
#   if(!is.null(datasetInput())){
#     
#     data <- datasetInput()
# 
#     data_raw <- data %>%
#       select_if(~ sum(!is.na(.)) > 0) %>% # drop columns that only have NAs
#       janitor::clean_names() %>% # clean column names
#       select(-date_sample, -record_num_hvh, -sample_num, -age_years, -gender, -department, 
#              -area_of_care, -outcome, -follow_up_days, -follow_up_samples_total_n, -tzc_onset, -tzc_final)
#     
#     x <- colnames(my_data_names)
#     
#     updateSelectInput(session, "transformation_log", choices = c("None", x), selected = x)
#     updateSelectInput(session, "transformation_log2", choices = c("None", x), selected = "None")
#     updateSelectInput(session, "transformation_log10", choices = c("None", x), selected = "None")
#     updateSelectInput(session, "transformation_sqrt", choices = c("None", x), selected = "None")
#     
#   }
# })

##

output$densityplots <- renderPlot({

  if(is.null(processedInput())){
    return(NULL)
  }
  else{

    data_subset <- processedInput()

    if(!is.null(input$contents_proc_rows_selected)){
      data_subset <- data_subset[input$contents_proc_rows_selected ,]
    }
    
    myvar <- input$dens_feat
    
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

