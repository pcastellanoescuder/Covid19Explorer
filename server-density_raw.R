
observe({
  if(!is.null(processedInput())){

    if(isTRUE(input$remove_first)){
      data_subset <- datasetInput() %>%
        slice(-1) # remove first now
    } else {
      data_subset <- datasetInput()
    }

    no_num <- c("date_sample", "record_num_hvh", "sample_num", "gender", "department",
                "area_of_care", "outcome", "follow_up_days",
                "follow_up_samples_total_n", "tzc_onset", "tzc_final")

    data_subset <- data_subset %>%
      janitor::clean_names() %>% # clean column names
      select_if(~ sum(!is.na(.)) > 0) %>% # drop columns that only have NAs
      select_at(vars(-matches(no_num)))

    x <- colnames(data_subset)

    updateSelectInput(session, "dens_feat_raw", choices = c("All features", x), selected = "All features")
  }
})

##

DESFUN <- reactive({
  
  if(!is.null(processedInput())){
    
    if(isTRUE(input$remove_first)){
      data_subset <- datasetInput() %>%
        slice(-1)
    } 
    else {
      data_subset <- datasetInput()
    }
    
    # if(!is.null(input$contents_rows_selected)){
    #   data_subset <- data_subset[input$contents_rows_selected ,]
    #   }
    
    no_num <- c("date_sample", "record_num_hvh", "sample_num", 
                "age_years", "gender", "department", 
                "area_of_care", "outcome", "follow_up_days", 
                "follow_up_samples_total_n", "tzc_onset", "tzc_final")
    
    my_table_num <- data_subset %>%
      janitor::clean_names() %>% # clean column names
      select_if(~ sum(!is.na(.)) > 0) %>% # drop columns that only have NAs
      mutate_at(vars(-matches(no_num)), ~ as.numeric(as.character(.))) %>% # char to num
      mutate_at(vars(contains("date_sam")), as.character) %>%
      mutate_at(vars(contains("age")), as.numeric) %>%
      mutate_at(vars(contains("record_nu")), as.character) %>% # modify var type
      mutate_at(vars(contains("sample_nu")), as.character) %>% # modify var type
      mutate_at(vars(contains("follow_up_days")), as.character) %>% # modify var type
      mutate_at(vars(contains("follow_up_samples")), as.character) %>% # modify var type
      mutate_at(vars(contains("tzc")), as.character) %>% # modify var type
      mutate_at(vars(contains("gender")), as.factor) %>% # modify var type
      mutate_at(vars(contains("department")), as.factor) %>% # modify var type
      mutate_at(vars(contains("area")), as.factor) %>% # modify var type
      mutate_at(vars(contains("outcome")), as.factor)
    
    my_table <- my_table_num %>%
      skim() %>%
      dplyr::rename(type = skim_type, variable = skim_variable, percent_complete = complete_rate) %>%
      mutate(percent_complete = percent_complete*100) %>%
      filter(variable != "date_sample") %>%
      filter(variable != "record_num_hvh") %>%
      filter(variable != "sample_num") %>%
      mutate_if(is.numeric, round, 3) %>%
      dplyr::select(-c(character.min:factor.n_unique)) %>%
      dplyr::rename(Freq = factor.top_counts, mean = numeric.mean, sd = numeric.sd, min = numeric.p0, 
                    p25 = numeric.p25, median = numeric.p50, p75 = numeric.p75,
                    max = numeric.p100, histogram = numeric.hist)
    
    my_table_num <- my_table_num %>%
      select_if(is.numeric)
    
    return(list(my_table = my_table, my_table_num = my_table_num))
    
  }
  
})

##

output$descriptive_raw <- renderDataTable({
  
  my_table <- DESFUN()$my_table
  datatable(my_table, rownames = FALSE, options = list(scrollX = TRUE))
  
})

##

output$densityplots_raw <- renderPlot({

  data_subset <- DESFUN()$my_table_num

    myvar <- input$dens_feat_raw

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

