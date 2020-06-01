
observe({
  if(!is.null(processedInput())){

    if(isTRUE(input$remove_first)){
      data_subset <- datasetInput() %>%
        slice(-1) # remove first now
    } else {
      data_subset <- datasetInput()
    }

    data_subset <- data_subset %>%
      janitor::clean_names() %>% # clean column names
      select_if(~ sum(!is.na(.)) > 0) %>% # drop columns that only have NAs
      select(starts_with("tn_"), starts_with("n_"))

    x <- colnames(data_subset)
    
    if(length(x) > 36){
      updateSelectInput(session, "dens_feat_raw", choices = x, selected = x[1])
    } else{
      updateSelectInput(session, "dens_feat_raw", choices = c("All features", x), selected = "All features")
    }
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
    
    my_table_num <- data_subset %>%
      janitor::clean_names() %>% # clean column names
      select_if(~ sum(!is.na(.)) > 0) %>% # drop columns that only have NAs
      mutate_at(vars(tidyr::starts_with("tn_")), ~ as.numeric(as.character(.))) %>%
      mutate_at(vars(tidyr::starts_with("n_")), ~ as.numeric(as.character(.))) %>%
      mutate_at(vars(tidyr::starts_with("f_")), ~ as.factor(as.character(.))) %>%
      mutate_at(vars(tidyr::starts_with("c_")), as.character) %>%
      mutate_at(vars(tidyr::starts_with("id_")), as.character) %>%
      rename_at(vars(contains("date")), ~ "date") %>% # modify var name
      rename_at(vars(tidyr::starts_with("id_")), ~ "id")
    
    my_table <- my_table_num %>%
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

