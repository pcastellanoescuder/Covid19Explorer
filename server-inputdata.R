
datasetInput <- reactive({
  
  infile <- input$dataset
  
  if(is.null(infile)){
    return(NULL)
    }
  else {
    data <- readxl::read_excel(infile$datapath)
    return(data)
    }
})

####

processedInput <- reactive({
  
  if(is.null(datasetInput())){
    return(NULL)
  }
  else {
  
  data <- datasetInput()
  
  if(!is.null(input$contents_rows_selected)){
    data_subset <- data[input$contents_rows_selected ,]
  } 
  else{
    data_subset <- data
  }
  
  data_subset <- data_subset %>%
    janitor::clean_names() %>%
    mutate_at(vars(contains("data")), dmy) %>% 
    mutate_at(vars(contains("edad")), as.character) %>%        
    mutate_at(vars(contains("numer")), as.character) %>%  
    mutate_at(vars(contains("codi")), as.character) %>% 
    mutate_at(vars(contains("seguim")), as.character) %>% 
    mutate_at(vars(contains("gend")), as.factor) %>% 
    mutate_at(vars(contains("serve")), as.factor) %>% 
    rename_at(vars(contains("codi")), ~ "codi_extern") %>%
    rename_at(vars(contains("gend")), ~ "gender") %>%
    rename_at(vars(contains("serve")), ~ "servei") %>%
    mutate(tcz = as.factor(ifelse(is.na(tcz), 0, tcz))) %>% 
    mutate_if(is.numeric, log) %>%
    rename_if(is.numeric, ~ paste0(., "_log")) %>%
    rename_if(is.Date, ~ "data_calendar") %>%
    arrange(desc(data_calendar))
  
  #   data_subset <- data_subset %>%
  #     group_by(codi_extern, data_calendar) %>% 
  #     slice(n(), 1) %>% # last measuse of each subject
  #     ungroup()

  data_subset[data_subset == -Inf] <- log(0.01) 
  
  return(data_subset)
  
  }

})

####

output$contents <- DT::renderDataTable(datasetInput(), class = 'cell-border stripe', rownames = FALSE, server = TRUE)

####

output$contents_proc <- DT::renderDataTable({
  
  my_processed <- processedInput()
  
  DT::datatable(my_processed, 
                filter = 'none',extensions = 'Buttons',
                escape = FALSE,  rownames = FALSE, 
                class = 'cell-border stripe',
                options = list(
                  dom = 'Bfrtip',
                  buttons = 
                    list("copy", "print", list(
                      extend="collection",
                      buttons=list(list(extend = "csv",
                                        filename = paste0(Sys.Date(), "_processed_data")),
                                   list(extend = "excel",
                                        filename = paste0(Sys.Date(), "_processed_data")),
                                   list(extend = "pdf",
                                        filename = paste0(Sys.Date(), "_processed_data"))),
                                   text = "Dowload")),
                      order = list(list(2, "desc")),
                      pageLength = nrow(my_processed)))
                
})

