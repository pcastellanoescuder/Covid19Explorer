
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
  
  data_subset <- data %>%
    janitor::clean_names() %>% # clean column names
    mutate_at(vars(contains("data")), dmy) %>% # modify var type
    mutate_at(vars(contains("edad")), as.character) %>% # modify var type
    mutate_at(vars(contains("numer")), as.character) %>% # modify var type
    mutate_at(vars(contains("codi")), as.character) %>% # modify var type
    mutate_at(vars(contains("seguim")), as.character) %>% # modify var type
    mutate_at(vars(contains("gend")), as.factor) %>% # modify var type
    mutate_at(vars(contains("serve")), as.factor) %>% # modify var type
    rename_at(vars(contains("codi")), ~ "subject_code") %>% # modify var name
    rename_at(vars(contains("gend")), ~ "gender") %>% # modify var name
    rename_at(vars(contains("serve")), ~ "service") %>% # modify var name
    rename_at(vars(contains("edad")), ~ "age") %>% # modify var name
    mutate(tcz = as.factor(ifelse(is.na(tcz), 0, tcz))) %>% # replace NA by 0 and modify type var
    select_if(~ !(sum(is.na(.))/nrow(data))*100 > input$removeNA) %>% # remove na
    mutate_if(is.numeric, function(x)ifelse(x == 0, input$replaceZeros, x)) %>% # replace zeros
    mutate_if(is.numeric, input$transformation_type) %>% # transformation
    rename_if(is.numeric, ~ paste0(., "_", input$transformation_type, "_trans")) %>% # modify numeric var names
    rename_if(is.Date, ~ "date") %>% # modify var name
    mutate(age = as.numeric(age)) %>% # return age to numerical
    mutate(complete_vars = apply(data, 1, function(x)sum(!is.na(x)))) %>% # create complete variables count
    dplyr::group_by(subject_code) %>% # create time points var
    add_count(name = "time_points") %>% # create time points var
    dplyr::ungroup() %>% # create time points var
    select(complete_vars, time_points, everything()) # reorder columns
    
  #### choose fist and last point of each subject
  
  if(isTRUE(input$first_last)){
    data_subset <- data_subset %>% 
      arrange(date) %>%
      group_by(subject_code) %>% 
      slice(1L, n())
    data_subset <-  data_subset[!duplicated(data_subset) ,]
  }
  
  return(data_subset)
  
  }

})

####

output$contents <- DT::renderDataTable({
  
  my_raw <- datasetInput()
  
  DT::datatable(my_raw, 
                filter = 'none',extensions = 'Buttons',
                escape = T,  rownames = FALSE, 
                class = 'cell-border stripe',
                options = list(
                  scrollX = TRUE))
  
})

####

output$contents_proc <- DT::renderDataTable({
  
  my_processed <- processedInput()
  
  DT::datatable(my_processed, 
                filter = 'top',extensions = 'Buttons',
                escape = FALSE,  rownames = FALSE, 
                class = 'cell-border stripe',
                options = list(
                  scrollX = TRUE,
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

