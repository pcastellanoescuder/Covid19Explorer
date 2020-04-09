
datasetInput <- reactive({
  
  infile <- input$dataset
  
  if(is.null(infile)){
    return(NULL)
    }
  else {
    data <- readxl::read_excel(infile$datapath)
    data <- data %>% 
      mutate(num_var = apply(data, 1, function(x)sum(!is.na(x)))) %>%
      dplyr::select(num_var, everything())
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
    dplyr::select(-num_var) %>%
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

output$contents <- DT::renderDataTable({
  
  my_raw <- datasetInput()
  
  DT::datatable(my_raw, 
                filter = 'top',extensions = 'Buttons',
                escape = T,  rownames = FALSE, 
                class = 'cell-border stripe',
                options = list(
                  scrollX = TRUE,
                  stateSave = FALSE,
                  # default column search strings and global search string
                  search = list(regex = TRUE, caseInsensitive = FALSE)
                ))
  
})

# output$contents <- renderD3tf({
#   
#   # Define table properties. See http://tablefilter.free.fr/doc.php
#   # for a complete reference
#   tableProps <- list(
#     btn_reset = TRUE,
#     # alphabetic sorting for the row names column, numeric for all other columns
#     col_types = c("string", rep("number", ncol(datasetInput())))
#   );
#   
#   d3tf(datasetInput(),
#        tableProps = tableProps,
#        extensions = list(
#          list(name = "sort")
#        ),
#        showRowNames = FALSE,
#        selectableRows = "multi",
#        selectableRowsClass = "success",
#        tableStyle = "table table-bordered");
#   
# })

####

output$contents_proc <- DT::renderDataTable({
  
  my_processed <- processedInput()
  
  DT::datatable(my_processed, 
                filter = 'none',extensions = 'Buttons',
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

