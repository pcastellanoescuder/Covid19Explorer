
observe_helpers(help_dir = "help_mds")

##

datasetInput <- reactive({
  
  infile <- input$dataset
  
  if(is.null(infile)){
    return(NULL)
    }
  else {
    if(input$readCSV == "csv"){
      if(input$separator == ";"){
        data <- readr::read_csv2(infile$datapath, local = locale(encoding = "ISO-8859-1", decimal_mark = ","))
      } else {
        data <- readr::read_csv(infile$datapath, local = locale(encoding = "ISO-8859-1", decimal_mark = ","))
      }
    } else {
      data <- readxl::read_xlsx(infile$datapath)
    }
    return(data)
    }
})

####

observe({
  
  if(!is.null(datasetInput())){

  if(isTRUE(input$remove_first)){
    data <- datasetInput() %>%
      slice(-1) # remove first now
  } else {
    data <- datasetInput()
  }
  
  my_data_names <- data %>%
    select_if(~ sum(!is.na(.)) > 0) %>% # drop columns that only have NAs
    janitor::clean_names() %>% # clean column names
    select_at(vars(starts_with("tn_")))
  
  x <- colnames(my_data_names)
  
  updateSelectInput(session, "transformation_log", choices = c("None", x), selected = x)
  updateSelectInput(session, "transformation_log2", choices = c("None", x), selected = "None")
  updateSelectInput(session, "transformation_log10", choices = c("None", x), selected = "None")
  updateSelectInput(session, "transformation_sqrt", choices = c("None", x), selected = "None")
  
  }
  
})

##

processedInput <- eventReactive(input$process,
                                ignoreNULL = TRUE, {
                                  withProgress(message = "Processing data, please wait",{
                                    
                                    if(is.null(datasetInput())){
                                      return(NULL)
                                    }
                                    else {
                                      
                                      validate(
                                        need(!is.null(input$transformation_log) & 
                                             !is.null(input$transformation_log2) &
                                             !is.null(input$transformation_log10) &
                                             !is.null(input$transformation_sqrt), 
                                             HTML("Please, fill ALL the transformation options in the 'Advanced settings' panel. If you don't want to use some of them please, select 'None'."))
                                      )
                                      
                                      if(isTRUE(input$remove_first)){
                                        data <- datasetInput() %>% 
                                          slice(-1) # remove first now
                                      } else {
                                        data <- datasetInput()
                                      }
                                      
                                      data_subset <- data %>%
                                        janitor::clean_names() %>% # clean column names
                                        select_if(~ sum(!is.na(.)) > 0) %>% # drop columns that only have NAs
                                        mutate_at(vars(tidyr::starts_with("tn_")), ~ as.numeric(as.character(.))) %>%
                                        mutate_at(vars(tidyr::starts_with("n_")), ~ as.numeric(as.character(.))) %>%
                                        mutate_at(vars(tidyr::starts_with("f_")), ~ as.factor(as.character(.))) %>%
                                        mutate_at(vars(tidyr::starts_with("c_")), as.character) %>%
                                        mutate_at(vars(tidyr::starts_with("id_")), as.character) %>%
                                        rename_at(vars(contains("date")), ~ "date") %>% # modify var name
                                        rename_at(vars(tidyr::starts_with("id_")), ~ "id") %>% # modify var name
                                        select_if(~ !(sum(is.na(.))/nrow(data))*100 > input$removeNA) %>% # remove na
                                        mutate_at(vars(tidyr::starts_with("tn_")), function(x)ifelse(x == 0, input$replaceZeros, x)) # replace zeros
                                      
                                        if(input$trans_data){
                                          data_subset <- data_subset %>%
                                            mutate(date = janitor::excel_numeric_to_date(as.numeric(as.character(date)))) # format date
                                        } 
                                      
                                        else {
                                          if(input$date_format == "ymd"){
                                            data_subset <- data_subset %>%
                                              mutate_at(vars(matches("date")), lubridate::ymd) # format date
                                          }
                                          else if(input$date_format == "dmy"){
                                            data_subset <- data_subset %>%
                                              mutate_at(vars(matches("date")), lubridate::dmy) # format date
                                          }
                                          else if(input$date_format == "mdy"){
                                            data_subset <- data_subset %>%
                                              mutate_at(vars(matches("date")), lubridate::mdy) # format date
                                          }
                                        }
                                        
                                        data_subset <- data_subset %>%
                                          mutate_at(vars(matches(input$transformation_log)), log) %>% # log transformation
                                          mutate_at(vars(matches(input$transformation_log2)), log2) %>% # log2 transformation
                                          mutate_at(vars(matches(input$transformation_log10)), log10) %>% # log10 transformation
                                          mutate_at(vars(matches(input$transformation_sqrt)), sqrt) %>% # sqrt transformation
                                          rename_at(vars(matches(input$transformation_log)), ~ paste0(., "_log")) %>% # modify log transformed var names
                                          rename_at(vars(matches(input$transformation_log2)), ~ paste0(., "_log2")) %>% # modify log2 transformed var names
                                          rename_at(vars(matches(input$transformation_log10)), ~ paste0(., "_log10")) %>% # modify log10 transformed var names
                                          rename_at(vars(matches(input$transformation_sqrt)), ~ paste0(., "_sqrt")) %>% # modify sqrt transformed var names
                                          dplyr::group_by(id) %>% # create time points var
                                          add_count(name = "time_points") %>% # create time points var
                                          dplyr::ungroup() %>% # create time points var
                                          dplyr::select(id, date, time_points, everything()) # reorder columns
                                        
                                      
                                      if(input$summarize_points != 'all'){
                                        
                                        #### choose fist and last point of each subject
                                        
                                        if(input$summarize_points == 'first_last'){
                                          data_subset <- data_subset %>% 
                                            dplyr::arrange(date) %>%
                                            dplyr::group_by(id) %>% 
                                            dplyr::slice(1L, n()) %>%
                                            dplyr::ungroup() %>%
                                            dplyr::filter(time_points > 1)
                                          data_subset <-  data_subset[!duplicated(data_subset) ,]
                                        }
                                        
                                        #### choose fist point of each subject
                                        
                                        if(input$summarize_points == 'first'){
                                          data_subset <- data_subset %>% 
                                            dplyr::arrange(date) %>%
                                            dplyr::group_by(id) %>% 
                                            dplyr::slice(1L) %>%
                                            dplyr::ungroup()
                                          data_subset <-  data_subset[!duplicated(data_subset) ,]
                                        }
                                        
                                        #### choose last point of each subject
                                        
                                        if(input$summarize_points == 'last'){
                                          data_subset <- data_subset %>% 
                                            dplyr::arrange(date) %>%
                                            dplyr::group_by(id) %>% 
                                            dplyr::slice(n()) %>%
                                            dplyr::ungroup()
                                          data_subset <-  data_subset[!duplicated(data_subset) ,]
                                        }
                                      }
                                      
                                      return(data_subset)
                                      
                                    }
                                    })
                                  })

####

output$contents <- DT::renderDataTable({
  
  my_raw <- datasetInput()
  
  DT::datatable(my_raw, 
                filter = 'none',extensions = 'Buttons',
                escape = T,  rownames = FALSE, 
                class = 'cell-border stripe',
                options = list(
                  scrollX = TRUE,
                  lengthMenu = list(c(10, 25, 50, 100, -1), c('10','25','50', '100', 'All'))
                  ))
  
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
                  pageLength = nrow(my_processed)
                  # lengthMenu = list(c(10, 25, 50, 100, -1), c('10','25','50', '100', 'All'))
                  ))
                
})

##

observeEvent(input$process, ({
  updateCollapse(session, id = "input_collapse_panel", open = "proc_panel",
                 style = list("proc_panel" = "success",
                              "raw_panel" = "default"))
}))

##

observeEvent(datasetInput(),({
  updateCollapse(session, id =  "input_collapse_panel", open = "raw_panel",
                 style = list("raw_panel" = "default",
                              "proc_panel" = "success"))
}))

##

output$number_feat <- renderValueBox({
  
  if(!is.null(processedInput())){
    
    data_subset <- processedInput() %>%
      select_at(vars(starts_with("n_") | starts_with("tn_")))
    
  }
  
  infoBox(
    "Numeric Features", paste0(ncol(data_subset)), icon = icon("list"),
    color = "purple", fill = TRUE
  )
})

##

output$factor_feat <- renderValueBox({
  
  if(!is.null(processedInput())){
    
    data_subset <- processedInput() %>%
      select_at(vars(starts_with("f_")))
    
  }
  
  infoBox(
    "Factorial Features", paste0(ncol(data_subset)), icon = icon("list"),
    color = "yellow", fill = TRUE
  )
})

##

output$samples_num <- renderValueBox({
  
  if(!is.null(processedInput())){
    data_subset <- processedInput()
  }
  
  infoBox(
    "Samples", paste0(nrow(data_subset)), icon = icon("list"),
    color = "aqua", fill = TRUE
  )
})

