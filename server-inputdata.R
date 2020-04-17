
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
    janitor::clean_names() # clean column names
  
  x <- colnames(my_data_names)
  
  remove <- c("date_sample", "record_num_hvh", "sample_num", "age_years", "gender", "department", "area_of_care", "outcome", 
              "follow_up_days", "follow_up_samples_total_n", "tzc_onset", "tzc_final")
  x <- x[!(x %in% remove)]
  
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
                                      
                                      no_num <- c("date_sample", "record_num_hvh", "sample_num", 
                                                  "age_years", "gender", "department", 
                                                  "area_of_care", "outcome", "follow_up_days", 
                                                  "follow_up_samples_total_n", "tzc_onset", "tzc_final")
                                      
                                      data_subset <- data %>%
                                        janitor::clean_names() %>% # clean column names
                                        select_if(~ sum(!is.na(.)) > 0) %>% # drop columns that only have NAs
                                        mutate_at(vars(-matches(no_num)), ~ as.numeric(as.character(.))) %>% # char to num
                                        select_if(~ !(sum(is.na(.))/nrow(data))*100 > input$removeNA) %>% # remove na
                                        mutate_if(is.numeric, function(x)ifelse(x == 0, input$replaceZeros, x)) # replace zeros
                                        
                                        if(input$trans_data){
                                          data_subset <- data_subset %>%
                                            mutate(date_sample = janitor::excel_numeric_to_date(as.numeric(as.character(date_sample)))) # format date
                                        } 
                                      
                                        # else {
                                        #   data_subset <- data_subset %>%
                                        #     mutate(date_sample = lubridate::as_date(date_sample)) # format date
                                        # }
                                      
                                        else {
                                          if(input$date_format == "ymd"){
                                            data_subset <- data_subset %>%
                                              mutate_at(vars(contains("date_sam")), lubridate::ymd) # format date
                                          }
                                          else if(input$date_format == "dmy"){
                                            data_subset <- data_subset %>%
                                              mutate_at(vars(contains("date_sam")), lubridate::dmy) # format date
                                          }
                                          else if(input$date_format == "mdy"){
                                            data_subset <- data_subset %>%
                                              mutate_at(vars(contains("date_sam")), lubridate::mdy) # format date
                                          }
                                        }
                                        
                                        data_subset <- data_subset %>%
                                          mutate_at(vars(contains("record_nu")), as.character) %>% # modify var type
                                          mutate_at(vars(contains("sample_nu")), as.character) %>% # modify var type
                                          mutate_at(vars(contains("age")), as.character) %>% # modify var type
                                          mutate_at(vars(contains("follow_up_days")), as.character) %>% # modify var type
                                          mutate_at(vars(contains("follow_up_samples")), as.character) %>% # modify var type
                                          mutate_at(vars(contains("tzc")), as.character) %>% # modify var type
                                          mutate_at(vars(contains("gender")), as.factor) %>% # modify var type
                                          mutate_at(vars(contains("department")), as.factor) %>% # modify var type
                                          mutate_at(vars(contains("area")), as.factor) %>% # modify var type
                                          mutate_at(vars(contains("outcome")), as.factor) %>% # modify var type
                                          rename_at(vars(contains("age")), ~ "age") %>% # modify var name
                                          rename_if(lubridate::is.Date, ~ "date") %>% # modify var name
                                          
                                          mutate_at(vars(matches(input$transformation_log)), log) %>% # log transformation
                                          mutate_at(vars(matches(input$transformation_log2)), log2) %>% # log2 transformation
                                          mutate_at(vars(matches(input$transformation_log10)), log10) %>% # log10 transformation
                                          mutate_at(vars(matches(input$transformation_sqrt)), sqrt) %>% # sqrt transformation
                                          rename_at(vars(matches(input$transformation_log)), ~ paste0(., "_log_trans")) %>% # modify log transformed var names
                                          rename_at(vars(matches(input$transformation_log2)), ~ paste0(., "_log2_trans")) %>% # modify log2 transformed var names
                                          rename_at(vars(matches(input$transformation_log10)), ~ paste0(., "_log10_trans")) %>% # modify log10 transformed var names
                                          rename_at(vars(matches(input$transformation_sqrt)), ~ paste0(., "_sqrt_trans")) %>% # modify sqrt transformed var names
                                          
                                          rename_if(is.numeric, ~ paste0(., "_proc")) %>% # modify all numeric var names
                                          mutate_at(vars(contains("age")), as.numeric) %>% # return age to numerical
                                          mutate(complete_vars = apply(data_subset, 1, function(x)sum(!is.na(x)))) %>% # create complete variables count
                                          dplyr::group_by(record_num_hvh) %>% # create time points var
                                          add_count(name = "time_points") %>% # create time points var
                                          dplyr::ungroup() %>% # create time points var
                                          dplyr::select(complete_vars, time_points, everything()) # reorder columns
                                        
                                      
                                      if(input$summarize_points != 'all'){
                                        
                                        #### choose fist and last point of each subject
                                        
                                        if(input$summarize_points == 'first_last'){
                                          data_subset <- data_subset %>% 
                                            dplyr::arrange(date) %>%
                                            dplyr::group_by(record_num_hvh) %>% 
                                            dplyr::slice(1L, n()) %>%
                                            dplyr::ungroup() %>%
                                            dplyr::filter(time_points > 1)
                                          data_subset <-  data_subset[!duplicated(data_subset) ,]
                                        }
                                        
                                        #### choose fist point of each subject
                                        
                                        if(input$summarize_points == 'first'){
                                          data_subset <- data_subset %>% 
                                            dplyr::arrange(date) %>%
                                            dplyr::group_by(record_num_hvh) %>% 
                                            dplyr::slice(1L) %>%
                                            dplyr::ungroup()
                                          data_subset <-  data_subset[!duplicated(data_subset) ,]
                                        }
                                        
                                        #### choose last point of each subject
                                        
                                        if(input$summarize_points == 'last'){
                                          data_subset <- data_subset %>% 
                                            dplyr::arrange(date) %>%
                                            dplyr::group_by(record_num_hvh) %>% 
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

