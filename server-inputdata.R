
observe_helpers(help_dir = "help_mds")

observe({
  if(!is.null(datasetInput())){

    data <- datasetInput()
    my_data_names <- data %>%
      # slice(-1) %>%
      janitor::clean_names() %>% # clean column names
      mutate_all(as.numeric) %>%
      mutate_at(vars(contains("gender")), as.factor) %>% # modify var type
      mutate_at(vars(contains("department")), as.factor) %>% # modify var type
      mutate_at(vars(contains("area")), as.factor) %>% # modify var type
      mutate_at(vars(contains("outcome")), as.factor) %>% # modify var type
      mutate_at(vars(contains("age")), as.character) %>% # modify var type
      mutate_at(vars(contains("record")), as.character) %>% # modify var type
      mutate_at(vars(contains("sample_nu")), as.character) %>% # modify var type
      mutate_at(vars(contains("follow_up_days")), as.character) %>% # modify var type
      mutate_at(vars(contains("follow_up_samples")), as.character) %>% # modify var type
      mutate_at(vars(contains("tzc")), as.character) %>% # modify var type
      select_if(is.numeric) # select only numeric vars

    x <- colnames(my_data_names)

    updateSelectInput(session, "transformation_log", choices = c("None", x), selected = x)
    updateSelectInput(session, "transformation_log2", choices = c("None", x), selected = "None")
    updateSelectInput(session, "transformation_log10", choices = c("None", x), selected = "None")
    updateSelectInput(session, "transformation_sqrt", choices = c("None", x), selected = "None")

  }
})

####

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
                                      
                                      data <- datasetInput()
                                      my_dates <- data$Date_sample
                                      
                                      data_subset <- data %>%
                                        # slice(-1) %>% # remove first now
                                        janitor::clean_names() %>% # clean column names
                                        mutate(date_sample = excel_numeric_to_date(as.numeric(as.character(my_dates)), date_system = "modern")) %>%
                                        select_if(~ sum(!is.na(.)) > 0) %>% # drop columns that only have NAs
                                        select_if(~ !(sum(is.na(.))/nrow(data))*100 > input$removeNA) %>% # remove na
                                        mutate_if(is.numeric, function(x)ifelse(x == 0, input$replaceZeros, x)) %>% # replace zeros
                                        
                                        mutate_at(vars(contains("gender")), as.factor) %>% # modify var type
                                        mutate_at(vars(contains("department")), as.factor) %>% # modify var type
                                        mutate_at(vars(contains("area")), as.factor) %>% # modify var type
                                        mutate_at(vars(contains("outcome")), as.factor) %>% # modify var type
                                        
                                        mutate_if(is.character, ~ as.numeric(as.character(.))) %>% # all char to num
                                        
                                        # mutate_at(vars(contains("date_sample")), dmy) %>% # modify var type
                                        mutate_at(vars(contains("age")), as.character) %>% # modify var type
                                        mutate_at(vars(contains("record")), as.character) %>% # modify var type
                                        mutate_at(vars(contains("sample_nu")), as.character) %>% # modify var type
                                        mutate_at(vars(contains("follow_up_days")), as.character) %>% # modify var type
                                        mutate_at(vars(contains("follow_up_samples")), as.character) %>% # modify var type
                                        mutate_at(vars(contains("tzc")), as.character) %>% # modify var type

                                        rename_at(vars(contains("age")), ~ "age") %>% # modify var name

                                        mutate_at(vars(matches(input$transformation_log)), log) %>% # log transformation
                                        mutate_at(vars(matches(input$transformation_log2)), log2) %>% # log2 transformation
                                        mutate_at(vars(matches(input$transformation_log10)), log10) %>% # log10 transformation
                                        mutate_at(vars(matches(input$transformation_sqrt)), sqrt) %>% # sqrt transformation
                                        rename_at(vars(matches(input$transformation_log)), ~ paste0(., "_log_trans")) %>% # modify log transformed var names
                                        rename_at(vars(matches(input$transformation_log2)), ~ paste0(., "_log2_trans")) %>% # modify log2 transformed var names
                                        rename_at(vars(matches(input$transformation_log10)), ~ paste0(., "_log10_trans")) %>% # modify log10 transformed var names
                                        rename_at(vars(matches(input$transformation_sqrt)), ~ paste0(., "_sqrt_trans")) %>% # modify sqrt transformed var names

                                        rename_if(is.numeric, ~ paste0(., "_proc")) %>% # modify all numeric var names
                                        rename_if(is.Date, ~ "date") %>% # modify var name
                                      
                                        mutate(age = as.numeric(age)) %>% # return age to numerical
                                        mutate(complete_vars = apply(data, 1, function(x)sum(!is.na(x)))) %>% # create complete variables count
                                        dplyr::group_by(record_num_hvh) %>% # create time points var
                                        add_count(name = "time_points") %>% # create time points var
                                        dplyr::ungroup() %>% # create time points var
                                        select(complete_vars, time_points, everything()) # reorder columns
                          
                                      
                                      if(input$summarize_points != 'all'){
                                        
                                        #### choose fist and last point of each subject
                                        
                                        if(input$summarize_points == 'first_last'){
                                          data_subset <- data_subset %>% 
                                            arrange(date) %>%
                                            group_by(record_num_hvh) %>% 
                                            slice(1L, n())
                                          data_subset <-  data_subset[!duplicated(data_subset) ,]
                                        }
                                        
                                        #### choose fist point of each subject
                                        
                                        if(input$summarize_points == 'first'){
                                          data_subset <- data_subset %>% 
                                            arrange(date) %>%
                                            group_by(record_num_hvh) %>% 
                                            slice(1L)
                                          data_subset <-  data_subset[!duplicated(data_subset) ,]
                                        }
                                        
                                        #### choose last point of each subject
                                        
                                        if(input$summarize_points == 'last'){
                                          data_subset <- data_subset %>% 
                                            arrange(date) %>%
                                            group_by(record_num_hvh) %>% 
                                            slice(n())
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

