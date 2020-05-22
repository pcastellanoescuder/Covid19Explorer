
output$correlation_table <- DT::renderDataTable({
  
  data_subset <- processedInput() %>%
    dplyr::select(-time_points)
  
  if(!is.null(input$contents_proc_rows_selected)){
    data_subset <- data_subset[input$contents_proc_rows_selected ,]
  } 
  
  data_numeric <- data_subset %>%
    select_if(is.numeric)
  
  correlations <- cor(data_numeric, use = "pairwise.complete.obs")
  
  correlations[lower.tri(correlations, diag = TRUE)] <- NA
  correlations <- as.data.frame(as.table(correlations))
  correlations <- na.omit(correlations)
  correlations <- correlations[with(correlations, order(-Freq)), ]
  colnames(correlations)[3] <- "corr"
  
  DT::datatable(correlations, 
                filter = 'top', extensions = 'Buttons',
                escape = FALSE,  rownames = FALSE, 
                class = 'cell-border stripe',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons =
                    list("copy", "print", list(
                      extend="collection",
                      buttons=list(list(extend = "csv",
                                        filename = paste0(Sys.Date(), "_correlations")),
                                   list(extend = "excel",
                                        filename = paste0(Sys.Date(), "_correlations")),
                                   list(extend = "pdf",
                                        filename = paste0(Sys.Date(), "_correlations"))),
                      text = "Dowload")),
                  order = list(list(2, "desc")),
                  pageLength = nrow(correlations)
                ))
  
})

