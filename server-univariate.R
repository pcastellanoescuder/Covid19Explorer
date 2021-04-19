observe_helpers(help_dir = "help_mds")

observe({
  if(!is.null(processedInput())){
    
    # numeric
    data_num <- processedInput() %>%
      dplyr::select(-time_points) %>%
      select_if(is.numeric)
    
    x <- colnames(data_num)

    updateSelectInput(session, "feat_uni", choices = x, selected = x[1])
    
    # factor
    data_fac <- processedInput() %>%
      dplyr::select(-time_points) %>%
      select_if(is.factor)
    
    y <- colnames(data_fac)
    
    updateSelectInput(session, "fact_uni", choices = y, selected = y[1])
  }
})

##

PREPARE_UNI <- reactive({
  
  data_subset <- processedInput() %>%
    select_at(vars(starts_with("tn_") | starts_with("n_")))

  data_fact <- processedInput() %>%
    select_at(vars(matches(input$fact_uni))) %>%
    as.data.frame() %>%
    dplyr::rename(my_uni_factor = 1)
  
  data_subset <- cbind(data_fact, data_subset)
  
  if(!is.null(input$contents_proc_rows_selected)){
    data_subset <- data_subset[input$contents_proc_rows_selected ,]
  } 

  return(data_subset)
  
})

##

Univ_analisis <- 
  eventReactive(input$play_test,
                ignoreNULL = TRUE, {
                  withProgress(message = "Please wait",{
                    
                    data <- PREPARE_UNI()
                    
                    target <- data %>%
                      select(my_uni_factor) %>%
                      rownames_to_column("id") %>%
                      dplyr::rename(Group = my_uni_factor)
                    
                    e <- data %>%
                      select(-my_uni_factor)

                    Group <- as.factor(target$Group)
                    
                    group_means <- e %>% 
                      as.data.frame() %>% 
                      mutate(group = Group)
                    
                    ##
                    
                    mean_group <- group_means %>% 
                      group_by(group) %>% 
                      dplyr::summarise(across(
                        .cols = where(is.numeric), 
                        .fns = list(mean = mean), na.rm = TRUE, 
                        .names = "{col}"
                      )) %>%
                      ungroup() %>%
                      filter(!is.na(group)) %>%
                      column_to_rownames("group") %>%
                      t() %>%
                      as_tibble() %>%
                      rename_all(~ paste0("mean_", .))
                    
                    sd_group <- group_means %>% 
                      group_by(group) %>% 
                      dplyr::summarise(across(
                        .cols = where(is.numeric), 
                        .fns = list(sd = sd), na.rm = TRUE, 
                        .names = "{col}"
                      )) %>%
                      ungroup() %>%
                      filter(!is.na(group)) %>%
                      column_to_rownames("group") %>%
                      t() %>%
                      as_tibble() %>%
                      rename_all(~ paste0("sd_", .))
                    
                    group_means <- round(cbind(mean_group, sd_group), 2)
                    
                    ##
                    
                    if (input$univariate_test == "ttest"){
                      
                      # stat <- function(x) {t.test(x ~ Group, na.rm = TRUE, alternative = c("two.sided"),
                      #                             var.equal = input$var_ttest, paired = input$paired_ttest)$p.value}
                      
                      stat <- function(x) {t.test(x ~ Group, na.rm = TRUE, alternative = c("two.sided"),
                                                  var.equal = FALSE, paired = FALSE)$p.value}
                      
                      p <- data.frame(pvalue = apply(FUN = stat, MARGIN = 2, X = e))
                      p <- p %>%
                        rownames_to_column("ID") %>%
                        mutate(pvalueAdj = p.adjust(pvalue, method = "fdr")) %>%
                        column_to_rownames("ID")

                      p <- cbind(group_means, p) %>%
                        rownames_to_column("ID") %>%
                        mutate(Fold_Change_Ratio = as.numeric(round(group_means[, 2]/group_means[, 1], 3)),
                               Difference_Of_Means = as.numeric(round(group_means[, 2] - group_means[, 1], 3))) %>%
                        column_to_rownames("ID") %>%
                        dplyr::select(1,2,3,4,7,8,5,6)
                      
                      return(list(param_ttest = p))
                    }
                    
                    ##
                    
                    else if (input$univariate_test == "anova"){
                      
                      stat2 <- function(x) {anova(aov(x ~ Group))$"Pr(>F)"[1]}
                      p2 <- data.frame(pvalue = apply(FUN = stat2, MARGIN = 2, X = e))
                      p2 <- p2 %>% 
                        rownames_to_column("ID") %>%
                        mutate(pvalueAdj = p.adjust(pvalue, method = "fdr")) %>%
                        column_to_rownames("ID")
                      p2 <- bind_cols(group_means, p2)
                      
                      return(list(param_anova = p2))

                    }
                    
                    ##
                    
                    else if (input$univariate_test == "mann"){
                      
                      # non_param_mann <- data.frame(pvalue = apply(e, 2, function(x) {wilcox.test(x ~ as.factor(Group), 
                      #                                                                            paired = input$paired_mann)$p.value}))
                      
                      non_param_mann <- data.frame(pvalue = apply(e, 2, function(x) {wilcox.test(x ~ as.factor(Group), 
                                                                                                 paired = FALSE)$p.value}))
                      
                      non_param_mann <- non_param_mann %>% 
                        rownames_to_column("ID") %>% 
                        mutate(pvalueAdj = p.adjust(pvalue, method = "fdr")) %>% 
                        column_to_rownames("ID")
                      non_param_mann <- cbind(group_means, non_param_mann) %>% 
                        rownames_to_column("ID") %>% 
                        mutate(Fold_Change_Ratio = as.numeric(round(group_means[, 2]/group_means[, 1], 3)), 
                               Difference_Of_Means = as.numeric(round(group_means[, 2] - group_means[, 1], 3))) %>% 
                        column_to_rownames("ID") %>% 
                        dplyr::select(1, 2, 3, 4, 7, 8, 5, 6)
                      
                      return(list(non_param_mann = non_param_mann))
                      
                    }
                    
                    ##
                    
                    else if (input$univariate_test == "kruskal"){
                      
                      non_param_kru <- data.frame(pvalue = apply(e, 2, function(x) {kruskal.test(x ~ as.factor(Group))$p.value}))
                      non_param_kru <- non_param_kru %>% 
                        rownames_to_column("ID") %>%
                        mutate(pvalueAdj = p.adjust(pvalue, method = "fdr"), 
                               Kruskal_Wallis_Rank_Sum = apply(e, 2, function(x) {kruskal.test(x ~ as.factor(Group))$statistic})) %>%
                        column_to_rownames("ID")
                      non_param_kru <- bind_cols(group_means, non_param_kru)
                      
                      return(list(non_param_kru = non_param_kru))
                    }
                    })
                  })

####

output$matriu_anova <- DT::renderDataTable({
  
  param_anova <- Univ_analisis()$param_anova
  DT::datatable(param_anova,
                filter = 'top',extensions = 'Buttons',
                escape=FALSE,  rownames=TRUE, class = 'cell-border stripe',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = 
                    list("copy", "print", list(
                      extend="collection",
                      buttons=list(list(extend="csv",
                                        filename="anova_results"),
                                   list(extend="excel",
                                        filename="anova_results"),
                                   list(extend="pdf",
                                        filename="anova_results")),
                      text="Dowload")),
                  order=list(list(2, "desc")),
                  pageLength = nrow(param_anova)))
})

output$matriu_ttest <- DT::renderDataTable({

  param_ttest <- Univ_analisis()$param_ttest
                
        DT::datatable(param_ttest,
                filter = 'top',extensions = 'Buttons',
                escape=FALSE,  rownames=TRUE, class = 'cell-border stripe',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = 
                    list("copy", "print", list(
                      extend="collection",
                      buttons=list(list(extend="csv",
                                        filename="ttest_results"),
                                   list(extend="excel",
                                        filename="ttest_results"),
                                   list(extend="pdf",
                                        filename="ttest_results")),
                      text="Dowload")),
                  order=list(list(2, "desc")),
                  pageLength = nrow(param_ttest)))
})

###

output$matriu_mann <- DT::renderDataTable({
  
  non_param_mann <- Univ_analisis()$non_param_mann
    
  DT::datatable(non_param_mann, 
                filter = 'top',extensions = 'Buttons',
                escape=FALSE,  rownames=TRUE, class = 'cell-border stripe',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = 
                    list("copy", "print", list(
                      extend="collection",
                      buttons=list(list(extend="csv",
                                        filename="mann_whitney_results"),
                                   list(extend="excel",
                                        filename="mann_whitney_results"),
                                   list(extend="pdf",
                                        filename="mann_whitney_results")),
                      text="Dowload")),
                  order=list(list(2, "desc")),
                  pageLength = nrow(non_param_mann)))
})

###

output$matriu_kruskal <- DT::renderDataTable({
  
  non_param_kru <- Univ_analisis()$non_param_kru
  
  DT::datatable(non_param_kru, 
                filter = 'top',extensions = 'Buttons',
                escape=FALSE,  rownames=TRUE, class = 'cell-border stripe',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = 
                    list("copy", "print", list(
                      extend="collection",
                      buttons=list(list(extend="csv",
                                        filename="kruskal_wallis_results"),
                                   list(extend="excel",
                                        filename="kruskal_wallis_results"),
                                   list(extend="pdf",
                                        filename="kruskal_wallis_results")),
                      text="Dowload")),
                  order=list(list(2, "desc")),
                  pageLength = nrow(non_param_kru)))
})

##

output$univ_boxplot <- renderPlot({
  
  mynum <- input$feat_uni
  myfac <- input$fact_uni
  
  ##
  
  data_fac <- processedInput() %>%
    dplyr::select_at(vars(matches(myfac)))
  data_num <- processedInput() %>%
    dplyr::select_at(vars(matches(mynum)))
  data_subset <- cbind(data_fac, data_num)
  colnames(data_subset) <- c("my_factor", "my_numeric")
  
  if(!is.null(input$contents_proc_rows_selected)){
    data_subset <- data_subset[input$contents_proc_rows_selected ,]
  } 
  
  if(input$univariate_test %in% c("ttest", "anova")){
    type_boxplot <- "parametric"
  } else {
    type_boxplot <- "nonparametric"
  }
  
  ##
  
  # if(input$var_ttest){
  #   
  #   ggstatsplot::ggbetweenstats(
  #     data = data_subset,
  #     x = my_factor,
  #     y = my_numeric,
  #     title = "",
  #     messages = FALSE,
  #     type = type_boxplot,
  #     var.equal = TRUE,
  #     pairwise.display = "all",
  #     pairwise.comparisons = TRUE,
  #     p.adjust.method = "holm",
  #     results.subtitle = TRUE,
  #     xlab = myfac,
  #     ylab = mynum)
  #   
  # } else{
    
    ggstatsplot::ggbetweenstats(
      data = data_subset,
      x = my_factor,
      y = my_numeric,
      title = "",
      messages = FALSE,
      type = type_boxplot,
      var.equal = FALSE,
      pairwise.display = "all",
      pairwise.comparisons = TRUE,
      p.adjust.method = "holm",
      results.subtitle = TRUE,
      xlab = myfac,
      ylab = mynum)
  # }
  
})

##

output$univ_boxplot_int <- renderPlotly({
  
  mynum <- input$feat_uni
  myfac <- input$fact_uni
  
  ##
  
  data_fac <- processedInput() %>%
    dplyr::select_at(vars(matches(myfac)))
  data_num <- processedInput() %>%
    dplyr::select_at(vars(matches(mynum)))
  data_id <- processedInput() %>%
    dplyr::select(id)
  data_subset <- cbind(data_id, data_fac, data_num) %>%
    rename(id = 1, my_factor = 2, my_numeric = 3) %>%
    mutate(my_factor = as.factor(my_factor))
  
  if(!is.null(input$contents_proc_rows_selected)){
    data_subset <- data_subset[input$contents_proc_rows_selected ,]
  } 
  
  ggplotly(
    
    ggplot(data_subset, aes(x = my_factor, y = my_numeric, label = id)) +
      geom_jitter(alpha = 0.4, aes(fill = my_factor, color = my_factor)) +
      geom_violin(alpha = 0.8) +
      geom_boxplot(alpha = 0) +
      xlab(myfac) +
      ylab(mynum) +
      theme_bw() +
      theme(legend.title = element_blank())
    
  )
  
})

