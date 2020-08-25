fluidRow(
  column(width = 3,
         wellPanel(
           
           selectizeInput("fact_uni", label = "Select a factor variable:", choices = NULL),
           
           checkboxInput("sel_num_box", "Select a boxplot numeric variable (this parameter only affect to the boxplot)", FALSE),
           
           conditionalPanel(condition = "input.sel_num_box",
                            selectizeInput("feat_uni", label = "Select a numeric variable:", choices = NULL)),
           
           radioButtons("univariate_test",  h4("Univariate methods:"),
                        choices = c("T-test" = 'ttest',
                                    "ANOVA" = 'anova',
                                    "Mann-Whitney U Test" = 'mann',
                                    "Kruskal Wallis Test" = 'kruskal'),
                        selected = 'ttest'
                        ),
           
           # conditionalPanel(condition = ("input.univariate_test == 'ttest'"),
           #                  
           #                  radioButtons("var_ttest",  "Variances are equal:",
           #                               choices = c("TRUE" = TRUE, 
           #                                           "FALSE (Welch's T-test)" = FALSE),
           #                               selected = FALSE),
           #                  
           #                  radioButtons("paired_ttest",  "Paired samples:",
           #                               choices = c("TRUE" = TRUE, 
           #                                           "FALSE" = FALSE),
           #                               selected = FALSE)
           #                  ),
           
           # conditionalPanel(condition = ("input.univariate_test == 'mann'"),
           #                  
           #                  radioButtons("paired_mann",  h4("Paired samples:"),
           #                               choices = c("TRUE (Wilcoxon Signed Rank Test)" = TRUE, 
           #                                           "FALSE" = FALSE),
           #                               selected = FALSE)),
           
           actionButton("play_test","Analyze", icon("step-forward"),
                        style="color: #fff; background-color: #00b300; border-color: #009900") %>% helper(type = "markdown",
                                                                                                          title = "Univariate analysis helper",
                                                                                                          content = "univariate",
                                                                                                          icon = "question",
                                                                                                          colour = "green")
           
         )),
  
  column(width = 9,
         
         tabsetPanel(
           
           tabPanel("Results", 
                    
                    conditionalPanel(condition = ("input.univariate_test == 'ttest'"),
                                     DT::dataTableOutput("matriu_ttest")
                    ),
                    conditionalPanel(condition = ("input.univariate_test == 'anova'"),
                                     DT::dataTableOutput("matriu_anova")
                                     
                    ),
                    conditionalPanel(condition = ("input.univariate_test == 'mann'"),
                                     DT::dataTableOutput("matriu_mann")
                                     
                    ),
                    conditionalPanel(condition = ("input.univariate_test == 'kruskal'"),
                                     DT::dataTableOutput("matriu_kruskal")
                    )
                    ),
           tabPanel("Boxplot", plotOutput("univ_boxplot")),
           tabPanel("Interactive Boxplot", plotlyOutput("univ_boxplot_int"))
         )
  )
)

