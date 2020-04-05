
fluidRow(
  column(width = 3,
                wellPanel(
                  
                  selectInput("one", label = "Select variable 1:", choices = NULL),
                  selectInput("two", label = "Select variable 2:", choices = NULL),
                  
                  checkboxInput("smooth", "Smooth line (lm)"),
                  conditionalPanel(condition = ("input.smooth"),
                                   selectInput("smooth_color", "Smooth line colour", choices = c("red", "blue", "green"))),
                  
                  radioButtons("corr_method", "Correlation Method:", c("Pearson" = "pearson",
                                                                       "Spearman" = "spearman",
                                                                       "Kendall" = "kendall"))
  )),
  
  column(width = 8,
         
         fluidPage(
           tabsetPanel(
             tabPanel("Pairwise Correlation Scatterplot", 
                      plotlyOutput("cor_plot"))
             # tabPanel("Table of Correlations", dataTableOutput("corr_table"))
           ))

  ))

