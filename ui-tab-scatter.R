
fluidRow(
  column(width = 3,
                wellPanel(
                  
                  selectInput("one", label = "Select variable 1:", choices = NULL),
                  selectInput("two", label = "Select variable 2:", choices = NULL),
                  
                  selectInput("my_factor", label = "Select a factor:", choices = NULL),
                  checkboxInput("facet_factor", "Facet by factor"),
                  checkboxInput("showR", "Show correlation coefficients (pearson)"),
                  checkboxInput("showL", "Show labels"),
                  
                  checkboxInput("smooth", "Smooth line (lm)"),
                  conditionalPanel(condition = ("input.smooth"),
                                   selectInput("smooth_color", "Smooth line colour", choices = c("red", "blue", "green"))),
                  
                  radioButtons("corr_method", "Correlation Method:", c("Pearson" = "pearson",
                                                                       "Spearman" = "spearman",
                                                                       "Kendall" = "kendall")),
                  
                  actionButton("exclude_toggle", "Hide points", icon("ban"),
                               style="color: #fff; background-color: #FF0000; border-color: #AF0000"),
                  actionButton("exclude_reset", "Reset", icon("sync-alt"),
                               style="color: #fff; background-color: #00b300; border-color: #009900")
  )),
  
  column(width = 8,
         
         fluidPage(
           
           plotOutput("cor_plot", click = "plot1_click", brush = brushOpts(id = "plot1_brush"), height = "520px")
           )

  ))

