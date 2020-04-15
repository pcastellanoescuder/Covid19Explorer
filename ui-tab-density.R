fluidRow(
  column(width = 3,
         wellPanel(

           selectizeInput("dens_feat", label = "Select your feature:", choices = NULL)

           # selectInput("my_factor2", label = "Select a factor:", choices = NULL)

           # checkboxInput("show_comp", "Show all transformations", TRUE)

  )),

  column(width = 9,

         plotOutput("densityplots")

         )
  )

