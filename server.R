
shinyServer(function(input, output, session) {

Logged <- FALSE
my_username <- "PujolBorrell"
my_password <- "Covid19shinyVH2020"
  
values <- reactiveValues(authenticated = FALSE)

# Return the UI for a modal dialog with data selection input. If 'failed' 
# is TRUE, then display a message that the previous value was invalid.
dataModal <- function(failed = FALSE) {
  modalDialog(
    textInput("username", "Username:"),
    passwordInput("password", "Password:"),
    footer = tagList(
      actionButton("ok", "OK")
    )
  )
}

# Show modal when button is clicked.  
# This `observe` is suspended only whith right user credential

obs1 <- observe({
  showModal(dataModal())
})

# When OK button is pressed, attempt to authenticate. If successful,
# remove the modal. 

obs2 <- observe({
  req(input$ok)
  isolate({
    Username <- input$username
    Password <- input$password
  })
  Id.username <- which(my_username == Username)
  Id.password <- which(my_password == Password)
  if (length(Id.username) > 0 & length(Id.password) > 0) {
    if (Id.username == Id.password) {
      Logged <<- TRUE
      values$authenticated <- TRUE
      obs1$suspend()
      removeModal()
      
    } else {
      values$authenticated <- FALSE
    }     
  }
})

source("server-inputdata.R",local = TRUE)
source("server-timeplots.R",local = TRUE)
source("server-correlations.R",local = TRUE)
# source("server-multivariate.R",local = TRUE)

})

