#' @export
errorScreen <- function(reason){
  msg <- if (reason == "db"){
    "DB is not accessible"
  } else if (reason == "opts"){
    "Some options are missing"
  }

  shinyjs::addClass(selector = "body", class = "error-page")
  shiny::showModal(
    shiny::modalDialog(
      title = "App failed to start",
      shiny::div(
        icon("bomb"),
        shiny::br(),
        shiny::tags$p(msg),
        shiny::tags$p("Please contact the system administrator."),
      ),
      footer = NULL,
      easyClose = FALSE,
      size = "l"
    )
  )
}
