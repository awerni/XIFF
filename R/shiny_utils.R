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

#' @export
registerExtendedInputObserver <- function(input, rv, inputId, callback = NULL, initValue = NULL, debug = FALSE, ignoreInit = TRUE, ...){
  id <- inputId # prevent lazy load
  f <- callback

  if (!is.null(initValue)){
    rv[[id]] <- initValue
  }

  observeEvent(
    eventExpr = input[[id]],
    handlerExpr = {
      old <- rv[[id]]
      new <- input[[id]]

      if (isDifferent(old, new)){
        if (debug){
          print(paste("new value:", id))
          print(all.equal(old, new))
          cat("\t", old, "->", new, "\n")
        }

        if (is.function(f)){
          f(old, new)
        }
        rv[[id]] <- new
      }
    },
    ignoreInit = ignoreInit,
    ...
  )
}

#' @export
inputWithHelp <- function(param, helpText = "help me Obi-Wan Kenobi"){
  helpIcon <- icon("question-circle")
  helpIcon$attribs[["data-toggle"]] <- "tooltip"
  helpIcon$attribs[["title"]] <- helpText

  param$children[[1]]$children[[1]] <- div(
    param$children[[1]]$children[[1]],
    helpIcon
  )
  param
}
