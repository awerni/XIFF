#' @export
withErrorHandler <- function(expr, errorId = NULL, session = getDefaultReactiveDomain(), callback = NULL){
  tryCatch(
    expr = expr,
    error = function(e){
      errorMsg <- e$message
      
      if (is.character(errorId)){
        shinyBS::createAlert(
          session = session,
          anchorId = errorId,
          content = errorMsg,
          style = "danger"
        )
      }
      
      if (is.function(callback)){
        callback(errorMsg)
      }
    }
  )
}

#' @export
errorScreen <- function(reason){
  msg <- if (reason == "db"){
    "DB is not accessible"
  } else if (reason == "opts"){
    "Some options are missing"
  }

  shinyjs::addClass(selector = "body", class = "error-page")
  showModal(
    modalDialog(
      title = "App failed to start",
      div(
        icon("bomb"),
        br(),
        tags$p(msg),
        tags$p("Please contact the system administrator."),
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

#' @export
column_1 <- function(...) column(width = 1, ...)

#' @export
column_2 <- function(...) column(width = 2, ...)

#' @export
column_3 <- function(...) column(width = 3, ...)

#' @export
column_4 <- function(...) column(width = 4, ...)

#' @export
column_5 <- function(...) column(width = 5, ...)

#' @export
column_6 <- function(...) column(width = 6, ...)

#' @export
column_7 <- function(...) column(width = 7, ...)

#' @export
column_8 <- function(...) column(width = 8, ...)

#' @export
column_9 <- function(...) column(width = 9, ...)

#' @export
column_10 <- function(...) column(width = 10, ...)

#' @export
column_11 <- function(...) column(width = 11, ...)

#' @export
column_12 <- function(...) column(width = 12, ...)

#' @export
fluidRow_12 <- function(...) fluidRow(column_12(...))

#' @export
containerDT <- function(id){
  div(
    style = "font-size:80%",
    DT::dataTableOutput(id)
  )
}

#' @export
styleHigherCol <- function(condition, cl){
  c1 <- paste0('<span style="color:red">', cl$class1_name, '</span>')
  c2 <- paste0('<span style="color:blue">', cl$class2_name, '</span>')
  ifelse(condition, c1, c2)
}

#' @export
getEnsgRowCallback <- function(species, idx = 1){
  spFull <- getEnsemblSpecies(species)
  htmlwidgets::JS(paste0("function(row, data, dataIndex) { ensgRowCallback(row, data, '", spFull, "', ", idx - 1, "); }"))
}

#' Set DB options
#' 
#' This function sets all R options required for DB connection based on the
#' settings object.
#' 
#' @param settings list of settings
#' @return nothing 
#' @export
setDbOptions <- function(settings){
  options("dbname" = settings[["db"]][["name"]])
  options("dbhost" = settings[["db"]][["host"]])
  options("dbport" = settings[["db"]][["port"]])
  options("dbuser" = settings[["db"]][["user"]])
  options("dbpass" = settings[["db"]][["password"]])
}

#' @export
initialDbCheck <- function(timeout = 5){
  dbStatus <- isDbOnline(timeout)
  if (!dbStatus){
    message("Cannot connect to DB!")
    message("Reason: ", attr(dbStatus, "reason"))

    return(structure(FALSE, reason = "db"))
  }

  TRUE
}

#' @export
validateArgs <- function(args){
  filtered <- args[!vapply(args, is.null, FUN.VALUE=logical(1))]
  length(filtered) == length(args)
}
