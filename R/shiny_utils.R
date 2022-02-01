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

  if(length(idx) > 1) {
    idx <- sprintf("[%s]", paste(idx - 1, collapse = ","))
  } else {
    idx <- idx - 1
  }

  htmlwidgets::JS(paste0("function(row, data, dataIndex) { ensgRowCallback(row, data, '", spFull, "', ", idx, "); }"))
}

#' Set DB options
#'
#' This function sets all R options required for DB connection based on the
#' settings object or environment variables (dbhost, dbname, dbuser, dbpass, dbport).
#'
#' @param settings list of settings
#' @return nothing
#' @export
setDbOptions <- function(settings = NULL){

  if (is.null(settings)) {
    useGCloudAuth <- as.logical(Sys.getenv("dbusegcloudauth", unset = FALSE))
    if (!is.logical(useGCloudAuth)){
      stop("dbusegcloudauth env variable should be a logical")
    }

    settings <- list(db = list(
      dbhost = Sys.getenv("dbhost"),
      dbname = Sys.getenv("dbname"),
      dbport = as.numeric(Sys.getenv("dbport", unset = 5432)),
      dbusegcloudauth = useGCloudAuth
    ))
    altNames <- c("host", "name", "port", "useGCloudAuth")

    if (!useGCloudAuth){
      settings$db <- c(
        settings$db,
        list(
          dbuser = Sys.getenv("dbuser"),
          dbpass = Sys.getenv("dbpass")
        )
      )
      altNames <- c(altNames, "user", "password")
    }

    dbConfigMissing <- vapply(settings[["db"]], function(x) x == "", FUN.VALUE = TRUE)

    if (any(dbConfigMissing)) {
      missingNames <- names(dbConfigMissing[dbConfigMissing])
      stop(paste0(
        "'settings' parameter was NULL. ",
        "XIFF::setDbOptions attempted to read the config from environment variables.\n",
        "However, parameters ", paste(missingNames, collapse = ", "),
        " are missing.\n",
        "Please set the proper values using either the '.Renviron' file (it requires the session restart)"),
        " or by using Sys.setenv() function:\n",
        paste(sprintf("Sys.setenv(%s = 'your-%s-here')", missingNames, missingNames), collapse = "\n")
      )
    }

    # Rename values to be settings compatible. Using verions with `db` prefix is better easier
    # for rendering the error function (env names can be directry read from the list).
    names(settings[["db"]]) <- altNames
  }

  options("dbname" = settings[["db"]][["name"]])
  options("dbhost" = settings[["db"]][["host"]])
  options("dbport" = settings[["db"]][["port"]])

  if (settings[["db"]][["useGCloudAuth"]]){
    options("useGCloudAuth" = TRUE)
  } else {
    options("useGCloudAuth" = FALSE)
    options("dbuser" = settings[["db"]][["user"]])
    options("dbpass" = settings[["db"]][["password"]])
  }
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

.isMustRerun <- function(id, fm) {
  status <- fm$getButtonState(id)
  if(is.null(status)) {
    FALSE
  } else {
    status$status == "rerun"
  }
}

#' @export
registerFreezedClassLabel <- function(output, classLabel, Results, fm, id) {
  resultClassLabel <- reactiveValues(class1_name = NULL, class2_name = NULL)

  output$runClassLabel <- renderUI({

    status <- .isMustRerun(id, fm)
    log_trace("Class label - {id} status: {status}.")

    if(!status) {
      resultClassLabel$class1_name <- classLabel$class1_name
      resultClassLabel$class2_name <- classLabel$class2_name
    }
    # to depend on Results(), but not render the message if Results is an error
    # or not yet completed
    try(Results(), silent = TRUE)

    NULL
  })

  resultClassLabel
}


#' Column Tab Panel
#'
#' @param title
#' @param value
#' @param inputMenu
#' @param outputArea
#'
#' @export
columnTabPanel <- function(title, value, inputMenu, outputArea) {

  if(is.null(inputMenu)) {
    tabPanel(
      title = title,
      value = value,
      fluidRow(
        column_12(outputArea)
      )
    )
  } else {
    tabPanel(
      title = title,
      value = value,
      fluidRow(
        column_2(inputMenu),
        column_10(outputArea)
      )
    )
  }
}


#' Tabsets for shiny application
#'
#' @param id
#' @param docuLink
#' @param aboutTabUIFunc
#'
#' @export
#' @rdname appTabsets
#'
appUI_main_about <- function(id, docuLink, aboutTabUIFunc) {
  ns <- NS(id)
  tabPanel(title = "About", aboutTabUIFunc(ns("about"), docuLink))
}

#' @export
#' @rdname appTabsets
appUI_title_right <- function(id, docuLink, packageName, dbName) {
  ns <- NS(id)
  list(
    bslib::nav_item(
      class = "nav-item-right",
      paste(
        "Version",
        as.character(packageVersion(packageName))
      )
    ),
    bslib::nav_item(
      class = "nav-item-right",
      dbName
    ),
    bslib::nav_item(
      class = "nav-item-right",
      a(
        id = ns("help_link"),
        href = docuLink,
        target = "_blank",
        "help"
      )
    )
  )
}

#' @export
#' @rdname appTabsets
appUI_title <- function(id, title, logoPath) {
  img(
    src = logoPath,
    title = title,
    height = "40px",
    style = "margin:10px;"
  )
}

#' Default app theme
#' 
#' bslib theme to be used in apps
#' 
#' @param version bootstrap version to use
#' @param font font name to use
#' @param ... arguments passed to bs_theme
#' 
#' @return 
#' @export
appTheme <- function(version = 4, font = "Roboto", ...){
  robotoFont <- bslib::font_collection(bslib::font_google(font), "sans-serif")
  
  bslib::bs_theme(
    # v4 is set, because v5 is not compatible with shinyWidgets yet 
    # https://github.com/dreamRs/shinyWidgets/issues/434
    version = version,
    bootswatch = "default",
    primary = "#18bc9c",
    "line-height-base" = 1.2,
    "table-cell-padding" = "0.3rem",
    "table-hover-bg" = "rgba(24, 188, 156, 0.1)",
    "table-active-bg" = "#18bc9c",
    base_font = robotoFont,
    heading_font = robotoFont,
    ...
  )
}
