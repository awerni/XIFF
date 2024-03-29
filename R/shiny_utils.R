#' Shiny Utility Functions
#'
#' @param expr 
#' @param errorId 
#' @param session shiny session object
#' @param callback 
#'
#' @rdname shiny-utility
#' @export
#'
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

#' @rdname shiny-utility
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

#' @rdname shiny-utility
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

#' Add help text to selectInput shiny object
#' 
#' @param param selectInput shiny object
#' @param helpText 
#'
#' @rdname inputWithHelp
#' @export
#' 
#' @examples 
#' 
#' select <- shiny::selectInput("x", "Input", 1:2)
#' select2 <- inputWithHelp(select, "Help!")
#' 
#' if(interactive()) {
#'   shinyApp(fluidPage(select2), function(input, output, session) {})
#' }
#' 
inputWithHelp <- function(param, helpText = "help me Obi-Wan Kenobi"){
  hi <- helpIcon(helpText)

  param$children[[1]]$children[[1]] <- div(
    param$children[[1]]$children[[1]],
    hi
  )
  param
}

#' @rdname inputWithHelp
#' @export
helpIcon <- function(text){
  res <- icon("question-circle")
  res$attribs[["data-toggle"]] <- "tooltip"
  res$attribs[["title"]] <- text
  
  res
}

#' Convenience Shiny Column Functions
#' 
#' @param ... shiny ui elements
#'
#' @rdname column_x
#' @export
#' 
column_1 <- function(...) column(width = 1, ...)

#' @rdname column_x
#' @export
column_2 <- function(...) column(width = 2, ...)

#' @rdname column_x
#' @export
column_3 <- function(...) column(width = 3, ...)

#' @rdname column_x
#' @export
column_4 <- function(...) column(width = 4, ...)

#' @rdname column_x
#' @export
column_5 <- function(...) column(width = 5, ...)

#' @rdname column_x
#' @export
column_6 <- function(...) column(width = 6, ...)

#' @rdname column_x
#' @export
column_7 <- function(...) column(width = 7, ...)

#' @rdname column_x
#' @export
column_8 <- function(...) column(width = 8, ...)

#' @rdname column_x
#' @export
column_9 <- function(...) column(width = 9, ...)

#' @rdname column_x
#' @export
column_10 <- function(...) column(width = 10, ...)

#' @rdname column_x
#' @export
column_11 <- function(...) column(width = 11, ...)

#' @rdname column_x
#' @export
column_12 <- function(...) column(width = 12, ...)

#' @rdname column_x
#' @export
fluidRow_12 <- function(...) fluidRow(column_12(...))


#' DT Container with smaller font
#'
#' @param id shiny id
#'
#' @return div with datatable container
#' @export
containerDT <- function(id){
  div(
    style = "font-size:80%",
    DT::dataTableOutput(id)
  )
}

#' @rdname shiny-utility
#' @export
styleHigherCol <- function(condition, cl){
  c1 <- paste0('<span style="color:red">', cl$class1_name, '</span>')
  c2 <- paste0('<span style="color:blue">', cl$class2_name, '</span>')
  ifelse(condition, c1, c2)
}

#' @rdname shiny-utility
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

  if (settings[["db"]][["useGCloudAuth"]] %||% FALSE){
    options("useGCloudAuth" = TRUE)
  } else {
    options("useGCloudAuth" = FALSE)
    options("dbuser" = settings[["db"]][["user"]])
    options("dbpass" = settings[["db"]][["password"]])
  }
}

#' Check Database Connection
#'
#' @param timeout timout for database connection
#'
#' @return logical, TRUE if database connection works, or FALSE otherwise 
#' @export
#'
initialDbCheck <- function(timeout = 5){
  dbStatus <- isDbOnline(timeout)
  if (!dbStatus){
    message("Cannot connect to DB!")
    message("Reason: ", attr(dbStatus, "reason"))

    return(structure(FALSE, reason = "db"))
  }

  TRUE
}

#' @rdname shiny-utility
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

#' Freeze Class Labels
#'
#' @param output shiny output object
#' @param classLabel reactive containing class labels
#' @param Results reactive containing the result of FutureManager
#' @param fm FutureManager object
#' @param id shiny id
#'
#' @details Used for keeping the old class labels values if Results object
#' is invalidated.
#'
#' @export
#'
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
#' @param title character string
#' @param value character string
#' @param inputMenu shiny tag
#' @param outputArea shiny tag
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
#' @param id shiny id
#' @param docuLink link to the documentation
#' @param aboutTabUIFunc function that creates about ui
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

#' Get future strategy
#' 
#' Returns strategy for the background calculations. Settings should contain
#' settings$options$fm_strategy field with a character string: "auto", "multicore",
#' or "multisession". Auto strategy will use 
#' \code{\link[FutureManager]{fmParallelStrategy}}, other choices refer to 
#' \code{\link[future]{multicore}} and \code{\link[future]{multisession}} respectively.
#' 
#' For debug purposes \code{\link[future]{sequential}} is used - to enable it please 
#' set `XIFF_FM_DEBUG` environmental variable to "YES".
#' 
#' @param settings application settings list
#' @return future strategy
#' @export
getFmStrategy <- function(settings = NULL){
  if (Sys.getenv("XIFF_FM_DEBUG", unset = "NO") != "NO") {
    warning(
      "You're using the sequential future strategy (debug mode). ",
      "To disable it please set 'XIFF_FM_DEBUG' env variable to 'NO'"
    )
    return(future::sequential)
  }
  
  defaultStrategy <- if (exists("fmParallelStrategy", where = asNamespace("FutureManager"), mode = "function")) {
    # Tests if the fmParallelStrategy is exported by the FutureManager
    # in the long run the if statement should be removed and only this branch should be used.
    # (after FutureManager CRAN release)
    FutureManager::fmParallelStrategy()
  } else {
    # TODO: remove when all environments will be updated to new FutureManager version
    # that exports fmParallelStrategy
    FutureManager::multiprocess
  }
  
  strategy <- settings$options$fm_strategy
  if (is.null(strategy)){
    defaultStrategy
  } else {
    switch(
      EXPR = strategy,
      auto = defaultStrategy,
      multicore = future::multicore,
      multisession = future::multisession,
      stop("'strategy' should be one of 'auto', 'multicore', 'multisession'")
    )
  }
}
