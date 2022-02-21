#' Save Classes Properties in Shiny Application
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#' @param res data.frame with additional info about 
#' @param classSelection reactive containing class selection
#' @param classLabel reactive containing class labels
#' @param classStack reactive used to save data.frame with class properties
#' @param Annotation annotation data
#' @param msg additional message to be shown when saving data
#'
#' @details 
#' 
#' \code{saveProperties} is a function to be used in other modules to
#' save the properties for \code{restoreSelectionInputMode} module.
#'
#' @rdname restoreSelectionInputMode
#' @export
#'
restoreSelectionInputModeUI <- function(id, ...){
  ns <- NS(id)

  list(
    br(),
    ...,
    fluidRow(
      column_4(
        selectInput(
          inputId = ns("column"),
          label = "select table column",
          choices = NULL
        )
      ),
      column_4(
        selectizeInput(
          inputId = ns("column_facet"),
          label = "split view by",
          choices = NULL
        )
      ),
      column_4(
        checkboxInput(
          inputId = ns("display_bar"),
          label = "stack numeric values",
          value = FALSE)
      )
    ),
    brushPlotUI(ns("barplot"))
  )
}

#' @rdname restoreSelectionInputMode
#' @export
restoreSelectionInputMode <- function(input, output, session, classStack){
  colname <- getOption("xiff.column")
  colname <- rlang::sym(colname)
  
  observeEvent(classStack(), {
    df <- classStack()
    req(df)

    d <- df %>% select(-!!colname)
    updateSplitChoices("column", "column_facet", d, input, session)
  })

  RestorePlot <- reactive({
    df <- classStack()
    req(nrow(df) > 0)

    sel_col <- input$column
    facet_col <- input$column_facet
    req(sel_col)
    req(facet_col)
    req(!is.null(input$display_bar))

    shouldFacet <- facet_col != "-- none --"

    if (shouldFacet){
      facet_col <- rlang::sym(facet_col)
      df <- df %>% mutate(facet_var = !!facet_col)
    } else {
      df <- df %>% mutate(facet_var = "dummy")
    }

    d <- df %>% rename(x_score = !!sel_col)

    if (sel_col == "tumortype") d <- d %>% mutate(tumortype = x_score)

    d <- d %>%
      select(!!colname, x_score, facet_var, tumortype) %>%
      filter(!is.na(x_score), !is.na(facet_var)) %>%
      arrange(desc(x_score), as.character(!!colname)) %>%
      mutate(!!colname := forcats::as_factor(as.character(!!colname)))

    if (is.null(d)) return()

    # stack numeric only for numerics
    bar_possible <- class(d$x_score) == "numeric" && length(unique(d$x_score)) <= 10
    if (bar_possible){
      shinyjs::show("display_bar")
    } else {
      shinyjs::hide("display_bar")
    }

    if (class(d$x_score) != "numeric" || (bar_possible && input$display_bar)){
      d <- d %>% select(-tumortype)
      g <- generateScoreBarPlot(d, sel_col)

    } else {
      my_scale <- guessAxisScale(d$x_score)
      g <- generateScoreWaterfallPlot(d, sel_col, my_scale)
    }

    if (shouldFacet){
      g <- g + facet_grid(. ~ facet_var, scales = "free_x", space = "free")
    }

    return(g)
  })

  RestorePlotCheck <- reactive({
    !is.null(input$column) &&
      !is.null(input$column_facet) &&
      !is.null(input$display_bar) &&
      !is.null(classStack()) && 
      nrow(classStack()) > 0
  })

  Restore_items <- callModule(
    module = brushPlot,
    id = "barplot",
    plotExpr = RestorePlot,
    checkExpr = RestorePlotCheck,
    textCallback = function(n, rx, ry){
      paste(get_label(n), "selected.")
    },
    defaultCutoff_y = -1,
    message = "Plotting data",
    value = 0.3
  )

  observeEvent(classStack(), {
    session$resetBrush("plot_brush")
  })

  Restore_items
}

#' @rdname restoreSelectionInputMode
#' @export
saveProperties <- function(res, classSelection, classLabel, classStack, Annotation, msg = ""){
  n_cl1 <- length(classSelection$class1)
  n_cl2 <- length(classSelection$class2)

  colname <- getOption("xiff.column")
  label <- getOption("xiff.label")

  cs <- reactiveValuesToList(classSelection)
  cl <- reactiveValuesToList(classLabel)
  assignment <- stackClasses(cs, cl, return_factor = TRUE)

  if (nrow(assignment) > 0) {
    dialog_text <- paste0(
      "Saving ", msg, " of ", nrow(assignment), " ", label, "s.\n",
      "In 'Input' tab select 'restore selection' to use these properties",
      " for a new ", label, " classification "
    )

    if (is.data.frame(res) && nrow(res) > 0 && colname %in% names(res)){
      if ("class" %in% names(res)){
        res <- res %>% select(-class)
      }
      assignment <- assignment %>% left_join(res, by = colname)
    }

    df <- assignment %>%
      left_join(Annotation(), by = colname) %>%
      select(!!colname, tidyselect::everything())

    classStack(df)
  } else {
    dialog_text <- "Class selection is empty. Nothing saved."
  }
  showModal(
    modalDialog(
      title = paste0("Putting ", label, "s onto application stack"),
      dialog_text,
      easyClose = TRUE,
      footer = modalButton("ok")
    )
  )
}

