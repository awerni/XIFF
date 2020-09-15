#' @export
restoreSelectionInputModeUI <- function(id){
  ns <- shiny::NS(id)

  list(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::selectInput(
          inputId = ns("column"),
          label = "select table column",
          choices = NULL
        )
      ),
      shiny::column(
        width = 4,
        shiny::selectizeInput(
          inputId = ns("column_facet"),
          label = "split view by",
          choices = NULL
        )
      ),
      shiny::column(
        width = 4,
        shiny::checkboxInput(
          inputId = ns("display_bar"),
          label = "stack numeric values",
          value = FALSE)
      )
    ),
    brushPlotUI(ns("barplot"))
  )
}

#' @export
restoreSelectionInputMode <- function(input, output, session, classStack){
  colname <- getOption("xiff.column")
  colname <- rlang::sym(colname)

  ns <- session$ns

  shiny::observe({
    df <- classStack()
    shiny::req(df)

    d <- df %>% dplyr::select(-!!colname)
    updateSplitChoices("column", "column_facet", d, input, session)
  })

  RestorePlot <- shiny::reactive({
    df <- classStack()
    shiny::req(nrow(df) > 0)

    sel_col <- input$column
    facet_col <- input$column_facet
    shiny::req(sel_col)
    shiny::req(facet_col)
    shiny::req(sel_col != facet_col)
    shiny::req(!is.null(input$display_bar))

    shouldFacet <- facet_col != "-- none --"

    if (shouldFacet){
      facet_col <- rlang::sym(facet_col)
      df <- df %>% dplyr::mutate(facet_var = !!facet_col)
    } else {
      df <- df %>% dplyr::mutate(facet_var = "dummy")
    }

    d <- df %>% dplyr::rename(x_score = !!sel_col)

    if (sel_col == "tumortype") d <- d %>% dplyr::mutate(tumortype = x_score)

    d <- d %>%
      dplyr::select(!!colname, x_score, facet_var, tumortype) %>%
      dplyr::filter(!is.na(x_score), !is.na(facet_var)) %>%
      dplyr::arrange(dplyr::desc(x_score), as.character(!!colname)) %>%
      dplyr::mutate(!!colname := forcats::as_factor(as.character(!!colname)))

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
      my_scale = "norm"
      x_range <- range(d$x_score, na.rm = TRUE)
      if (min(x_range) > 0) {
        if ((x_range[[2]]/x_range[[1]]) >= 1000) my_scale = "log10"
      }
      g <- generateScoreWaterfallPlot(d, sel_col, my_scale)
    }

    if (shouldFacet){
      g <- g + ggplot2::facet_grid(. ~ facet_var, scales = "free_x", space = "free")
    }

    return(g)
  })

  RestorePlotCheck <- shiny::reactive({
    !is.null(input$column) &&
      !is.null(input$column_facet) &&
      !is.null(input$display_bar) &&
      nrow(classStack()) > 0
  })

  Restore_items <- shiny::callModule(
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

  shiny::observeEvent(classStack(), {
    session$resetBrush("plot_brush")
  })

  Restore_items
}

#' @export
saveProperties <- function(res, classSelection, classStack, Annotation, msg = ""){
  n_cl1 <- length(classSelection$class1)
  n_cl2 <- length(classSelection$class2)

  colname <- getOption("xiff.column")
  label <- getOption("xiff.label")

  if (nrow(res) > 0) {
    dialog_text <- paste0(
      "Saving ", msg, " of ", nrow(res), " ", label, "s.\n",
      "Goto 'input' and select 'restore selection' to use these properties",
      "for a new ", label, " classification "
    )

    df <- res %>%
      dplyr::left_join(Annotation(), by = colname) %>%
      dplyr::select(!!colname, tidyselect::everything())

    classStack(df)
  } else {
    dialog_text <- "Class selection is empty. Nothing saved."
  }
  shiny::showModal(
    shiny::modalDialog(
      title = paste0("Putting ", label, "s onto application stack"),
      dialog_text,
      easyClose = TRUE,
      footer = shiny::modalButton("ok")
    )
  )
}

