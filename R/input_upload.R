#' @export
uploadInputModeUI <- function(id){
  ns <- shiny::NS(id)

  list(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::h3(paste(
          "Provide a file with a header row, with", getOption("xiff.label"), "names and additional",
          "columns with numeric values like IC50s or character values like classifications"
        ))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::fileInput(
          inputId = ns("upload"),
          label = "Choose File",
          multiple = FALSE,
          accept = c(
            "text/csv", "text/comma-separated-values,text/plain", ".csv",
            "text/tsv", "text/tab-separated-values,text/plain", ".tsv",
            "text/txt", ".txt",
            "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
            "application/vnd.ms-excel", ".xlsx"
          )
        ),
        shiny::textOutput(ns("stat"))
      ),
      shiny::column(
        width = 4,
        shiny::selectInput(
          inputId = ns("column"),
          label = "select file column",
          choices = NULL
        ),
        shiny::checkboxInput(
          inputId = ns("display_bar"),
          label = "stack numeric values",
          value = FALSE
        )
      ),
      shiny::column(
        width = 4,
        shiny::selectizeInput(
          inputId = ns("column_facet"),
          label = "split view by",
          choices = NULL
        )
      )
    ),
    brushPlotUI(
      id = ns("barplot"),
      shinyBS::bsAlert(ns("unknown_items"))
    )
  )
}

#' @export
uploadInputMode <- function(input, output, session, AnnotationFull, translationFun){
  colname <- getOption("xiff.column")
  colname <- rlang::sym(colname)

  ns <- session$ns

  fileUploadRaw <- shiny::reactive({
    shiny::req(input$upload)

    tryCatch(
      expr = {
        file_name <- input$upload$datapath
        if (grepl("xlsx$", file_name)) {
          df <- readxl::read_xlsx(file_name, na = c("", "NA"), guess_max = 200)
        } else {
          l <- readLines(file_name, n = 1)
          if (grepl("\t", l)) {
            df <- readr::read_tsv(file_name, na = c("", "NA"), guess_max = 200)
          } else if (grepl(",", l)) {
            df <- readr::read_delim(file_name, delim = ",", na = c("", "NA"), guess_max = 200)
          } else if (grepl(";", l)) {
            df <- readr::read_delim(file_name, delim = ";", na = c("", "NA"), guess_max = 200)
          } else {
            df <- readr::read_csv2(file_name, na = c("", "NA"), guess_max = 200)
          }
        }
      },
      error = function(e) stop(shiny::safeError(e))
    )

    anno <- AnnotationFull() %>% dplyr::select(!!colname, tumortype)
    if ("tumortype" %in% colnames(df)) df <- df %>% dplyr::select(-tumortype)
    ret <- translationFun(df, anno)

    if (is.null(ret)) {
      shinyBS::createAlert(
        session = session,
        anchorId = ns("unknown_items"),
        content = "Could not find any column with", getOption("xiff.label"), "names or identifiers<br>",
        style = "danger"
      )
      return()
    }

    df <- ret$df
    if (length(ret$missing) > 0) {
      shinyBS::createAlert(
        session = session,
        anchorId = ns("unknown_items"),
        content = paste0("unknown ", getOption("xiff.label"), "s:<br>", paste(ret$missing, collapse = "<br>")),
        style = "danger"
      )
    }

    col_type <- purrr::map_chr(df, ~ class(.x)[1])
    return(list(df = df, col_type = col_type))
  })

  shiny::observe({
    df <- fileUploadRaw()$df
    shiny::req(df)

    d <- df %>% dplyr::select(-!!colname)
    updateSplitChoices("column", "column_facet", d, input, session)
  })

  fileUploadIDs <- shiny::reactive({
    file_data <- fileUploadRaw()
    shiny::req(file_data)

    sel_col <- input$column
    facet_col <- input$column_facet
    shiny::req(sel_col)
    shiny::req(facet_col)
    shiny::req(sel_col != facet_col)

    d <- file_data$df
    if ((is.null(d)) | (!sel_col %in% names(file_data$col_type))) return()

    shouldFacet <- facet_col != "-- none --"

    if (shouldFacet){
      facet_col <- sym(facet_col)
      d <- d %>% dplyr::mutate(facet_var = !!facet_col)
    } else {
      d <- d %>% dplyr::mutate(facet_var = "dummy")
    }

    sel_type <- file_data$col_type[[sel_col]]
    d <- d %>%
      dplyr::rename(x_score = !!sel_col) %>%
      dplyr::filter(!is.na(x_score), !is.na(facet_var)) %>%
      dplyr::arrange(dplyr::desc(x_score), as.character(!!colname)) %>%
      dplyr::mutate(!!colname := forcats::as_factor(as.character(!!colname)))

    if (sel_type == "numeric" && length(unique(d$x_score)) <= 10) {
      shinyjs::show("display_bar")
      bar_possible <- TRUE
    } else {
      #updateCheckboxInput(session, "display_bar", value = FALSE)
      shinyjs::hide("display_bar")
      bar_possible <- FALSE
    }

    list(
      df = d,
      col_type_selected = sel_type,
      sel_col = sel_col,
      bar_possible = bar_possible,
      shouldFacet = shouldFacet
    )
  })

  UploadPlot <- shiny::reactive({
    shiny::validate(
      shiny::need(!is.null(fileUploadIDs()), "no file data available")
    )
    file_data <- fileUploadIDs()
    d <- file_data$df
    if (is.null(d)) return()

    if (file_data$col_type_selected != "numeric" || (file_data$bar_possible && input$display_bar)){
      g <- generateScoreBarPlot(d, file_data$sel_col)
    } else {
      my_scale <- "norm"
      x_range <- range(d$x_score, na.rm = TRUE)
      if (min(x_range) > 0) {
        if ((x_range[[2]]/x_range[[1]]) >= 1000) my_scale = "log10"
      }
      g <- generateScoreWaterfallPlot(d, file_data$sel_col, my_scale)
    }

    if (file_data$shouldFacet){
      g <- g + ggplot2::facet_grid(. ~ facet_var, scales = "free_x", space = "free")
    }

    return(g)
  })
  UploadPlotCheck <- shiny::reactive({
    !is.null(fileUploadIDs()) && !is.null(input$display_bar)
  })

  Upload_items <- shiny::callModule(
    module = brushPlot,
    id = "barplot",
    plotExpr = UploadPlot,
    checkExpr = UploadPlotCheck,
    textCallback = function(n, rx, ry){
      myT <- paste(get_label(n), "selected.")
      if (all(!is.na(ry))) myT <- paste(myT, " Score from", signif(ry[1], 4), "to", signif(ry[2], 4))
      return(myT)
    },
    defaultCutoff_y = -1,
    message = "Retrieving data",
    value = 0.3
  )

  output$stat <- shiny::renderText({
    shiny::req(fileUploadIDs()$df)
    paste(get_label(nrow(fileUploadIDs()$df)), "found")
  })

  shiny::observeEvent(input$upload, {
    session$resetBrush("plot_brush")
  })

  Upload_items
}

#' @export
updateSplitChoices <- function(basicId, splitId, df, input, session){
  choices <- names(df)
  if (length(choices) == 0) return()

  selected <- input[[basicId]]
  if (is.null(selected) || !nzchar(selected)){
    selected <- choices[[1]]
  }

  choices_facet <- df %>%
    dplyr::select_if(~ is.character(.x) || is.factor(.x) || is.logical(.x)) %>%
    sapply(dplyr::n_distinct, na.rm = TRUE) %>%
    Filter(f = function(x) x > 1 && x < 5) %>%
    names() %>%
    setdiff(selected)

  shiny::updateSelectInput(
    session = session,
    inputId = splitId,
    choices = c("-- none --", choices_facet),
    selected = "-- none --"
  )
  shiny::updateSelectInput(
    session = session,
    inputId = basicId,
    choices = choices,
    selected = selected
  )
}
