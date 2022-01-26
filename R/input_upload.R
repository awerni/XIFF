#' @export
#' @rdname uploadInputMode
uploadInputModeUI <- function(id, allowRds = FALSE, helpUrl = NULL){
  ns <- NS(id)

  acceptExt <- c(
    "text/csv", "text/comma-separated-values,text/plain", ".csv",
    "text/tsv", "text/tab-separated-values,text/plain", ".tsv",
    "text/txt", ".txt",
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    "application/vnd.ms-excel", ".xlsx"
  )

  label <- "Provide a text- or MS-Excel-file"

  if (allowRds){
    label <- paste(label, "or a machine learning model file")
    acceptExt <- c(acceptExt, ".rds")
  }

  helpLink <- if (!is.null(helpUrl)){
    a(
      href = helpUrl,
      target = "_blank",
      "[help]"
    )
  }

  list(
    fluidRow_12(h5(label, helpLink)),
    shinyBS::bsAlert(ns("top_error")),
    fluidRow(
      column_4(
        fileInput(
          inputId = ns("upload"),
          label = "Choose File",
          multiple = FALSE,
          accept = acceptExt
        ),
        uiOutput(ns("stat"))
      ),
      column_8(uiOutput(ns("options")))
    ),
    uiOutput(ns("plot")),
    shinyBS::bsAlert(ns("bottom_error"))
  )
}


#' Upload Input Mode
#'
#' @param input 
#' @param output 
#' @param session 
#' @param AnnotationFull 
#' @param translationFun 
#' @param AllTumortype 
#' @param AnnotationFiltered 
#' @param mlUseTumortypeFilter 
#'
#' @export
#' @rdname uploadInputMode
#' 
uploadInputMode <- function(input, output, session, AnnotationFull, translationFun, AllTumortype = NULL, AnnotationFiltered = NULL, mlUseTumortypeFilter = TRUE){
  ns <- session$ns

  Dummy_rds <- reactiveVal()
  Dummy_xlsx <- reactiveVal()

  FileInfo <- reactive({
    info <- input$upload
    req(info)

    info[["isXlsx"]] <- grepl("xlsx$", info$datapath)
    info[["isRds"]] <- grepl("rds$", info$datapath)

    Dummy_xlsx(info[["isXlsx"]])
    Dummy_rds(info[["isRds"]])
    info
  })

  output$stat <- renderUI({
    isRds <- Dummy_rds()
    isXlsx <- Dummy_xlsx()
    req(is.logical(isRds) && is.logical(isXlsx))

    if (isRds){
      mlUploadInputModeUI_stat(ns("ml"))
    } else {
      classicUploadInputModeUI_stat(ns("classic"), isXlsx)
    }
  })

  output$options <- renderUI({
    isRds <- Dummy_rds()
    req(is.logical(isRds))

    if (isRds){
      mlUploadInputModeUI_options(ns("ml"), input, AllTumortype)
    } else {
      classicUploadInputModeUI_options(ns("classic"))
    }
  })

  output$plot <- renderUI({
    isRds <- Dummy_rds()
    req(is.logical(isRds))

    if (isRds){
      mlUploadInputModeUI_plot(ns("ml"))
    } else {
      classicUploadInputModeUI_plot(ns("classic"))
    }
  })

  topErrorId <- ns("top_error")
  bottomErrorId <- ns("bottom_error")

  ClassicItems <- callModule(
    module = classicUploadInputMode,
    id = "classic",
    FileInfo = FileInfo,
    topErrorId = topErrorId,
    bottomErrorId = bottomErrorId,
    AnnotationFull = AnnotationFull,
    translationFun = translationFun
  )

  MlAnnotation <- reactive({

    if(mlUseTumortypeFilter) {
      AnnotationFull()
    } else {
      AnnotationFiltered()
    }

  })

  MLItems <- callModule(
    module = mlUploadInputMode,
    id = "ml",
    FileInfo = FileInfo,
    topErrorId = topErrorId,
    bottomErrorId = bottomErrorId,
    Annotation = MlAnnotation,
    translationFun = translationFun,
    useTumortypeFilter = mlUseTumortypeFilter
  )

  UploadItems <- reactive({
    isRds <- Dummy_rds()
    req(is.logical(isRds))

    if (isRds){
      MLItems()
    } else {
      ClassicItems()
    }
  })

  UploadItems
}

# classic upload --------------------------------------------------------------
classicUploadInputModeUI_stat <- function(id, isXlsx){
  ns <- NS(id)

  sheetSelector <- selectInput(
    inputId = ns("sheet"),
    label = "select sheet",
    choices = NULL
  )

  div(
    `if`(isXlsx, sheetSelector),
    textOutput(ns("stat"))
  )
}

classicUploadInputModeUI_options <- function(id){
  ns <- NS(id)

  fluidRow(
    column_6(
      selectInput(
        inputId = ns("column"),
        label = "select file column",
        choices = NULL
      ),
      checkboxInput(
        inputId = ns("display_bar"),
        label = "stack numeric values",
        value = FALSE
      )
    ),
    column_6(
      selectizeInput(
        inputId = ns("column_facet"),
        label = "split view by",
        choices = NULL
      )
    )
  )
}

classicUploadInputModeUI_plot <- function(id){
  ns <- NS(id)

  brushPlotUI(ns("barplot"))
}

classicUploadInputMode <- function(input, output, session, FileInfo, topErrorId, bottomErrorId, AnnotationFull, translationFun){
  colname <- getOption("xiff.column")
  colname <- rlang::sym(colname)

  ns <- session$ns
  rv <- reactiveValues()

  registerExtendedInputObserver(
    input = input,
    rv = rv,
    inputId = "sheet"
  )

  observeEvent(
    eventExpr = FileInfo(),
    handlerExpr = {
      info <- FileInfo()

      if (info[["isXlsx"]]){
        sheets <- readxl::excel_sheets(info$datapath)

        if (length(sheets) > 0){
          rv[["sheet"]] <- sheets[1]
          updateSelectInput(
            session = session,
            inputId = "sheet",
            choices = sheets,
            selected = sheets[1]
          )
        }
      }
    }
  )

  fileUploadRaw <- reactive({
    info <- FileInfo()
    req(!is.null(info) && !info[["isRds"]])

    tryCatch(
      expr = {
        file_name <- info$datapath
        if (info[["isXlsx"]]) {
          sheet <- rv[["sheet"]]
          validate(need(sheet, "select sheet first"))

          df <- readxl::read_xlsx(
            path = file_name,
            sheet = sheet,
            na = c("", "NA"),
            guess_max = 200
          )
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
      error = function(e) stop(safeError(e))
    )

    df <- removeEmptyColumns(df)

    anno <- AnnotationFull() %>% select(!!colname, tumortype)
    if ("tumortype" %in% colnames(df)) df <- df %>% select(-tumortype)
    ret <- translationFun(df, anno)

    if ("tumortype" %in% colnames(ret$df)) ret$df <- ret$df %>% relocate(tumortype, .after = 1)

    if (is.null(ret)) {
      shinyBS::createAlert(
        session = session,
        anchorId = topErrorId,
        content = paste("Could not find any column with", getOption("xiff.label"), "names or identifiers<br>"),
        style = "danger"
      )
      return()
    }

    df <- ret$df
    if (length(ret$missing) > 0) {
      shinyBS::createAlert(
        session = session,
        anchorId = bottomErrorId,
        content = paste0("unknown ", getOption("xiff.label"), "s:<br>", paste(ret$missing, collapse = "<br>")),
        style = "danger"
      )
    }

    col_type <- purrr::map_chr(df, ~ class(.x)[1])
    return(list(df = df, col_type = col_type))
  })

  observeEvent(fileUploadRaw(), {
    df <- fileUploadRaw()$df
    req(df)

    d <- df %>% select(-!!colname)
    updateSplitChoices("column", "column_facet", d, input, session)
  })

  fileUploadIDs <- reactive({
    file_data <- fileUploadRaw()
    req(file_data)

    sel_col <- input$column
    facet_col <- input$column_facet
    req(sel_col)
    req(facet_col)

    d <- file_data$df
    if ((is.null(d)) | (!sel_col %in% names(file_data$col_type))) return()

    shouldFacet <- facet_col != "-- none --"

    if (shouldFacet){
      facet_col <- sym(facet_col)
      d <- d %>% mutate(facet_var = !!facet_col)
    } else {
      d <- d %>% mutate(facet_var = "dummy")
    }

    sel_type <- file_data$col_type[[sel_col]]
    d <- d %>%
      rename(x_score = !!sel_col) %>%
      filter(!is.na(x_score), !is.na(facet_var)) %>%
      arrange(desc(x_score), as.character(!!colname)) %>%
      mutate(!!colname := forcats::as_factor(as.character(!!colname)))

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

  UploadPlot <- reactive({
    validate(
      need(!is.null(fileUploadIDs()), "no file data available")
    )
    file_data <- fileUploadIDs()
    d <- file_data$df
    if (is.null(d)) return()

    if (file_data$col_type_selected != "numeric" || (file_data$bar_possible && input$display_bar)){
      g <- generateScoreBarPlot(d, file_data$sel_col)
    } else {
      my_scale <- guessAxisScale(d$x_score)
      g <- generateScoreWaterfallPlot(d, file_data$sel_col, my_scale)
    }

    if (file_data$shouldFacet){
      g <- g + facet_grid(. ~ facet_var, scales = "free_x", space = "free")
    }

    return(g)
  })
  UploadPlotCheck <- reactive({
    !is.null(fileUploadIDs()) && !is.null(input$display_bar)
  })

  Upload_items <- callModule(
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

  output$stat <- renderText({
    req(fileUploadIDs()$df)
    paste(get_label(nrow(fileUploadIDs()$df)), "found")
  })

  observeEvent(input$upload, {
    session$resetBrush("plot_brush")
  })

  Upload_items
}

updateSplitChoices <- function(basicId, splitId, df, input, session){
  choices <- names(df)
  if (length(choices) == 0) return()

  selected <- isolate(input[[basicId]])
  if (is.null(selected) || !nzchar(selected) || !selected %in% choices){
    selected <- choices[[1]]
  }


  choices_facet <- df %>%
    filterSplitChoices() %>%
    names()
  facetSelected <- isolate(input[[splitId]])
  if (is.null(facetSelected) ||
      !nzchar(facetSelected) ||
      !facetSelected %in% choices_facet){
    facetSelected <- "-- none --"
  }

  updateSelectInput(
    session = session,
    inputId = splitId,
    choices = c("-- none --", choices_facet),
    selected = facetSelected
  )
  updateSelectInput(
    session = session,
    inputId = basicId,
    choices = choices,
    selected = selected
  )
  freezeReactiveValue(input, splitId)
  freezeReactiveValue(input, basicId)
}

filterSplitChoices <- function(df, min_number = 10, min_percent = 5, min_distinct = 2, max_distinct = 4){
  filterFun <- function(x){
    n_dist <- n_distinct(x, na.rm = TRUE)
    if (n_dist < min_distinct || n_dist > max_distinct) return(FALSE)

    n_available <- sum(!is.na(x))
    n_total <- length(x)
    n_required <- min(min_number, min_percent / 100 * n_total)

    n_available >= n_required
  }

  df %>%
    select_if(~ is.character(.x) || is.factor(.x) || is.logical(.x)) %>%
    Filter(f = filterFun)
}

removeEmptyColumns <- function(df){
  validCols <- sapply(
    X = df,
    FUN = function(x){
      any(nzchar(as.character(x)) & !is.na(x))
    }
  )

  df[validCols]
}

# Machine learning upload -----------------------------------------------------
mlUploadInputModeUI_stat <- function(id){
  NULL
}

mlUploadInputModeUI_options <- function(id, input, AllTumortype = NULL){
  ns <- NS(id)

  if(!is.null(AllTumortype)) {
    myT <- AllTumortype()
  }


  fluidRow(
    column_6(
      radioButtons(
        inputId = ns("show"),
        label = "Show",
        choices = c("split training and unseen", "unseen only", "all"),
        selected = "split training and unseen"
      )
    ),
    if(!is.null(AllTumortype)) column_6(
      selectInput(
        inputId = ns("tumortype"),
        label = "Tumor Types:",
        multiple = TRUE,
        choices = myT,
        selected = isolate(input$tumortype),
        selectize = FALSE,
        size = 7
      ) %>%
        inputWithHelp("tumortypes from the training set are selected by default")
    )
  )
}

mlUploadInputModeUI_plot <- function(id){
  ns <- NS(id)
  brushPlotUI(ns("barplot"))
}

mlUploadInputMode <- function(input, output, session, FileInfo, topErrorId,
                              bottomErrorId, Annotation, translationFun, useTumortypeFilter = TRUE){
  ns <- session$ns

  colname <- getOption("xiff.column")
  colname <- rlang::sym(colname)

  Model <- reactive({
    info <- FileInfo()
    req(!is.null(info) && info[["isRds"]])

    model <- withProgress(
      expr = readRDS(info$datapath),
      value = 0.2,
      message = "loading ML model..."
    )

    validateXiffMachineLearningResult(model)
    log_trace("mlUploadInput: model loaded.")
    model
  })

  TrainingSet <- reactive({
    m <- Model()
    anno <- Annotation()
    req(m, anno)
    log_trace("mlUploadInput: Training Set - translating")
    translated <- translationFun(tibble(!!colname := m$trainingItems), anno)
    if (is.null(translated)) return()
    log_trace("mlUploadInput: Training Set - translated.")
    translated
  })

  observeEvent(
    eventExpr = Model(),
    handlerExpr = {
      m <- Model()
      req(m, useTumortypeFilter)

      cl <- TrainingSet()$df[[colname]]
      req(length(cl) > 0)

      tt <- Annotation() %>%
        filter(!!colname %in% cl) %>%
        pull(tumortype) %>%
        unique()

      log_trace("mlUploadInput: Updating the tumors")
      updateSelectInput(
        session = session,
        inputId = "tumortype",
        selected = tt
      )
    },
    priority = 100
  )

  DB_Data <- reactive({
    m <- Model()
    req(m)

    withProgress(
      expr = getRawDataForModel(features = m),
      value = 0.4,
      message = "fetching DB data..."
    )
  })

  Data <- reactive({
    m <- Model()
    d <- DB_Data()

    req(d, m, TrainingSet())
    trainingSet <- TrainingSet()$df[[colname]]

    if(useTumortypeFilter) {
      tumortypes <- input$tumortype
      if (length(tumortypes) == 0) return()

      items <- Annotation() %>%
        filter(tumortype %in% tumortypes) %>%
        pull(!!colname)
    } else {
      items <- Annotation() %>% pull(!!colname)
    }

    showType <- input$show

    if (showType == "all"){
      useTraining <- TRUE
      useFacets <- FALSE
    } else if (showType == "unseen only"){
      useTraining <- FALSE
      useFacets <- FALSE
    } else { # split view
      useTraining <- TRUE
      useFacets <- TRUE
    }

    if (!useTraining){
      items <- setdiff(items, trainingSet)
    }

    df <- d %>% filter(!!colname %in% items)

    validate(need(nrow(df) > 0, "no data available"))

    df <- df %>% tidyr::pivot_wider(names_from = ensg, values_from = score)

    assignment <- withProgress(
      expr = withErrorHandler(
        expr = predict(m, newdata = df, useClassLabels = FALSE),
        errorId = topErrorId,
        session = session
      ),
      value = 0.8,
      message = "predicting..."
    )

    if (is.null(assignment)) return()

    cl <- m$classLabel
    cl <- c(cl$class1_name, cl$class2_name)

    df <- tibble::tibble(
      !!colname := df[[colname]],
      class = assignment
    ) %>%
      filter(!is.na(class)) %>%
      mutate(
        class = ifelse(class == "class1", cl[1], cl[2]),
        class = factor(class, levels = cl)
      )

    if (useFacets){
      df <- df %>% mutate(
        type = factor(ifelse(
          test = !!colname %in% trainingSet,
          yes = "training",
          no = "unseen"
        ))
      )
    }

    df
  })

  UploadPlot <- reactive({
    df <- Data()
    req(df)

    p <- generateScoreBarPlot(df %>% rename(x_score = class), "class") +
      scale_y_continuous(breaks = scales::breaks_pretty(n = 5))

    if ("type" %in% names(df)){
      p <- p + facet_wrap(~ type, scales = "free_y")
    }

    p
  })

  UploadPlotCheck <- reactive({

    validate(need(!is.null(Model()), "Please Load model."))
    if(useTumortypeFilter) {
      validate(need(length(input$tumortype) > 0, "Please select tumors."))
    }
    validate(
      need(!is.null(Data()),
        paste(
          "No data for model.",
          "If this is not expected please contact the app authors"
        )
      )
    )

    TRUE
  })

  ML_items <- callModule(
    module = brushPlot,
    id = "barplot",
    plotExpr = UploadPlot,
    checkExpr = UploadPlotCheck,
    textCallback = function(n, rx, ry){
      paste(get_label(n), "selected")
    },
    message = "Recalculating plot",
    value = 0.3
  )

  ML_items
}
