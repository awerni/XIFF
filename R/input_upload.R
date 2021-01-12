#' @export
uploadInputModeUI <- function(id, allowRds = FALSE){
  ns <- shiny::NS(id)

  acceptExt <- c(
    "text/csv", "text/comma-separated-values,text/plain", ".csv",
    "text/tsv", "text/tab-separated-values,text/plain", ".tsv",
    "text/txt", ".txt",
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    "application/vnd.ms-excel", ".xlsx"
  )

  if (allowRds){
    acceptExt <- c(acceptExt, ".rds")
  }

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
    shinyBS::bsAlert(ns("error")),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::fileInput(
          inputId = ns("upload"),
          label = "Choose File",
          multiple = FALSE,
          accept = acceptExt
        ),
        shiny::uiOutput(ns("stat"))
      ),
      shiny::column(
        width = 8,
        shiny::uiOutput(ns("options"))
      )
    ),
    shiny::uiOutput(ns("plot"))
  )
}

#' @export
uploadInputMode <- function(input, output, session, AnnotationFull, translationFun, AllTumortype){
  ns <- session$ns

  FileInfo <- shiny::reactive({
    info <- input$upload
    shiny::req(info)

    info[["isXlsx"]] <- grepl("xlsx$", info$datapath)
    info[["isRds"]] <- grepl("rds$", info$datapath)

    info
  })

  shiny::observeEvent(
    eventExpr = FileInfo(),
    handlerExpr = {
      info <- FileInfo()
      shiny::req(info)

      if (info[["isRds"]]){
        statContent <- mlUploadInputModeUI_stat(ns("ml"))
        optionsContent <- mlUploadInputModeUI_options(ns("ml"), input, AllTumortype)
        plotContent <- mlUploadInputModeUI_plot(ns("ml"))
      } else {
        statContent <- classicUploadInputModeUI_stat(ns("classic"))
        optionsContent <- classicUploadInputModeUI_options(ns("classic"))
        plotContent <- classicUploadInputModeUI_plot(ns("classic"))
      }

      output$stat <- shiny::renderUI({ statContent })
      output$options <- shiny::renderUI({ optionsContent })
      output$plot <- shiny::renderUI({ plotContent })
    }
  )

  errorId <- ns("error")

  ClassicItems <- shiny::callModule(
    module = classicUploadInputMode,
    id = "classic",
    FileInfo = FileInfo,
    errorId = errorId,
    AnnotationFull = AnnotationFull,
    translationFun = translationFun
  )

  MLItems <- shiny::callModule(
    module = mlUploadInputMode,
    id = "ml",
    FileInfo = FileInfo,
    errorId = errorId,
    Annotation = AnnotationFull
  )

  UploadItems <- shiny::reactive({
    info <- FileInfoTrigger()
    shiny::req(info)

    if (info[["isRds"]]){
      MLItems()
    } else {
      ClassicItems()
    }
  })

  UploadItems
}

# classic upload --------------------------------------------------------------
classicUploadInputModeUI_stat <- function(id){
  ns <- shiny::NS(id)
  shiny::textOutput(ns("stat"))
}

classicUploadInputModeUI_options <- function(id){
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      shiny::selectInput(
        inputId = ns("sheet"),
        label = "select sheet",
        choices = NULL
      ),
      shiny::checkboxInput(
        inputId = ns("display_bar"),
        label = "stack numeric values",
        value = FALSE
      )
    ),
    shiny::column(
      width = 6,
      shiny::selectInput(
        inputId = ns("column"),
        label = "select file column",
        choices = NULL
      ),
      shiny::selectizeInput(
        inputId = ns("column_facet"),
        label = "split view by",
        choices = NULL
      )
    )
  )
}

classicUploadInputModeUI_plot <- function(id){
  ns <- shiny::NS(id)

  brushPlotUI(
    id = ns("barplot"),
    shinyBS::bsAlert(ns("unknown_items"))
  )
}

classicUploadInputMode <- function(input, output, session, FileInfo, errorId, AnnotationFull, translationFun){
  colname <- getOption("xiff.column")
  colname <- rlang::sym(colname)

  ns <- session$ns
  rv <- shiny::reactiveValues()

  registerExtendedInputObserver(
    input = input,
    rv = rv,
    inputId = "sheet"
  )

  shiny::observeEvent(
    eventExpr = FileInfo(),
    handlerExpr = {
      info <- FileInfo()

      if (info[["isXlsx"]]){
        sheets <- readxl::excel_sheets(info$datapath)

        if (length(sheets) > 0){
          rv[["sheet"]] <- sheets[1]
          shiny::updateSelectInput(
            session = session,
            inputId = "sheet",
            choices = sheets,
            selected = sheets[1]
          )
        }
        shinyjs::show("sheet")
      } else {
        shinyjs::hide("sheet")
      }
    }
  )

  fileUploadRaw <- shiny::reactive({
    info <- FileInfo()
    shiny::req(!is.null(info) && !info[["isRds"]])

    tryCatch(
      expr = {
        file_name <- info$datapath
        if (info[["isXlsx"]]) {
          sheet <- rv[["sheet"]]
          shiny::validate(shiny::need(sheet, "select sheet first"))

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
      error = function(e) stop(shiny::safeError(e))
    )

    df <- removeEmptyColumns(df)

    anno <- AnnotationFull() %>% dplyr::select(!!colname, tumortype)
    if ("tumortype" %in% colnames(df)) df <- df %>% dplyr::select(-tumortype)
    ret <- translationFun(df, anno)

    if ("tumortype" %in% colnames(ret$df)) ret$df <- ret$df %>% relocate(tumortype, .after = 1)

    if (is.null(ret)) {
      shinyBS::createAlert(
        session = session,
        anchorId = errorId,
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
      my_scale <- guessAxisScale(d$x_score)
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
    filterSplitChoices() %>%
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

filterSplitChoices <- function(df, min_number = 10, min_percent = 5, min_distinct = 2, max_distinct = 4){
  filterFun <- function(x){
    n_dist <- dplyr::n_distinct(x, na.rm = TRUE)
    if (n_dist < min_distinct || n_dist > max_distinct) return(FALSE)

    n_available <- sum(!is.na(x))
    n_total <- length(x)
    n_required <- min(min_number, min_percent / 100 * n_total)

    n_available >= n_required
  }

  df %>%
    dplyr::select_if(~ is.character(.x) || is.factor(.x) || is.logical(.x)) %>%
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

mlUploadInputModeUI_options <- function(id, input, AllTumortype){
  ns <- shiny::NS(id)
  myT <- AllTumortype()

  shiny::fluidRow(
    shiny::column(
      width = 6,
      shiny::radioButtons(
        inputId = ns("show"),
        label = "Show",
        choices = c("split training and unseen", "unseen only", "all"),
        selected = "split training and unseen"
      )
    ),
    shiny::column(
      width = 6,
      shiny::selectInput(
        inputId = ns("tumortype"),
        label = "Tumor Types:",
        multiple = TRUE,
        choices = myT,
        selected = shiny::isolate(input$tumortype),
        selectize = FALSE,
        size = 7
      ) %>%
        inputWithHelp("tumortypes from the training set are selected by default")
    )
  )
}

mlUploadInputModeUI_plot <- function(id){
  ns <- shiny::NS(id)
  brushPlotUI(ns("barplot"))
}

mlUploadInputMode <- function(input, output, session, FileInfo, errorId, Annotation){
  ns <- session$ns

  colname <- getOption("xiff.column")
  colname <- rlang::sym(colname)

  Model <- shiny::reactive({
    info <- FileInfo()
    shiny::req(!is.null(info) && info[["isRds"]])
    loadMachineLearningModel(info$datapath, errorId, session)
  })

  shiny::observeEvent(
    eventExpr = Model(),
    handlerExpr = {
      m <- Model()
      shiny::req(m)

      cl <- m$trainingSet
      shiny::req(length(cl) > 0)

      tt <- Annotation() %>%
        dplyr::filter(!!colname %in% cl) %>%
        dplyr::pull(tumortype)

      shiny::updateSelectInput(
        session = session,
        inputId = "tumortype",
        selected = tt
      )
    },
    priority = 100
  )

  DB_Data <- shiny::reactive({
    m <- Model()
    shiny::req(m)

    sql <- paste0("SELECT ", colname, ", ensg, log2tpm AS score FROM cellline.processedrnaseqview ",
                  "WHERE ", getSQL_filter("ensg", m$bestFeatures))
    getPostgresql(sql)
  })

  Data <- shiny::reactive({
    d <- DB_Data()
    m <- Model()
    shiny::req(d, m)

    tumortypes <- input$tumortype
    if (length(tumortypes) == 0) return()

    items <- Annotation() %>%
      dplyr::filter(tumortype %in% tumortypes) %>%
      dplyr::pull(!!colname)

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
      items <- setdiff(items, m$trainingSet)
    }

    df <- d %>% dplyr::filter(!!colname %in% items)

    shiny::validate(shiny::need(nrow(df) > 0, "no data available"))

    df <- df %>% tidyr::pivot_wider(names_from = ensg, values_from = score)

    assignment <- predictFromModel(m, df %>% dplyr::select(- !!colname), errorId, session)
    if (is.null(assignment)) return()

    cl <- m$classLabel
    cl <- c(cl$class1_name, cl$class2_name)

    df <- tibble::tibble(
      !!colname := df[[colname]],
      class = assignment
    ) %>%
      dplyr::mutate(
        class = ifelse(class == "class1", cl[1], cl[2]),
        class = factor(class, levels = cl)
      )

    if (useFacets){
      df <- df %>% dplyr::mutate(
        type = factor(ifelse(
          test = !!colname %in% m$trainingSet,
          yes = "training",
          no = "unseen"
        ))
      )
    }

    df
  })

  UploadPlot <- shiny::reactive({
    df <- Data()
    shiny::req(df)

    p <- generateScoreBarPlot(df %>% dplyr::rename(x_score = class), "class") +
      ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 5))

    if ("type" %in% names(df)){
      p <- p + ggplot2::facet_wrap(~ type, scales = "free_y")
    }

    p
  })

  UploadPlotCheck <- shiny::reactive({
    !is.null(Data()) && !is.null(input$tumortype)
  })

  ML_items <- shiny::callModule(
    module = brushPlot,
    id = "barplot",
    plotExpr = UploadPlot,
    checkExpr = UploadPlotCheck,
    textCallback = function(n, rx, ry){
      paste(get_label(n), "selected")
    },
    message = "Retrieving data",
    value = 0.3
  )

  ML_items
}
