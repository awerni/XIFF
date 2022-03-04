#' @rdname tabLayout
#' @export
tabLayoutUI_main <- function(id, useMiddlePlot = TRUE){
  ns <- NS(id)

  width <- if (useMiddlePlot) 4 else 6

  div(
    fluidRow(
      column(
        width = width,
        plotWrapperUI(ns("plot_left"))
      ),
      `if`(
        useMiddlePlot,
        column(
          width = width,
          plotWrapperUI(ns("plot_middle"))
        )
      ),
      column(
        width = width,
        plotWrapperUI(ns("plot_right"))
      )
    ),
    fluidRow_12(containerDT(ns("table")))
  )
}

#' @rdname tabLayout
#' @export
tabLayoutUI_sidebar <- function(id, defaults = list(), input = list(), additionalChoices = c(), hidden = c(), useMiddlePlot = TRUE){
  ns <- NS(id)

  choices <- c(
    "ROC curve" = "roc",
    "point plot" = "point",
    "violin plot" = "violin",
    "box plot" = "box",
    "data coverage" = "coverage",
    additionalChoices
  )

  if (length(hidden) > 0){
    choices <- Filter(
      f = function(x) !x %in% hidden,
      x = choices
    )
  }

  leftId <- ns("type_left")
  leftSelected <- (isolate(input[[leftId]]) %||% defaults$left) %||% choices[[1]]

  rightId <- ns("type_right")
  rightSelected <- (isolate(input[[rightId]]) %||% defaults$right) %||% choices[[3]]

  middlePlotInput <- if (useMiddlePlot){
    middleId <- ns("type_middle")
    middleSelected <- (isolate(input[[middleId]]) %||% defaults$middle) %||% choices[[2]]

    selectInput(
      inputId = middleId,
      label = "Middle plot",
      choices = choices,
      selected = middleSelected
    )
  }

  div(
    selectInput(
      inputId = leftId,
      label = "Left plot",
      choices = choices,
      selected = leftSelected
    ),
    middlePlotInput,
    selectInput(
      inputId = rightId,
      label = "Right plot",
      choices = choices,
      selected = rightSelected
    )
  )
}


#' Tab module layout
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#' @param plotFun 
#' @param TableData 
#' @param jsRowCallback 
#'
#' @rdname tabLayout
#' @export
#'
tabLayout <- function(input, output, session, plotFun, TableData,
                      jsRowCallback = htmlwidgets::JS("preventLinkSelections")){
  rowCallback <- if (is.reactive(jsRowCallback)){
    jsRowCallback
  } else {
    reactive({ jsRowCallback })
  }

  varDict <- list()

  LeftPlotType <- reactive({ input$type_left })
  LeftPlotExpr <- reactive({ plotFun(LeftPlotType()) })
  callModule(
    module = plotWrapper,
    id = "plot_left",
    PlotExpr = LeftPlotExpr,
    PlotType = LeftPlotType,
    varDict = varDict
  )

  MiddlePlotType <- reactive({ input$type_middle })
  MiddlePlotExpr <- reactive({ plotFun(MiddlePlotType()) })
  callModule(
    module = plotWrapper,
    id = "plot_middle",
    PlotExpr = MiddlePlotExpr,
    PlotType = MiddlePlotType,
    varDict = varDict
  )

  RightPlotType <- reactive({ input$type_right })
  RightPlotExpr <- reactive({ plotFun(RightPlotType()) })
  callModule(
    module = plotWrapper,
    id = "plot_right",
    PlotExpr = RightPlotExpr,
    PlotType = RightPlotType,
    varDict = varDict
  )

  output$table <- DT::renderDataTable({
    df <- TableData()
    rc <- rowCallback()

    DT::datatable(
      data = df,
      options = list(
        pageLength = 10,
        search = list(regex = TRUE),
        createdRow = rc
      ),
      rownames = FALSE,
      filter = "top",
      escape = FALSE,
      selection = list(
        mode = "single",
        selected = 1,
        target = "row"
      )
    )
  })

  SelectedRow <- reactive({
    input$table_rows_selected
  })

  AllRows <- reactive({
    input$table_rows_all
  })

  list(
    SelectedRow = SelectedRow,
    AllRows = AllRows
  )
}
