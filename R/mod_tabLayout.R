#' @export
tabLayoutUI_main <- function(id, useMiddlePlot = TRUE){
  ns <- shiny::NS(id)

  width <- if (useMiddlePlot) 4 else 6

  shiny::div(
    shiny::fluidRow(
      shiny::column(
        width = width,
        plotWrapperUI(ns("plot_left"))
      ),
      `if`(
        useMiddlePlot,
        shiny::column(
          width = width,
          plotWrapperUI(ns("plot_middle"))
        )
      ),
      shiny::column(
        width = width,
        plotWrapperUI(ns("plot_right"))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::div(
          style = "font-size:80%",
          DT::dataTableOutput(ns("table"))
        )
      )
    )
  )
}

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
  leftSelected <- (shiny::isolate(input[[leftId]]) %||% defaults$left) %||% choices[[1]]

  rightId <- ns("type_right")
  rightSelected <- (shiny::isolate(input[[rightId]]) %||% defaults$right) %||% choices[[3]]

  middlePlotInput <- if (useMiddlePlot){
    middleId <- ns("type_middle")
    middleSelected <- (shiny::isolate(input[[middleId]]) %||% defaults$middle) %||% choices[[2]]

    shiny::selectInput(
      inputId = middleId,
      label = "Middle plot",
      choices = choices,
      selected = middleSelected
    )
  }

  shiny::div(
    shiny::selectInput(
      inputId = leftId,
      label = "Left plot",
      choices = choices,
      selected = leftSelected
    ),
    middlePlotInput,
    shiny::selectInput(
      inputId = rightId,
      label = "Right plot",
      choices = choices,
      selected = rightSelected
    )
  )
}

#' @export
tabLayout <- function(input, output, session, plotFun, TableData,
                      jsRowCallback = htmlwidgets::JS("preventLinkSelections")){
  rowCallback <- if (shiny::is.reactive(jsRowCallback)){
    jsRowCallback
  } else {
    shiny::reactive({ jsRowCallback })
  }

  varDict <- list()

  LeftPlotType <- shiny::reactive({ input$type_left })
  LeftPlotExpr <- shiny::reactive({ plotFun(LeftPlotType()) })
  shiny::callModule(
    module = plotWrapper,
    id = "plot_left",
    PlotExpr = LeftPlotExpr,
    PlotType = LeftPlotType,
    varDict = varDict
  )

  MiddlePlotType <- shiny::reactive({ input$type_middle })
  MiddlePlotExpr <- shiny::reactive({ plotFun(MiddlePlotType()) })
  shiny::callModule(
    module = plotWrapper,
    id = "plot_middle",
    PlotExpr = MiddlePlotExpr,
    PlotType = MiddlePlotType,
    varDict = varDict
  )

  RightPlotType <- shiny::reactive({ input$type_right })
  RightPlotExpr <- shiny::reactive({ plotFun(RightPlotType()) })
  shiny::callModule(
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

  SelectedRow <- shiny::reactive({
    input$table_rows_selected
  })

  AllRows <- shiny::reactive({
    input$table_rows_all
  })

  list(
    SelectedRow = SelectedRow,
    AllRows = AllRows
  )
}
