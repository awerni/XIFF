#' @export
tooltipPlotUI <- function(id, width = "100%", height = "400px"){
  width <- htmltools::validateCssUnit(width)
  height <- htmltools::validateCssUnit(height)

  ns <- shiny::NS(id)
  shiny::uiOutput(
    outputId = ns("plot"),
    class = "shiny-report-size tooltip-plot-output",
    style = sprintf("width: %s; height: %s;", width, height)
  )
}

#' @export
tooltipPlot <- function(input, output, session, plotExpr, varDict, callback = callbackFun, ...){
  id <- paste0("output_", session$ns("plot"))

  Width <- reactive({
    w <- session$clientData[[paste0(id, "_width")]]

    if (is.null(w)){
      NA
    } else {
      w / 72
    }
  })

  Height <- reactive({
    h <- session$clientData[[paste0(id, "_height")]]

    if (is.null(h)){
      NA
    } else {
      h / 72
    }
  })

  output$plot <- ggtips::renderWithTooltips(
    plot = {
      p <- plotExpr()
      req(Width(), p) # re-render on resize
      p
    },
    varDict = varDict,
    callback = callback,
    width = shiny::isolate(Width()),
    height = shiny::isolate(Height()),
    ...
  )
}

callbackFun <- function(x){
  x[[getOption("xiff.column")]]
}
