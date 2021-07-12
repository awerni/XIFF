#' @export
tooltipPlotUI <- function(id, width = "100%", height = "400px"){
  width <- htmltools::validateCssUnit(width)
  height <- htmltools::validateCssUnit(height)

  ns <- NS(id)
  uiOutput(
    outputId = ns("plot"),
    class = "shiny-report-size tooltip-plot-output",
    style = sprintf("width: %s; height: %s;", width, height)
  )
}

#' @export
tooltipPlot <- function(input, output, session, plotExpr, varDict,
                        callback = getOption("xiff.tooltipCallbackFun"), ...){
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
  
  SvgData <- reactive({
    p <- plotExpr()
    req(Width(), p) # re-render on resize
    res <- ggtips::getSvgAndTooltipdata(
      plot = p, 
      varDict = varDict,
      callback = callback,
      width = isolate(Width()),
      height = isolate(Height()),
      ...
    )
    res$svg <- removeSvgDimensions(res$svg)
    res
  })

  output$plot <- renderUI({
    d <- SvgData()
    req(d)
    
    ggtips::htmlWithGivenTooltips(
      svg = d$svg,
      data = d$data,
      height = isolate(Height()),
      width = isolate(Width())
    )
  })
}

tooltipCallbackFun <- function(x){
  x[[getOption("xiff.column")]]
}

removeSvgDimensions <- function(svg){
  x <- xml2::read_xml(svg)
  xml2::xml_set_attr(x, "width", NULL)
  xml2::xml_set_attr(x, "height", NULL)
  as.character(x)
}
