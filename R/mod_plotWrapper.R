#' Plot Wrapper
#'
#' @param input shiny input object
#' @param output shiny output object 
#' @param session shiny session object
#' @param PlotExpr expression to create the plot
#' @param PlotType 
#' @param filename 
#' @param varDict 
#' @param allowedTooltipTypes 
#' @param tooltipCallback 
#' @param ... 
#'
#' @rdname plotWrapper
#' @export
#'
plotWrapperUI <- function(id, width = "100%", height = "400px", center = FALSE){
  ns <- NS(id)
  width <- htmltools::validateCssUnit(width)
  height <- htmltools::validateCssUnit(height)

  div(
    class = "downloadable-plot",
    uiOutput(
      outputId = ns("container"),
      class = "shiny-report-size",
      style = sprintf("width: %s; height: %s;%s", width, height, `if`(center, " margin: 0 auto;", ""))
    ),
    actionButton(
      inputId = ns("modalButton"),
      class = "download-btn",
      label = NULL,
      icon = icon("download")
    )
  )
}


#' @rdname plotWrapper
#' @export
plotWrapper <- function(input, output, session, PlotExpr, PlotType = FALSE,
                        filename = "image", varDict = list(class = "Class"),
                        allowedTooltipTypes = c("point", "comparison", "scatterplot"),
                        tooltipCallback = getOption("xiff.tooltipCallbackFun"), ...){
  ns <- session$ns
  sizeId <- reactiveVal()
  
  PlotExprWrapper <- reactive({
    jsId <- paste0("#", ns("container"))
    shinyjs::addClass(
      selector = paste0(jsId, " .shiny-plot-output, ", jsId, " .tooltip-plot-output"),
      class = "recalculating"
    )
    PlotExpr()
  })

  output$plot_normal <- renderPlot({
    req(!PlotType() %in% allowedTooltipTypes, cancelOutput = TRUE)
    PlotExprWrapper()
  }, ...)

  shouldInitTooltips <- TRUE
  normalOnly <- !is.reactive(PlotType)
  if (normalOnly){
    defaultType <- if (PlotType){
      "point"
    } else {
      shouldInitTooltips <- FALSE
      "other"
    }
    PlotType <- reactive({ defaultType })
  }

  if (shouldInitTooltips){
    TooltipPlotExpr <- reactive({
      req(PlotType() %in% allowedTooltipTypes, cancelOutput = TRUE)
      PlotExprWrapper()
    })

    callModule(
      module = tooltipPlot,
      id = "plot_tooltip",
      plotExpr = TooltipPlotExpr,
      varDict = varDict,
      callback = tooltipCallback
    )
  }

  output$container <- renderUI({
    hid <- paste0("output_", ns("container"), "_height")
    height <- isolate(session$clientData[[hid]]) # will be available when renderUI is called (i.e. output is visible)
    type <- PlotType()
    validate(need(type, "wait"))

    plotContainer <- if (type %in% allowedTooltipTypes){
      id <- ns("plot_tooltip")
      sizeId(paste0(id, "-plot")) # tooltip module ns
      tooltipPlotUI(id, width = "100%", height = height)
    } else {
      id <- ns("plot_normal")
      sizeId(id)
      plotOutput(id, width = "100%", height = height)
    }

    list(
      plotContainer,
      div(class = "loader")
    )
  })

  # Download ------------------------------------------------------------------
  observeEvent(
    eventExpr = input$modalButton,
    handlerExpr = {
      sid <- sizeId()
      if (is.null(sid)){
        wp <- 400
        hp <- 400
      } else {
        id <- paste0("output_", sid)
        wp <- session$clientData[[paste0(id, "_width")]] # size in pixel
        hp <- session$clientData[[paste0(id, "_height")]]
      }
      downloadWidth <- ceiling(wp / 72) #inches
      ratio <- hp/wp # plot ratio

      showModal(modalDialog(
        title = "Download plot",
        textInput(
          inputId = ns("filename"),
          label = "Filename",
          value = filename
        ),
        radioButtons(
          inputId = ns("format"),
          label = "Plot format",
          choices = c("pdf", "png"),
          selected = "pdf"
        ),
        numericInput(
          inputId = ns("width"),
          label = "Plot width [in]",
          value = downloadWidth
        ),
        numericInput(
          inputId = ns("height"),
          label = "Plot height [in]",
          value = signif(downloadWidth * ratio, 2)
        ),
        size = "s",
        footer = tagList(
          tags$button(
            type = "button",
            class = "btn",
            `data-dismiss` = "modal",
            "Cancel"
          ),
          downloadButton(ns("download"))
        )
      ))
    }
  )

  output$download <- downloadHandler(
    filename = function() {
      paste(input$filename, input$format, sep = ".")
    },
    content = function(file) {
      args <- list(
        filename = file,
        plot = PlotExpr(),
        width = input$width,
        height = input$height,
        units = "in"
      )

      if (input$format == "png"){
        args$type <- "cairo-png"
      }

      do.call(ggsave, args)
      removeModal()
    }
  )
}
