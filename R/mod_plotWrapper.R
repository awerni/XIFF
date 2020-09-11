#' @export
plotWrapperUI <- function(id, width = "100%", height = "400px", center = FALSE){
  ns <- shiny::NS(id)
  width <- htmltools::validateCssUnit(width)
  height <- htmltools::validateCssUnit(height)

  shiny::div(
    class = "downloadable-plot",
    shiny::uiOutput(
      outputId = ns("container"),
      class = "shiny-report-size",
      style = sprintf("width: %s; height: %s;%s", width, height, `if`(center, " margin: 0 auto;", ""))
    ),
    shiny::actionButton(
      inputId = ns("modalButton"),
      class = "download-btn",
      label = NULL,
      icon = shiny::icon("download")
    )
  )
}

#' @export
plotWrapper <- function(input, output, session, PlotExpr, PlotType = FALSE,
                        filename = "image", varDict = list(class = "Class"), allowedTooltipTypes = c("point", "comparison", "scatterplot"), ...){
  ns <- session$ns
  sizeId <- shiny::reactiveVal()

  output$plot_normal <- shiny::renderPlot({
    req(!PlotType() %in% allowedTooltipTypes, cancelOutput = TRUE)
    PlotExpr()
  }, ...)

  shouldInitTooltips <- TRUE
  normalOnly <- !shiny::is.reactive(PlotType)
  if (normalOnly){
    defaultType <- if (PlotType){
      "point"
    } else {
      shouldInitTooltips <- FALSE
      "other"
    }
    PlotType <- shiny::reactive({ defaultType })
  }

  if (shouldInitTooltips){
    TooltipPlotExpr <- shiny::reactive({
      shiny::req(PlotType() %in% allowedTooltipTypes, cancelOutput = TRUE)
      PlotExpr()
    })

    shiny::callModule(
      module = tooltipPlot,
      id = "plot_tooltip",
      plotExpr = TooltipPlotExpr,
      varDict = varDict
    )
  }

  output$container <- shiny::renderUI({
    hid <- paste0("output_", ns("container"), "_height")
    height <- shiny::isolate(session$clientData[[hid]]) # will be available when renderUI is called (i.e. output is visible)
    type <- PlotType()
    shiny::validate(shiny::need(type, "wait"))

    plotContainer <- if (type %in% allowedTooltipTypes){
      id <- ns("plot_tooltip")
      sizeId(paste0(id, "-plot")) # tooltip module ns
      tooltipPlotUI(id, width = "100%", height = height)
    } else {
      id <- ns("plot_normal")
      sizeId(id)
      plotOutput(id, width = "100%", height = height)
    }
  })

  # Download ------------------------------------------------------------------
  shiny::observeEvent(
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

      shiny::showModal(shiny::modalDialog(
        title = "Download plot",
        shiny::textInput(
          inputId = ns("filename"),
          label = "Filename",
          value = filename
        ),
        shiny::radioButtons(
          inputId = ns("format"),
          label = "Plot format",
          choices = c("pdf", "png"),
          selected = "pdf"
        ),
        shiny::numericInput(
          inputId = ns("width"),
          label = "Plot width [in]",
          value = downloadWidth
        ),
        shiny::numericInput(
          inputId = ns("height"),
          label = "Plot height [in]",
          value = signif(downloadWidth * ratio, 2)
        ),
        size = "s",
        footer = shiny::tagList(
          shiny::tags$button(
            type = "button",
            class = "btn",
            `data-dismiss` = "modal",
            "Cancel"
          ),
          shiny::downloadButton(ns("download"))
        )
      ))
    }
  )

  output$download <- shiny::downloadHandler(
    filename = function() {
      paste(input$filename, input$format, sep = ".")
    },
    content = function(file) {
      ggplot2::ggsave(
        filename = file,
        plot = PlotExpr(),
        width = input$width,
        height = input$height,
        units = "in"
      )

      shiny::removeModal()
    }
  )
}
