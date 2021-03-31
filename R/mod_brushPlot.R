#' @export
brushPlotUI <- function(id, ..., direction = "x", height = "600px"){
  ns <- NS(id)

  tagList(
    fluidRow(
      class = "brush-container",
      column(
        width = 12,
        fluidRow(
          class = "brush-info",
          textOutput(ns("selectionStat"), inline = TRUE),
          uiOutput(ns("dropdown"), inline = TRUE, class = "dropdown-container")
        ),
        plotOutput(
          outputId = ns("plot"),
          dblclick = ns("plot_info"), # event to capture the plot info
          brush = brushOpts(
            id = ns("plot_brush"),
            resetOnNew = TRUE,
            direction = direction
          ),
          height = height
        ),
        div(class = "loader")
      )
    ),
    ...
  )
}

defaultTextCallback <- function(n, rx, ry){
  xText <- if (all(is.na(rx))){
    ""
  } else {
    sprintf("X range: (%s, %s);", signif(rx[1], 4), signif(rx[2], 4))
  }

  yText <- if (all(is.na(ry))){
    ""
  } else {
    sprintf("Y range: (%s, %s);", signif(ry[1], 4), signif(ry[2], 4))
  }

  glue::glue("{n} {getOption('xiff.label')}s selected in {xText} {yText}")
}

getPanelPositions <- function(p, fVar, res = 72){
  b <- ggplot_build(p)
  fVar <- rlang::sym(fVar)
  layout <- b$layout$layout %>%
    mutate(
      PANEL = as.numeric(as.character(PANEL)),
      name = as.character(!!fVar),
      name = ifelse(is.na(name), "NA", name)
    )

  g <- ggplot_gtable(b)
  ranges <- shiny:::find_panel_ranges(g, res)

  lapply(
    X = structure(
      layout$PANEL,
      names = layout$name
    ),
    FUN = function(i){
      r <- ranges[[i]]
      list(
        x = (r$left + r$right) / 2,
        y = (r$top + r$bottom) / 2
      )
    }
  )
}

#' @export
brushPlot <- function(input, output, session, plotExpr, checkExpr,
                      textCallback = defaultTextCallback,
                      defaultCutoff_x = NULL, step_x = 0.1, dx = 0.5, sorted_x = "no",
                      defaultCutoff_y = -1, step_y = 0.1, dy = 0, sorted_y = "desc",
                      availableChoices = c("cutoff", "number", "percentile"), test = FALSE,
                      ...){
  ns <- session$ns
  PlotData <- reactiveVal()
  progressOpts <- list(...)

  colname <- getOption("xiff.column")
  colname <- rlang::sym(colname)

  output$plot <- renderPlot({
    if (!checkExpr()){ # check expression is used to prevent unwanted re-rendering
      PlotData(NULL)
      return()
    }

    shinyjs::addClass(id = "plot", class = "recalculating") # shiny is not adding this class on plot init

    p <- if (length(progressOpts) > 0){
      withProgress(isolate(plotExpr()), ...)
    } else {
      isolate(plotExpr())
    }

    if (is.null(p)){
      PlotData(NULL)
    } else {
      fVar <- p$facet$vars()
      facetInfo <- if (length(fVar) > 0){
        list(
          facetVar = fVar[1],
          free = p$facet$params$free,
          panelPositions = getPanelPositions(p, fVar[1])
        )
      }

      xTrans <- p$scales$get_scales("x")$trans$name
      if (is.null(xTrans)){
        xTrans <- "identity"
      }
      yTrans <- p$scales$get_scales("y")$trans$name
      if (is.null(yTrans)){
        yTrans <- "identity"
      }

      PlotData(list(
        data = p$data,
        xVar = rlang::quo_name(p$mapping$x),
        yVar = rlang::quo_name(p$mapping$y),
        facetInfo = facetInfo,
        xTrans = xTrans,
        yTrans = yTrans
      ))
    }

    p
  })

  output$dropdown <- renderUI({
    pd <- PlotData()
    if (is.null(pd) || pd$xVar == "NULL" || pd$yVar == "NULL"){
      return()
    }

    shinyWidgets::dropdownButton(
      inputId = ns("brush_options"),
      content = getOptionsContent(
        ns = ns,
        facetInfo = pd$facetInfo,
        defaultCutoff_x = defaultCutoff_x,
        defaultCutoff_y = defaultCutoff_y,
        availableChoices = availableChoices
      ),
      circle = FALSE,
      status = "default",
      icon = icon("gear"),
      label = NULL,
      width = "222px",
      inline = TRUE,
      size = "sm"
    )
  })

  Selected_items <- reactive({
    d <- PlotData()
    pb <- input$plot_brush
    if (is.null(d) || is.null(pb)) return()

    pb <- reverseTrans(pb, d)
    ti <- brushedPoints(d$data, pb, allRows = FALSE) %>%
      .[[colname]] %>%
      as.character() %>%
      sort()
    emptyRange <- c(NA, NA)

    if (length(ti) > 0) {
      df <- d$data %>% filter(!!colname %in% ti)

      x <- df[[d$xVar]]
      rx <- if (is.numeric(x)){
        range(x, na.rm = TRUE)
      } else {
        emptyRange
      }

      y <- df[[d$yVar]]
      ry <- if (is.numeric(y)){
        range(y, na.rm = TRUE)
      } else {
        emptyRange
      }
    } else {
      rx <- emptyRange
      ry <- emptyRange
    }

    res <- list(
      source = ns("plot_brush"),
      range_x = rx,
      range_y = ry
    )

    res[[colname]] <- ti
    res
  })

  output$selectionStat <- renderText({
    s <- Selected_items()
    n <- length(s[[colname]])
    if (n == 0) return("-")

    textCallback(
      n = n,
      rx = s$range_x,
      ry = s$range_y
    )
  })

  if (!is.null(defaultCutoff_x)){
    registerAxisObserver(
      input = input, output = output, ns = ns,
      axis = "x",
      defaultCutoff = defaultCutoff_x,
      step = step_x
    )
  }

  if (!is.null(defaultCutoff_y)){
    registerAxisObserver(
      input = input, output = output, ns = ns,
      axis = "y",
      defaultCutoff = defaultCutoff_y,
      step = step_y
    )
  }

  observeEvent(
    eventExpr = input$brush_apply,
    handlerExpr = {
      method_x <- input$score_method_x
      method_y <- input$score_method_y

      anyMethodAvailable <- !is.null(method_x) || !is.null(method_y)
      plotDetails <- input$plot_info
      pd <- PlotData()

      if (is.null(pd) || is.null(plotDetails) || !anyMethodAvailable){
        return()
      }

      x_options <- list(
        method = method_x,
        cutoff_action = input$cutoff_action_x,
        cutoff = input$cutoff_x,
        number_action = input$number_action_x,
        number = input$number_x,
        percentile_action = input$percentile_action_x,
        percentile = input$percentile_x,
        trans = pd$xTrans
      )

      y_options <- list(
        method = method_y,
        cutoff_action = input$cutoff_action_y,
        cutoff = input$cutoff_y,
        number_action = input$number_action_y,
        number = input$number_y,
        percentile_action = input$percentile_action_y,
        percentile = input$percentile_y,
        trans = pd$yTrans
      )

      if (is.null(method_x) && sorted_y != "no" && method_y != "cutoff"){
        if (sorted_y == "desc"){
          y_options$number_action <- oppositeAction(y_options$number_action)
          y_options$percentile_action <- oppositeAction(y_options$percentile_action)
          dx <- 0
        }

        transBuffer <- x_options$trans
        x_options$trans <- y_options$trans
        y_options$trans <- transBuffer

        buffer <- x_options
        x_options <- y_options
        y_options <- buffer
      }
      dr <- getDomainRange(
        df = pd$data,
        facetInfo = pd$facetInfo,
        panel = input$panel,
        x_options = x_options,
        y_options = y_options,
        xVar = pd$xVar,
        yVar = pd$yVar
      )

      if (is.null(dr)){
        session$resetBrush(ns("plot_brush"))
        return()
      }

      # fix log infinite lower limits to avoid NaN conversion
      if (is.infinite(dr$xmin) && !is.null(plotDetails$log$x)){
        dr$xmin <- 0
      }
      if (is.infinite(dr$ymin) && !is.null(plotDetails$log$y)){
        dr$ymin <- 0
      }

      # scale domain (plot) to range (CSS)
      px <- shiny:::scaleCoords(
        x = c(dr$xmin - dx, dr$xmax + dx), # make sure brush will select edge cases
        y = c(dr$ymin - dy, dr$ymax + dy),
        scaleinfo = plotDetails
      )

      x <- tidyr::replace_na(px$x, -1) # negative if outside the domain
      y <- tidyr::replace_na(px$y, -1)

      cssRatio <- plotDetails$img_css_ratio
      shinyjs::runjs(sprintf(
        "setPlotBrush('%s',%f,%f,%f,%f);",
        ns("plot"), # plot output ID
        x[1] / cssRatio$x, # x start
        x[2] / cssRatio$x, # x end
        y[1] / cssRatio$y, # y start
        y[2] / cssRatio$y # y end
      ))
    }
  )

  if (test) exportTestValues(pd = PlotData())

  return(Selected_items)
}

oppositeAction <- function(x){
  if (is.null(x)) return()

  switch(
    EXPR = x,
    lowest = "highest",
    highest = "lowest",
    gt = "lt",
    lt = "gt"
  )
}

getOptionsContent <- function(ns, facetInfo, defaultCutoff_x, defaultCutoff_y, availableChoices){
  content <- list()

  if (length(facetInfo) > 1){
    panelPositions <- facetInfo$panelPositions
    facetChoices <- names(panelPositions)
    inputId <- ns("panel")
    facetSelector <- selectInput(
      inputId = inputId,
      label = "Select panel",
      choices = facetChoices,
      selected = facetChoices[[1]]
    )

    content <- append(
      content,
      list(
        h4("Panel"),
        div(
          class = "panel-selection",
          facetSelector,
          tags$script(
            type = "application/json",
            `data-for` = inputId,
            jsonlite::toJSON(panelPositions, auto_unbox = TRUE)
          )
        )
      )
    )
  }

  label <- paste0(getOption("xiff.label"), "s")

  choices <- c(
    "score cutoff" = "cutoff",
    "dummy" = "number",
    "percentile" = "percentile"
  )
  names(choices)[2] <- paste("number of", label)

  choices <- Filter(
    f = function(x) x %in% availableChoices,
    x = choices
  )

  if (!is.null(defaultCutoff_x)){
    xSelector <- selectInput(
      inputId = ns("score_method_x"),
      label = paste("Select", label, "using"),
      choices = choices,
      selected = choices[[1]]
    )

    content <- append(
      content,
      list(
        h4("X axis"),
        `if`(length(choices) > 1, xSelector, shinyjs::hidden(xSelector)),
        uiOutput(ns("score_options_x"))
      )
    )
  }

  if (!is.null(defaultCutoff_y)){
    ySelector <- selectInput(
      inputId = ns("score_method_y"),
      label = paste("Select", label, "using"),
      choices = choices,
      selected = choices[[1]]
    )

    content <- append(
      content,
      list(
        h4("Y axis"),
        `if`(length(choices) > 1, ySelector, shinyjs::hidden(ySelector)),
        uiOutput(ns("score_options_y"))
      )
    )
  }

  append(
    content,
    list(actionButton(
      inputId = ns("brush_button"),
      label = "Apply",
      class = "set-brush",
      style = "float:right;",
      `data-event-id` = ns("brush_apply"),
      `data-plot-id` = ns("plot"),
      `data-panel-id` = ns("panel")
    ))
  )
}

registerAxisObserver <- function(input, output, ns, axis, defaultCutoff, step){
  outputId <- paste0("score_options_", axis)
  scoreMethodId <- paste0("score_method_", axis)
  label <- paste0(getOption("xiff.label"), "s")

  getCutoffContent <- function(value){
    tagList(
      selectInput(
        inputId = ns(paste0("cutoff_action_", axis)),
        label = "Score is",
        choices = c(
          "less than" = "lt",
          "greater than" = "gt"
        ),
        selected = "lte"
      ),
      numericInput(
        inputId = ns(paste0("cutoff_", axis)),
        label = "Cutoff",
        value = value,
        step = step
      )
    )
  }

  numberContent <- tagList(
    numericInput(
      inputId = ns(paste0("number_", axis)),
      label = "Select",
      value = 10
    ),
    selectInput(
      inputId = ns(paste0("number_action_", axis)),
      label = paste(label, "with"),
      choices = c(
        "the lowest score values" = "lowest",
        "the highest score values" = "highest"
      ),
      selected = "lowest"
    )
  )

  percentileContent <- tagList(
    numericInput(
      inputId = ns(paste0("percentile_", axis)),
      label = "Select",
      value = 10
    ),
    selectInput(
      inputId = ns(paste0("percentile_action_", axis)),
      label = paste("% of", label, "with"),
      choices = c(
        "the lowest score values" = "lowest",
        "the highest score values" = "highest"
      ),
      selected = "lowest"
    )
  )

  output[[outputId]] <- renderUI({
    switch(
      EXPR = input[[scoreMethodId]],
      cutoff = {
        x <- if (is.function(defaultCutoff)) defaultCutoff() else defaultCutoff
        getCutoffContent(x)
      },
      number = numberContent,
      percentile = percentileContent
    )
  })
}

getDomainRange <- function(df, facetInfo, panel, x_options, y_options, xVar, yVar){
  if (!is.null(panel)){
    facetVar <- rlang::sym(facetInfo$facetVar)
    df <- if (panel == "NA"){
      df %>% filter(is.na(!!facetVar))
    } else {
      df %>% filter(!!facetVar == panel)
    }

    if (facetInfo$free$x){
      df[[xVar]] <- droplevels(df[[xVar]])
    }
    if (facetInfo$free$y){
      df[[yVar]] <- droplevels(df[[yVar]])
    }
  }

  df <- df %>%
    select_at(c(xVar = xVar, yVar = yVar)) %>%
    mutate(
      xVar = as.numeric(xVar),
      yVar = as.numeric(yVar)
    )

  is2d <- !is.null(x_options$method) && !is.null(y_options$method)

  xlim <- getAxisRange(
    values = df$xVar,
    method = x_options$method,
    cutoff_action = x_options$cutoff_action,
    cutoff = x_options$cutoff,
    number_action = x_options$number_action,
    number = x_options$number,
    percentile_action = x_options$percentile_action,
    percentile = x_options$percentile,
    trans = x_options$trans,
    is2d = is2d
  )

  ylim <- getAxisRange(
    values = df$yVar,
    method = y_options$method,
    cutoff_action = y_options$cutoff_action,
    cutoff = y_options$cutoff,
    number_action = y_options$number_action,
    number = y_options$number,
    percentile_action = y_options$percentile_action,
    percentile = y_options$percentile,
    trans = y_options$trans,
    is2d = is2d
  )

  filtered <- df %>% filter(xVar > xlim$min, xVar < xlim$max, yVar > ylim$min, yVar < ylim$max)

  if (nrow(filtered) == 0 && (xlim$shouldAdjust || ylim$shouldAdjust)){
    # 1d case; no items found (condition outside of the plot range)
    return()
  }

  # use filtered limits if method for the axis is not defined (1d case)
  if (xlim$shouldAdjust){
    xrf <- range(filtered$xVar, na.rm = TRUE)
    xmin <- xrf[1]
    xmax <- xrf[2]
  } else {
    xmin <- xlim$min
    xmax <- xlim$max
  }

  if (ylim$shouldAdjust){
    yrf <- range(filtered$yVar, na.rm = TRUE)
    ymin <- yrf[1]
    ymax <- yrf[2]
  } else {
    ymin <- ylim$min
    ymax <- ylim$max
  }

  list(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax
  )
}

getAxisRange <- function(values, method, cutoff_action, cutoff, number_action, number, percentile_action, percentile, trans, is2d){
  minVal <- -Inf
  maxVal <- Inf

  if (!is.null(method)){
    if (method == "cutoff"){
      if (startsWith(cutoff_action, "l")){
        maxVal <- cutoff # < cutoff
      } else {
        minVal <- cutoff # > cutoff
      }
    } else if (method %in% c("number", "percentile")){
      s <- sort(values)
      n <- length(s)

      if (method == "percentile"){
        number <- min(max(round(n * percentile/100), 1), n)
        action <- percentile_action
      } else {
        action <- number_action
      }

      if (n > number) {
        # put the boundary between 2 points: 1 inside and 1 outside the required range
        if (action == "lowest"){
          maxVal <- mean(tail(head(s, number + 1), 2))
        } else {
          minVal <- mean(head(tail(s, number + 1), 2))
        }
      }
    }
  }

  if (is2d && trans == "sqrt"){
    # Shiny doesn't translate sqrt scale at all when in 2d mode
    if (minVal >= 0) minVal <- sqrt(minVal)
    if (maxVal >= 0) maxVal <- sqrt(maxVal)
  }

  list(
    min = minVal,
    max = maxVal,
    shouldAdjust = is.null(method)
  )
}

reverseTrans <- function(brushInfo, plotInfo){
  if (is.null(brushInfo)) return()

  if (plotInfo$xTrans == "sqrt"){
    brushInfo$xmin <- brushInfo$xmin ^ 2
    brushInfo$xmax <- brushInfo$xmax ^ 2
  }

  if (plotInfo$yTrans == "sqrt"){
    brushInfo$ymin <- brushInfo$ymin ^ 2
    brushInfo$ymax <- brushInfo$ymax ^ 2
  }

  brushInfo
}
