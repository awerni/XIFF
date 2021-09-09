
#' Returns the list of supported dimension reduction methods.
#'
#' @return character vector with supported methods.
#' @export
#'
dimRedAvailableMethods <- function() {
  
  hasPhate <- requireNamespace("phateR", quietly = TRUE)
  
  methods <- c(
    "t-SNE" = "tsne",
    "principal component analysis" = "pca",
    "UMAP" = "umap"
  )
  
  if(hasPhate) {
    methods <- c(methods, c("PHATE" = "phate"))
  }
  methods
}


#' @export
dimensionReductionUI <- function(id, width = "100%", height = "800px"){
  ns <- NS(id)
  plotWrapperUI(
    id = ns("plot"),
    width = width,
    height = height
  )
}

#' @export
dimensionReduction <- function(input,
                               output,
                               session,
                               InputData,
                               AnalysisParams,
                               ClusterMethod,
                               PlotParams,
                               classLabel,
                               funTSNE = calcTSNE,
                               funPCA = calcPCA_expression,
                               funUMAP = calcUMAP,
                               funPHATE = calcPHATE) {
  
  colname <- getOption("xiff.column")

  Results <- reactive({
    inputData <- InputData()
    FutureManager::fmValidate(inputData)

    params <- AnalysisParams()
    if (length(dropNulls(params)) < length(params)) return()

    method <- params$method

    dataSource <- params$dataSource
    n <- params$n
    unit <- params$unit
    clusterMethod <- params$clusterMethod

    res <- inputData$df
    annotationFocus <- inputData$annotationFocus
    
    switch(
      EXPR = method,
      tsne = {
        progressText <- "plot t-SNE"
        d <- funTSNE(res, dataSource, n, unit, p = session)
      },
      pca = {
        progressText <- "plot PCA"
        d <- funPCA(res, dataSource, n, p = session)
      },
      umap = {
        progressText <- "plot umap"
        d <- funUMAP(res, dataSource, n, unit, p = session)
      },
      phate = {
        progressText <- "plot PHATE"
        d <- funPHATE(res, dataSource, n, unit, p = session)
      }
    )
    if (is.null(d)) return()
    d$progressText <- progressText

    cs <- inputData$cs
    assignment <- stackClasses(cs)
    d$data <- d$data %>% select(-class) %>% left_join(assignment, by = colname)

    #d$data <- replaceClassLabels(d$data, cl)

    return(list(d = d, annotationFocus = annotationFocus))
  })

  ClusterResults <- reactive({
    res <- Results()
    req(res)

    cm <- ClusterMethod()
    res$d$data <- addClustering(res$d$data, cm, p = session)
    res
  })

  colorTooltip <- FALSE
  callback <- function(x){
    if (colorTooltip){
      # Fix #BIARD-291 - ggtips does not handle 3.5 and 4.1 does
      # not support NA in factors.
      colorLabel <- PlotParams()$colorDimRed
      paste0(x[[colname]], "\n", x[[colorLabel]])
    } else {
      tooltipCallbackFun(x)
    }
  }

  PlotExpr <- reactive({
    params <- PlotParams()
    if (length(dropNulls(params)) < length(params)) return()

    res <- ClusterResults()
    if (is.null(res)) return()

    cl <- reactiveValuesToList(classLabel)
    labels <- c(cl$class1_name, cl$class2_name)

    res$d$data$class <- factor(
      x = ifelse(
        test = res$d$data$class == "class1",
        yes = labels[1],
        no = labels[2]
      ),
      levels = labels
    )
    
    p <- plotDimRed(res$d, params, res$annotationFocus, p = session)
    validate(need(p$plot, "nothing to show yet..."))
    colorTooltip <<- p$colorTooltip
    p$plot
  })

  callModule(
    module = plotWrapper,
    id = "plot",
    PlotExpr = PlotExpr,
    varDict = list(),
    PlotType = TRUE,
    tooltipCallback = callback
  )

  ClusterResults
}

plotDimRed <- function(d, params, annotationFocus, p = FALSE) {
  if (is.null(d)) return()

  il <- params$labelDimRed
  ic <- params$colorDimRed
  fs <- params$fontsizeOverview

  if (is.null(il) || is.null(ic) || is.null(fs)) return()

  sa <- annotationFocus
  showLabels <- (il != "none")
  changeColor <- !(ic %in% c("class selection"))

  if (showLabels | changeColor) {
    newCols <- intersect(unique(c(il, ic)), colnames(sa))
    if (length(newCols) >= 1) {
      colname <- getOption("xiff.column")
      d$data <- d$data %>% left_join(sa[, c(colname, newCols)], by = colname)
    }
  }
  d$data$plotlabel <- NA
  if (showLabels) d$data <- d$data %>% mutate(plotlabel = d$data[, il])

  colorTooltip <- FALSE
  colorCol <- "class"
  if (changeColor) {
    d$data <- d$data %>% mutate(class = d$data[, ic])
    colorCol <- ic

    colorTooltip <- n_distinct(d$data[["class"]]) > 5
  }

  p <- generateDimRedPlot(
    data = d,
    progressText = d$progressText,
    showLabels = showLabels,
    colorCol = colorCol,
    fontSize = fs,
    p = p
  )

  list(
    plot = p$pl,
    colorTooltip = colorTooltip
  )
}
