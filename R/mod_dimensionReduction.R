#' @export
dimensionReductionUI <- function(id, width = "100%", height = "800px"){
  ns <- shiny::NS(id)
  plotWrapperUI(
    id = ns("plot"),
    width = width,
    height = height
  )
}

#' @export
dimensionReduction <- function(input, output, session,
                               InputData, AnalysisParams, PlotParams,
                               funTSNE = calcTSNE, funPCA = calcPCA_expression, funUMAP = calcUMAP){
  Results <- shiny::reactive({
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
        d <- funTSNE(res, dataSource, n, unit)
      },
      pca = {
        progressText <- "plot PCA"
        d <- funPCA(res, dataSource, n)
      },
      umap = {
        progressText <- "plot umap"
        d <- funUMAP(res, dataSource, n, unit)
      }
    )
    if (is.null(d)) return()
    d$progressText <- progressText

    cs <- inputData$cs
    cl <- inputData$cl
    assignment <- stackClasses(cs, cl)
    d$data <- d$data %>% select(-class) %>% dplyr::left_join(assignment, by = getOption("xiff.column"))

    #d$data <- replaceClassLabels(d$data, cl)
    d$data <- addClustering(d$data, clusterMethod)

    return(list(d = d, annotationFocus = annotationFocus))
  })

  PlotExpr <- shiny::reactive({
    params <- PlotParams()
    if (length(dropNulls(params)) < length(params)) return()

    res <- Results()
    if (is.null(res)) return()

    p <- plotDimRed(res$d, params, res$annotationFocus)
    shiny::validate(shiny::need(p, "nothing to show yet..."))
    p
  })
  shiny::callModule(
    module = plotWrapper,
    id = "plot",
    PlotExpr = PlotExpr,
    varDict = list(),
    PlotType = TRUE
  )

  Results
}

plotDimRed <- function(d, params, annotationFocus) {
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
      d$data <- d$data %>% dplyr::left_join(sa[, c(colname, newCols)], by = colname)
    }
  }
  d$data$plotlabel <- NA
  if (showLabels) d$data <- d$data %>% dplyr::mutate(plotlabel = d$data[, il])

  colorCol <- "class"
  if (changeColor) {
    d$data <- d$data %>% dplyr::mutate(class = d$data[, ic])
    colorCol <- ic
  }

  p <- generateDimRedPlot(d, d$progressText, showLabels, colorCol, fs)
  p$pl
}
