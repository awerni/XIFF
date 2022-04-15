#' @export
#' @rdname geneExpressionDimRedTab
geneExpressionDimRedTabUI_main <- function(id){
  ns <- NS(id)
  
  div(
    br(),
    plotWrapperUI(ns("plot"), height = "800px"),
    uiOutput(ns("indicator"))
  )
}

#' @export
#' @rdname geneExpressionDimRedTab
geneExpressionDimRedTabUI_sidebar <- function(id){
  ns <- NS(id)
  
  cluster_methods <- c(
    "pam k = automatic", "pam k = 2", "pam k = 3", "pam k = 4", 
    "affinity propagation",
    "k-means k = automatic", "k-means k = 2", "k-means k = 3", "k-means k = 4"
  )
  
  div(
    radioButtons(
      inputId = ns("method"), 
      label = "method:",
      choices = XIFF::dimRedAvailableMethods(),
      selected = "tsne"
    ),
    selectInput(
      inputId = ns("cluster_method"), 
      label = "Cluster method", 
      choices = cluster_methods,
      selected = cluster_methods[[1]]
    ),
    sliderInput(
      inputId = ns("n_genes"),
      label = "Number of genes",
      min = 40,
      max = 100,
      value = 50
    ),
    checkboxInput(
      inputId = ns("use_labels"), 
      label = "Show gene symbols", 
      value = TRUE
    ),
    checkboxInput(
      inputId = ns("use_cor_matrix"), 
      label = "Use Correlation Matrix", 
      value = TRUE
    )
  )
}

#' Gene Expression Dimension Reduction Module
#' 
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#' @param fm FutureManager object
#' @param Results 
#' @param TableData 
#' @param getDataGeneExpression 
#'
#' @importFrom tibble column_to_rownames
#' @importFrom FutureManager fmValidate fmGenerateTaskId
#' @importFrom tidyr pivot_wider
#' @export
#' @rdname geneExpressionDimRedTab
#' 
geneExpressionDimRedTab <- function(input, output, session, fm, Results, TableData, classLabel, getDataGeneExpression){
  ns <- session$ns
  
  GeneData <- reactive({
    req(input$n_genes)
    
    res <- Results()
    fmValidate(res)
    cs <- res$cs
    
    anno <- TableData() %>% head(input$n_genes)
    
    req(anno)
    
    column <- getOption("xiff.column")
    column <- rlang::sym(column)
    
    rawData <- c(cs$class1, cs$class2) %>%
      getDataGeneExpression() %>%
      filter(ensg %in% anno$ensg) %>%
      tidyr::pivot_wider(names_from = !!column, values_from = log2tpm) %>%
      tibble::column_to_rownames("ensg")
    
    list(
      anno = anno,
      rawData = rawData
    )
  })
  
  DimRedData <- reactiveVal()
  reRunRequired <- FALSE
  
  Args <- reactive({
    reRunRequired <<- TRUE
    
    list(
      gd = GeneData(),
      method = input$method,
      useCorrelation = input$use_cor_matrix
    )
  })
  
  ArgsTrigger <- reactive({
    hiddenId <- paste0("output_", session$ns("indicator"), "_hidden")
    list(
      args = Args(),
      isHidden = session$clientData[[hiddenId]]
    )
  })
  
  taskId <- "dim_red_data" # some initial ID
  observeEvent(
    eventExpr = ArgsTrigger(),
    handlerExpr = {
      d <- ArgsTrigger()
      
      if (d$isHidden || !reRunRequired) return()
      reRunRequired <<- FALSE
      
      args <- d$args
      req(validateArgs(args))
      
      fm$cancel(taskId)
      
      # generate new ID in case of changing args
      # new process with the latest args will spin up
      # the previous process with outdated args will be canceled
      taskId <<- fmGenerateTaskId("dim_red_data") 
      fm$showProgress(taskId, "DimRed", DimRedData)
      fm$run(
        taskId = taskId,
        fun = dimRedLongFun,
        args = args,
        statusVar = DimRedData
      )
    }
  )
  
  ClusterData <- reactive({
    clusterMethod <- input$cluster_method
    validate(need(clusterMethod, "missing input values"))
    
    drd <- DimRedData()
    fmValidate(drd)
    
    res <- calculateClusters(
      dimRedResults = drd, 
      method = clusterMethod
    )
    
    res$clusterRes <- res$clusterRes %>%
      left_join(drd$anno, by = c("id" = "ensg")) %>%
      mutate(label = ifelse(is.na(symbol), id, symbol))
    
    res
  })
  
  PlotExpr <- reactive({
    useLabels <- input$use_labels
    validate(need(is.logical(useLabels), "missing input values"))
    
    d <- ClusterData()
    validate(need(d, "missing cluster data"))
    
    df <- d$clusterRes %>%
      mutate(
        higher = factor(
          stripHtml(higher),
          levels = unlist(reactiveValuesToList(classLabel))
        )
      )
    
    labelVar <- if (useLabels) "label"
    
    generatePointPlot(
      df = df,
      xVar = "X1",
      yVar = "X2",
      colorVar = "higher",
      shapeVar = "cluster",
      tooltipVar = "label",
      labelVar = labelVar,
      xlabel = d$xlabel,
      ylabel = d$ylabel
    )
  })
  
  callback <- function(x){
    x[["label"]]
  }
  
  callModule(
    module = plotWrapper,
    id = "plot",
    PlotExpr = PlotExpr,
    varDict = list(),
    PlotType = TRUE,
    tooltipCallback = callback
  )
}

dimRedLongFun <- function(task, gd, method, useCorrelation){
  if (!is.null(task)) {
    if (fmIsInterrupted(task)) return()
    fmUpdateProgress(task, progress = 0.05, msg = "dimension reduction...")
  }
  
  df <- gd$rawData
  res <- switch(
    EXPR = method,
    tsne = calculateTSNE(df, useCorrelation),
    pca = calculatePCA(df, useCorrelation),
    umap = calculateUMAP(df, useCorrelation),
    phate = calculatePhate(df, useCorrelation)
  )
  
  res[["anno"]] <- gd$anno
  res
}

generatePointPlot <- function(df, xVar = "x", yVar = "y", colorVar = "color", 
                              shapeVar = NULL, labelVar = NULL, tooltipVar = NULL, 
                              size = 3, ...){
  xVar <- rlang::sym(xVar)
  yVar <- rlang::sym(yVar)
  colorVar <- rlang::sym(colorVar)
  
  mapping <- aes(
    x = !!xVar,
    y = !!yVar,
    color = !!colorVar,
    dummy = !!tooltipVar
  )
  
  if (!is.null(shapeVar)){
    shapeVar <- sym(shapeVar)
    mapping[["shape"]] <- shapeVar
  }
  
  if (!is.null(tooltipVar)){
    tooltipVar <- sym(tooltipVar)
    mapping[["dummy"]] <- tooltipVar
  }
  
  p <- generatePlotBase(
    df = df,
    mapping = mapping,
    ...
  ) +
    geom_point(size = size)
  
  notNumeric <- !is.numeric(df[[colorVar]]) || is.integer(df[[colorVar]])
  
  if (length(unique(df[[colorVar]])) <= 7 && notNumeric) {
    p <- p + scale_color_manual(values = plotColors)
  }
  
  addLabels(p, labelVar)
}

generatePlotBase <- function(df, mapping, title = NULL, xlabel = NULL, ylabel = NULL, fontSize = 15, legendPosition = "bottom"){
  p <- ggplot(
    data = df,
    mapping = mapping
  ) + commonPlotTheme() +
    theme(
      plot.title = element_text(hjust = 0.5),
      text = element_text(size = fontSize),
      legend.position = legendPosition
    )
  
  if (!is.null(title)) p <- p + ggtitle(title)
  if (!is.null(xlabel)) p <- p + xlab(xlabel)
  if (!is.null(ylabel)) p <- p + ylab(ylabel)
  
  p
}

addLabels <- function(p, labelVar = NULL, fontSize = 4){
  if (!is.null(labelVar)){
    labelVar <- rlang::sym(labelVar)
    
    p <- p + ggrepel::geom_text_repel(
      mapping = aes(label = !!labelVar),
      size = fontSize,
      show.legend = FALSE
    )
  }
  
  p
}
