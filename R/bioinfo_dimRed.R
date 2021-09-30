calculateClusters <- function(dimRedResults, method = "k-means automatic"){
  resDf <- dimRedResults$res
  resDf <- resDf[rowSums(is.na(resDf)) == 0, ]
  
  clusterRes <- XIFF:::addClustering(resDf, method)
  
  list(
    clusterRes = clusterRes,
    perp = dimRedResults$perp,
    xlabel = dimRedResults$xlabel,
    ylabel = dimRedResults$ylabel,
    dimRedObj = dimRedResults$dimRedObj
  )
}

calculateTSNE <- function(df, useCorrelation = TRUE, perplexity = "auto", inital_dims = "auto", ...){
  df <- sortByVariance(df)
  
  d <- duplicated(df)
  dfNoDuplicates <- df[!d, ]
  
  if(useCorrelation) {
    df <- df[!(apply(df, 1, sd) == 0),] # remove sd = 0
    d <- duplicated(df)
    dfNoDuplicates <- df[!d, ]
    dfNoDuplicates <- as.data.frame(cor(t(dfNoDuplicates)))
  } else {
    d <- duplicated(df)
    dfNoDuplicates <- df[!d, ]
  }
  
  n <- nrow(dfNoDuplicates)
  
  if(perplexity == "auto") {
    perplexity <- min(50, round(0.5 + n/10))  
  }
  if(inital_dims == "auto") {
    inital_dims <- min(50, ncol(df))
  }
  
  res <- Rtsne::Rtsne(
    X = dfNoDuplicates,
    perplexity = perplexity,
    inital_dims = inital_dims,
    ...
  )
  
  colnames(res$Y) <- c("X1", "X2")
  values <- tibble::as_tibble(res$Y)
  
  resDf <- tibble::tibble(
    id = rownames(df),
    X1 = rep(NA_real_, nrow(df)),
    X2 = NA_real_
  )
  resDf[!d, c("X1", "X2")] <- values

  
  list(
    res = resDf,
    perp = perplexity,
    xlabel = "t-SNE-1",
    ylabel = "t-SNE-2",
    dimRedObj = res
  )
}

calculatePCA <- function(df, useCorrelation = TRUE){
  df <- sortByVariance(df)
  
  if(useCorrelation) {
    df <- df[!(apply(df, 1, sd) == 0),] # remove sd = 0
    df <- as.data.frame(cor(t(df)))
    
    # based on http://www.sthda.com/english/wiki/wiki.php?id_contents=7866
    # how to calculate PCA based on the cor matrix
    res <- eigen(df)$vectors
    res <- list(x = res[,1:2])
    
  } else {
    res <- prcomp(df, ...)
  }
  
  resDf <- tibble::tibble(
    id = rownames(df),
    X1 = res$x[, 1], 
    X2 = res$x[, 2]
  )
  
  list(
    res = resDf,
    perp = NA,
    xlabel = "PC-1",
    ylabel = "PC-2",
    dimRedObj = res
  )
}

calculateUMAP <- function(df, useCorrelation = TRUE, perplexity = "auto", ...){
  df <- sortByVariance(df)
  
  if(useCorrelation) {
    df <- df[!(apply(df, 1, sd) == 0),] # remove sd = 0
    df <- as.data.frame(cor(t(df)))
  }
  
  n <- nrow(df)
  
  if(perplexity == "auto") {
    perplexity <- min(50, round(0.5 + n/10))  
  }
  
  res <- umap::umap(df, perplexity = perplexity, ...)
  
  colnames(res$layout) <- c("X1", "X2")
  resDf <- res$layout %>% 
    tibble::as_tibble() %>% 
    mutate(id = rownames(df)) %>%
    relocate(id, .before = 1)
  
  list(
    res = resDf,
    perp = perplexity,
    xlabel = "umap-1",
    ylabel = "umap-2",
    dimRedObj = res
  )
}

calculatePhate <- function(df, useCorrelation = TRUE, ...) {
  
  if(!requireNamespace("phateR")) {
    stop("Please install phateR package to use phate dimension reduction method.")
  } else {
    if(!reticulate::py_module_available(module = "phate")) {
      stop("phateR depends on the Python package `phate`",
           " which is currently not installed.", 
           " Please run phateR::install.phate() to install it.",
           " Call ?phateR::install.phate for more information",
           " about the installation process.")
    }
  }
  
  df <- sortByVariance(df)
  if(useCorrelation) {
    df <- df[!(apply(df, 1, sd) == 0),] # remove sd = 0
    df <- as.data.frame(cor(t(df)))
  }
  
  phateRes <- phateR::phate(data = df, ...)
  resDf <- phateRes$embedding %>% tibble::as_tibble() %>% 
    mutate(id = rownames(df)) %>%
    relocate(id, .before = 1)
  resDf <- resDf %>% rename(X1 = PHATE1, X2 = PHATE2)    
  
  list(
    res = resDf,
    perp = NA,
    xlabel = "PHATE1",
    ylabel = "PHATE2",
    dimRedObj = phateRes
  )
  
}

sortByVariance <- function(df){
  maxVarCols <- order(sapply(df, var), decreasing = TRUE)
  df[maxVarCols]
}


#' Gene Expression Dimension Reduction
#'
#' @param geneExpr gene expression data
#' @param anno gene annotation
#' @param method dimension reduction method
#' @param clusterMethod clustering method
#' @param useCorrelation should the dimension reduction be done on the corr matrix
#' @param ... parameters passed to dimension reduction functions.
#' @param ca 
#'
#' @return
#' @export
#'
#' @examples
geneExpressionDimRed <-
  function(geneExpr,
           anno,
           method = XIFF::dimRedAvailableMethods(),
           clusterMethod = c(
             "pam k = automatic", "pam k = 2", "pam k = 3", "pam k = 4", 
             "affinity propagation",
             "k-means k = automatic", "k-means k = 2",
             "k-means k = 3", "k-means k = 4"
           ),
           useCorrelation = TRUE,
           ...,
           ca = NULL) {
    
  method <- match.arg(method, XIFF::dimRedAvailableMethods())
  clusterMethod <- clusterMethod[[1]]
  
  column <- getOption("xiff.column")
  column <- rlang::sym(column)
  
  rawData <- geneExpr %>%
    filter(ensg %in% anno$ensg) %>%
    tidyr::pivot_wider(names_from = !!column, values_from = log2tpm) %>%
    tibble::column_to_rownames("ensg")
  
  res <- switch(
    EXPR = method,
    tsne = calculateTSNE(rawData, useCorrelation, ...),
    pca  = calculatePCA(rawData, useCorrelation, ...),
    umap  = calculateUMAP(rawData, useCorrelation, ...),
    phate = calculatePhate(rawData, useCorrelation, ...)
  )
  
  resWithClusters <- calculateClusters(dimRedResults = res,  method = clusterMethod)
  resWithClusters$clusterRes <- resWithClusters$clusterRes %>%
    left_join(anno, by = c("id" = "ensg")) %>%
    mutate(label = ifelse(is.na(symbol), id, symbol))
  
  if(inherits(anno, "ClassAssigmentInAttribute") && is.null(ca)) {
    ca <- getClassAssigmentAttribute(anno)
  }
  
  if(!is.null(ca)) {
    resWithClusters <- addClassAssigmentAttribute(resWithClusters, ca)
  }
  
  resWithClusters
}

#' @export
geneExpressionDimRedPlot <- function(dimRed, useLabels = FALSE) {
  
  df <- dimRed$clusterRes
  labelVar <- if(useLabels) "label"
  
  generatePointPlot(
    df = df,
    xVar = "X1",
    yVar = "X2",
    colorVar = "higher",
    shapeVar = "cluster",
    tooltipVar = "label",
    labelVar = labelVar,
    xlabel = dimRed$xlabel,
    ylabel = dimRed$ylabel
  )
}
