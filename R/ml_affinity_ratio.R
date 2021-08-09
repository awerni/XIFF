#' Feature selection using GREP methodology.
#'
#' @param df data frame with ensg.
#' @param threshold threshold for FDR.
#' @param maxFeatures max number of genes to be used in ratio calculations.
#' @param minFeatures min number of features resulting from affinity propagation.
#' @param epsilonRNAseq
#'
#' @return
#' @export
#'
#' @examples
getGrepFeatureSelection <- function(df,
                                    threshold = 0.1,
                                    maxFeatures = 750,
                                    maxGenes = 300,
                                    minFeatures = 15,
                                    epsilonRNAseq = 10,
                                    cor.method = "pearson") {
  
  dfNum <- df %>% select_if(is.numeric)
  log_trace("GREP: Feature Selection - start: {ncol(dfNum)}")
  
  dfNum <- mlGrepFilterLowExpressionAndVariabilityGenes(
    dfNum,
    maxGenes,
    epsilonRNAseq
  )
  log_trace("GREP: Feature Selection - after low expression: {ncol(dfNum)}")
    
  ratioMatrix <- mlGetLog2RatiosMatrix(dfNum, epsilonRNAseq = epsilonRNAseq)
  
  
  filteredRatioMatrix <- mlGrepGetSignificantFeatures(
    mat = ratioMatrix,
    class = df[["class"]],
    fdr = threshold,
    maxN = maxFeatures)
  
  
  examplarsFeaturesMatrix <- mlGrepAffinityPropagation(
    filteredRatioMatrix,
    n = minFeatures,
    cor.method = cor.method)
  
  dfSelected <- bind_cols(
    df %>% select(class),
    examplarsFeaturesMatrix %>% as.data.frame()
  )
  
  res <- selectBestFeaturesTTest(dfSelected, threshold = 1, maxFeatures = Inf)
  
  res$method <- "GREP"
  res
}




################## Internal GREP/Ratios functions #################
mlGrepFilterLowExpressionAndVariabilityGenes <- function(dfNum,
                                                         maxFeatures,
                                                         epsilonRNAseq) {
  
  maxExprAboveTreshold <- vapply(dfNum, FUN.VALUE = 0.0, FUN = max) > log2(epsilonRNAseq)
  
  dfNum <- dfNum[,maxExprAboveTreshold]
  
  varCoef <- vapply(dfNum, FUN.VALUE = 0.0, FUN = function(x) {
    xx <- 2^x
    sd(xx) / mean(xx)
  })
  
  varCoef <- sort(varCoef, decreasing = TRUE) %>% head(maxFeatures)
  
  log_trace(
    "GREP - prefilter genes:",
    " Before filtering: {length(maxExprAboveTreshold)}",
    " , After expression filter: {sum(maxExprAboveTreshold)}",
    " , Final: {length(varCoef)} "
  )
  
  dfNum <- dfNum[,names(varCoef)]
  dfNum
}


mlGetLog2RatiosMatrix <- function (df, epsilonRNAseq = 10) {
  
  # this function can only work for numeric features.
  # other features are dropped silently.
  df <- df %>% select_if(is.numeric)
  
  
  mat <- as.matrix(df)
  mat <- 2^mat
  mat <- log2(mat + epsilonRNAseq)
  
  combs <- combn(sort(colnames(mat)), m = 2)
  log_trace("GREP - number of ratios: {ncol(combs)}")
  
  ratios <- mat[,combs[1,], drop = FALSE] - mat[,combs[2,], drop = FALSE]
  colnames(ratios) <- paste(combs[1,], combs[2,], sep = ".")
  ratios
}

mlGrepGetSignificantFeatures <- function(mat, class, fdr = 0.05, maxN = 500) {
  if(is.character(class)) class <- as.factor(class)
  
  featuresInfo <- genefilter::colttests(mat, class) %>% 
    tibble::rownames_to_column(var = "feature") %>% 
    tibble::as_tibble() %>%
    dplyr::mutate(p_adj = p.adjust(p.value,method = "BH")) %>%
    dplyr::arrange(p_adj)
  
  
  featuresInfo <- featuresInfo %>% filter(p_adj <= fdr) %>% arrange(p_adj) %>% head(maxN)
  
  log_trace(
    "GREP - Significant features, ",
    " fdr: {fdr}",
    " maxN: {maxN}",
    " Before: {ncol(mat)}",
    " After: {nrow(featuresInfo)}"
  )
  
  mat[,featuresInfo$feature, drop = FALSE]
}

#' @importFrom apcluster apclusterK
mlGrepAffinityPropagation <- function (mat, n, cor.method = "pearson") {
  n <- pmin(pmax(2, n), ceiling(ncol(mat) / 2))
  distance <- 1 - abs(cor(mat, method = cor.method))
  clusters <- apcluster::apclusterK(distance, details = FALSE, 
                                    K = n, maxits = 2000)
  mat[,clusters@exemplars, drop = FALSE]
}

#' @export
mlGetTableData.XiffGREP <- function(model) {
  importanceName <- attr(model$df, "importanceName")
  importanceLabel <- paste0("importance (", importanceName, ")")
  
  model$df %>%
    mutate(importance = signif(importance, 3)) %>%
    rename(!!importanceLabel := importance) %>%
    mutate(ensg2 = ensg) %>%
    rename(`variable name` = ensg) %>%
    select(-symbol, -name, -location) %>% tidyr::separate(ensg2, c("ensg1", "ensg2"), sep = "\\.")
}

######### getDataForModel for GREP with support functions #########
#' @export
getDataForModel.XiffGREP <- function(assignment,
                                     features,
                                     schema = getOption("xiff.schema"),
                                     column = getOption("xiff.column")) {
  
  df <- getDataForModel(assignment,
                    mlGrepGetBestFeatures(features$bestFeatures),
                    schema = schema,
                    column = column)
  
  mlGrepTransformExpr2Ratio(df, features, features$otherParams$epsilonRNAseq)
  
}

mlGrepGetBestFeatures = function(x) {
  log_trace(
    "mlGrepGetBestFeatures",
    " - example: {paste(head(x,2), collapse = ', ')}")
  unique(strsplit(x, split = "\\.") %>% unlist)
}

mlGrepTransformExpr2Ratio <- function(x, model, epsilonRNAseq = 10) {
  
  log_trace("GREP - transform prediction epsilonRNAseq: {epsilonRNAseq}")
  quotientMatrix <- mlGetLog2RatiosMatrix(x, epsilonRNAseq)
  
  if(!all(model$bestFeatures %in% colnames(quotientMatrix))) {
    stop("Some ratios not available")
  }
  
  features <- as_tibble(as.data.frame(quotientMatrix[, model$bestFeatures]))
  
  res <- x %>% select(class, !!rlang::sym(getOption("xiff.column")))
  bind_cols(res, features)  
  
}


#' @export
mlGenerateExpressionPlot.XiffGREP <- function(model, df, ca, plotType = "point", gene) {
  generatePlotByType(
    data = df,
    ca = ca,
    plotType = plotType,
    dataCol = "ratio",
    title = gene$ensg,
    ylabel = "Ratio",
    trans = "identity"
  )
  
}

#' @export
mlGetTpmData.XiffGREP <- function(model, ensg, annoFocus) {
  
  cs <- model$cs
  ensg <- strsplit(ensg, split = "\\.") %>% unlist %>% sort
  
  data <- getDataGeneExpressionById(ensg, cs) %>% 
    addTumortypes(annoFocus)
  
  eps <- model$otherParams$epsilonRNAseq
  
  data2 <- data %>%
    group_by(!!rlang::sym(getOption("xiff.column")), tumortype) %>%
    arrange(ensg) %>%
    summarize(ensg = paste(ensg, collapse = "."), ratio = -diff(log2(tpm + eps)))
  
  data2
}
