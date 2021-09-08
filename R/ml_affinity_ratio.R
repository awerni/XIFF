#' Feature selection using GREP methodology.
#'
#' @param df data frame with ensg.
#' @param threshold not used, can be anything. Rquired only for compability
#'        with other feature selection methods.
#' @param maxFeatures max number of genes to be used in ratio calculations.
#' @param minFeatures min number of features resulting from affinity propagation.
#' @param epsilonRNAseq
#'
#' @return
#' @export
#'
#' @examples
getGrepFeatureSelection <- function(df,
                                    threshold = 0.05,
                                    maxFeatures = 600,
                                    maxGenes = 300,
                                    minFeatures = 15,
                                    epsilonRNAseq = 10,
                                    cor.method = "pearson",
                                    .otherParams = list()) {
  
  dfNum <- df %>% select_if(is.numeric)
  log_trace("GREP: Feature Selection - start: {ncol(dfNum)}")
  
  dfNum <- mlGrepFilterLowExpressionAndVariabilityGenes(
    dfNum,
    maxGenes,
    epsilonRNAseq
  )
  log_trace("GREP: Feature Selection - after low expression: {ncol(dfNum)}")
  
  .otherParams$epsilonRNAseq <- epsilonRNAseq
  ratioMatrix <- mlGetLog2RatiosMatrix(dfNum, epsilonRNAseq = epsilonRNAseq)
  
  
  filteredRatioMatrix <- mlGrepGetSignificantFeatures(
    mat = ratioMatrix,
    class = df[["class"]],
    N = maxFeatures)
  
  
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
  res$.otherParams <- .otherParams
  res
}




################## Internal GREP/Ratios functions #################
mlGrepFilterLowExpressionAndVariabilityGenes <- function(dfNum,
                                                         maxFeatures,
                                                         epsilonRNAseq,
                                                         varianceEpsilon = 0.0001) {
  
  maxExprAboveTreshold <-
    vapply(
      dfNum,
      FUN.VALUE = 0.0,
      FUN = function(x) max(2^x - 1)
    ) > epsilonRNAseq
  
  dfNum <- dfNum[,maxExprAboveTreshold]
  
  if(ncol(dfNum) == 0) {
    msg <- glue::glue(
      "There are no enought genes with",
      " max(log2tpm) > log2(epsilonRNAseq)",
      ", log2(epsilonRNAseq) = log2({epsilonRNAseq}) = {round(log2(epsilonRNAseq),5)}"
    )
    stop(msg)
  }
  
  
  varCoef <- vapply(dfNum, FUN.VALUE = 0.0, FUN = function(x) {
    xx <- 2^x - 1
    if(sd(xx) < varianceEpsilon) return(0.0) # filter nearly zero variance genes
    sd(xx) / mean(xx)
  })
  
  varCoef <- sort(varCoef, decreasing = TRUE) %>% head(maxFeatures)
  varCoef <- varCoef[varCoef > varianceEpsilon]
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
  mat <- 2^mat - 1
  mat <- log2(mat + epsilonRNAseq)
  
  combs <- combn(sort(colnames(mat)), m = 2)
  log_trace("GREP - number of ratios: {ncol(combs)}")
  
  ratios <- mat[,combs[1,], drop = FALSE] - mat[,combs[2,], drop = FALSE]
  colnames(ratios) <- paste(combs[1,], combs[2,], sep = ".")
  ratios
}

mlGrepGetSignificantFeatures <- function(mat, class, N = 600) {
  if(is.character(class)) class <- as.factor(class)
  
  featuresInfo <- genefilter::colttests(mat, class) %>% 
    tibble::rownames_to_column(var = "feature") %>% 
    tibble::as_tibble() %>%
    dplyr::arrange(p.value) %>%
    head(N)
  
  log_trace(
    "GREP - Significant features, ",
    " N: {N}",
    " Before: {ncol(mat)}",
    " After: {nrow(featuresInfo)}"
  )
  
  mat[,featuresInfo$feature, drop = FALSE]
}

#' @importFrom apcluster apclusterK
mlGrepAffinityPropagation <- function (mat, n, cor.method = "pearson") {
  
  if(ncol(mat) < 4) {
    log_trace("mlGrepAffinityPropagation",
    " - less than 4 features - returning whole matrix.")
    return(mat)
  }
  
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
    rename(`variable name` = ensg) %>%
    select(`variable name`, !!importanceLabel, ensg1, ensg2, symbol1, symbol2)
}

######### getDataForModel for GREP with support functions #########
#' @export
getDataForModel.XiffGREP <- function(assignment,
                                     features,
                                     schema = getOption("xiff.schema"),
                                     column = getOption("xiff.column"),
                                     classLabel = NULL) {
  
  if(is.null(classLabel)) features$classLabel
  df <- getDataForModel(assignment,
                    mlGrepGetBestFeatures(features$bestFeatures),
                    schema = schema,
                    column = column,
                    classLabel = classLabel)
  
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
  
  class <- mlGetClassColumn(model, x, asSymbol = TRUE)
  
  res <- x %>% select(!!class, !!rlang::sym(getOption("xiff.column")))
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
  
  cs <- model$trainingItems
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


#' @export
getRawDataForModel.XiffGREP <- function(features,
                                      names = NULL,
                                      schema = getOption("xiff.schema"),
                                      column = getOption("xiff.column")) {
  
  rawDt <- getRawDataForModel(mlGrepGetBestFeatures(features$bestFeatures),
                     names,
                     schema,
                     column)
  
  eps <- features$otherParams$epsilonRNAseq
  
  featuresAll <- strsplit(features$bestFeatures, split = "\\.")
  leftSide  <- sapply(featuresAll, "[[", 1)
  rightSide <- sapply(featuresAll, "[[", 2)
  
  rawDtLeft <- rawDt %>% filter(ensg %in% leftSide) %>% rename(lensg = ensg)
  rawDtRight <- rawDt %>% filter(ensg %in% rightSide) %>% rename(rensg = ensg)
  
  data <- inner_join(rawDtLeft, rawDtRight, by = column)
  
  data2 <- data %>% mutate(
    ensg = paste(lensg, rensg, sep = "."),
    score = log2(2^score.x - 1 + eps) - log2(2^score.y - 1 + eps)
  ) %>% select(
    -score.x, -score.y, -rensg, -lensg
  )
  
  data2 <- data2 %>% filter(ensg %in% features$bestFeatures)
  
  data2
}

mlGrepJoinAnno <- function(importanceRes, geneAnno) {
  
  importanceRes <- importanceRes  %>% mutate(ensg2 = ensg) %>%
    tidyr::separate(ensg2, c("ensg1", "ensg2"), sep = "\\.") %>%
    left_join(geneAnno, by = c("ensg1" = "ensg")) %>%
    left_join(geneAnno, by = c("ensg2" = "ensg"))
  
  colnames(importanceRes) <- 
    gsub(colnames(importanceRes), pattern = "\\.x$", replacement = "1")
  colnames(importanceRes) <- 
    gsub(colnames(importanceRes), pattern = "\\.y$", replacement = "2")
  importanceRes
  
}


#' Strip GREP model into simple format.
#'
#' @param grepFit GREP model. 
#' @param geneAnno gene annotations
#'
#' @return list with modelCoefficients and epsilon
#' 
#' @export
stripGrepModel <- function(grepFit, geneAnno) {
  
  finalModel <- grepFit$finalModel
  
  ### extract coefficients ###
  coefs <- data.frame(Coefficient = coefficients(finalModel)) %>%
    tibble::rownames_to_column(var = "Ratios")
  
  coefs <- coefs %>% tidyr::separate(
    col = "Ratios",
    sep = "\\.",
    into = c("ensg1", "ensg2"),
    remove = FALSE,
    fill = "left"
  ) %>% mutate(ensg2 = ifelse(Ratios == "(Intercept)", NA, ensg2))
  
  ### add gene symbols
  anno <- geneAnno %>% select(ensg, symbol) %>% as_tibble()
    
  anno1 <- anno %>% rename(gene1 = symbol)
  anno2 <- anno %>% rename(gene2 = symbol)
  modelCoef <- coefs %>%
    left_join(anno1, by = c("ensg1" = "ensg")) %>%
    left_join(anno2, by = c("ensg2" = "ensg"))
  
  
  modelCoef <-
    modelCoef %>% mutate(Ratios = ifelse(
      Ratios == "(Intercept)",
      "(Intercept)",
      paste(gene1, gene2, sep = ".")
    )) %>% select(Ratios, Coefficient, gene1, gene2, ensg1, ensg2)
  
  list(
    modelCoefficients = modelCoef,
    epsilon = grepFit$otherParams$epsilonRNAseq
  )
  
}

