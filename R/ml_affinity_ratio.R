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
                                    threshold = 0.01,
                                    maxFeatures = 750,
                                    maxGenes = 200,
                                    minFeatures = 15,
                                    epsilonRNAseq = 10,
                                    cor.method = "pearson") {
  
  df <- getDataForModel(assignment, features)
  
  dfNum <- df %>% select_if(is.numeric)
  
  dfNum <- mlGrepFilterLowExpressionAndVariabilityGenes(
    dfNum,
    maxGenes,
    epsilonRNAseq
  )
    
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



#' Extract Raw Gene names from model when GREP/Ratio is used.
#'
#' @param x character vector with best features.
#'
#' @return
#' @export
mlGrepGetBestFeatures = function(x) {
  unique(strsplit(x, split = "\\.") %>% unlist)
}

#' Create function that transforms raw log2tpm expression data into GREP/Ratio data.
#'
#' @param epsilonRNAseq 
#'
#' @return Data frame with ratios.
#' 
#' @details 
#' 
#' Usually this function should not be used by user directly. It is used when
#' method 'Ratio' is selected.
#' 
#' @export
#' 
mlGrepMakeTransformExpr2RatioFunction <- function(epsilonRNAseq = 10) {
  
  epsilonRNAseq <- force(epsilonRNAseq)
  
  transformFeaturesFnc <- function(x, model) {
    
    quotientMatrix <- mlGetLog2RatiosMatrix(x, epsilonRNAseq)
    
    if(!all(model$bestFeatures %in% colnames(quotientMatrix))) {
      stop("Some ratios not available")
    }
    
    features <- as_tibble(as.data.frame(quotientMatrix[, model$bestFeatures]))
    
    res <- dt2 %>% select(class, !!rlang::sym(getOption("xiff.column")))
    bind_cols(res, features)  
  }
  
  transformFeaturesFnc
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
  
  combs <- combn(colnames(mat), m = 2)
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
  
  mat[,featuresInfo$feature, drop = FALSE]
}

mlGrepAffinityPropagation <- function (mat, n, cor.method = "pearson") {
  n <- pmax(2, n)
  distance <- 1 - abs(cor(mat, method = cor.method))
  clusters <- apcluster::apclusterK(distance, details = FALSE, 
                                    K = n, maxits = 2000)
  mat[,clusters@exemplars, drop = FALSE]
}

