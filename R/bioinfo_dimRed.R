calculateClusters <- function(dimRedResults, method = "k-means automatic"){
  resDf <- dimRedResults$res
  resDf <- resDf[rowSums(is.na(resDf)) == 0, ]
  
  clusterRes <- XIFF:::addClustering(resDf, method)
  
  list(
    clusterRes = clusterRes,
    perp = dimRedResults$perp,
    xlabel = dimRedResults$xlabel,
    ylabel = dimRedResults$ylabel
  )
}

calculateTSNE <- function(df, useCorrelation = TRUE){
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
  perp <- min(50, round(0.5 + n/10))
  
  res <- Rtsne::Rtsne(
    X = dfNoDuplicates,
    perplexity = perp,
    inital_dims = min(50, ncol(df))
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
    perp = perp,
    xlabel = "t-SNE-1",
    ylabel = "t-SNE-2"
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
    res <- prcomp(df)
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
    ylabel = "PC-2"
  )
}

calculateUMAP <- function(df, useCorrelation = TRUE){
  df <- sortByVariance(df)
  
  if(useCorrelation) {
    df <- df[!(apply(df, 1, sd) == 0),] # remove sd = 0
    df <- as.data.frame(cor(t(df)))
  }
  
  n <- nrow(df)
  
  perp <- min(50, round(0.5 + n/10))
  res <- umap::umap(df, perplexity = perp)
  
  colnames(res$layout) <- c("X1", "X2")
  resDf <- res$layout %>% 
    tibble::as_tibble() %>% 
    mutate(id = rownames(df)) %>%
    relocate(id, .before = 1)
  
  list(
    res = resDf,
    perp = perp,
    xlabel = "umap-1",
    ylabel = "umap-2"
  )
}

calculatePhate <- function(df, useCorrelation = TRUE) {
  
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
  
  phateRes <- phateR::phate(data = df)
  resDf <- phateRes$embedding %>% tibble::as_tibble() %>% 
    mutate(id = rownames(df)) %>%
    relocate(id, .before = 1)
  resDf <- resDf %>% rename(X1 = PHATE1, X2 = PHATE2)    
  
  list(
    res = resDf,
    perp = NA,
    xlabel = "PHATE1",
    ylabel = "PHATE2"
  )
  
}

sortByVariance <- function(df){
  maxVarCols <- order(sapply(df, var), decreasing = TRUE)
  df[maxVarCols]
}
