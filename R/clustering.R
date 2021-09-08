addClustering <- function(df, cluster_method, p = FALSE) {
  progress <- ProcessProgress$new("Clustering", p)
  progress$update(0.5, "clustering...")

  data <- select_if(df, is.numeric)
  mc <- min(nrow(data) - 1, 20)

  if (grepl("k-means", cluster_method)) {
    if (grepl("automatic", cluster_method)) {
      a <- sapply(2:mc, function(k) sum(kmeans(data, centers = k)$withinss))
      b <- a[1:(mc - 2)] - a[2:(mc - 1)]
      d <- b[1:(mc - 3)] - b[2:(mc - 2)]
      k <- which(d == max(d)) + 1
      df$cluster <- as.factor(kmeans(data, centers = k)$cluster)
    } else {
      k <- as.numeric(gsub("k-means k = ", "", cluster_method))
      df$cluster <- as.factor(kmeans(data, centers = k)$cluster)
    }
  } else if (cluster_method == "affinity propagation") {
    apres <- apcluster::apcluster(apcluster::negDistMat(data, r = 2))
    df$cluster <- apres@clusters %>%
      purrr::map_dfr(as_tibble, .id = "cluster") %>%
      arrange(value) %>%
      mutate(cluster = as.factor(cluster)) %>%
      .$cluster
  } else if (grepl("pam", cluster_method)) {
    if (grepl("automatic", cluster_method)) {
      df$cluster <- as.factor(fpc::pamk(data, krange = 2:mc)$pamobject$clustering)
    } else {
      k <- as.numeric(gsub("pam k = ", "", cluster_method))
      df$cluster <- as.factor(fpc::pamk(data, k)$pamobject$clustering)
    }
  }
  df
}

# Returns genes order by variance and limits their number if needed
orderByVariance <- function(data.cross, geneSource, numGenes) {
  maxVarRows <- order(apply(data.cross, 1, var), decreasing = TRUE)
  if (geneSource == "varying_genes") maxVarRows <- head(maxVarRows, numGenes)
  maxVarRows
}

#' @export
calcPCA_expression <- function(PCAData, geneSource, numGenes = 300, p = FALSE) {
  if (is.null(PCAData)) return()

  progress <- ProcessProgress$new("PCA", p)
  progress$update(0.3, "sorting genes by variance...")

  maxVarRows <- orderByVariance(PCAData[["data.log2tpm"]], geneSource, numGenes)
  data.counts <- PCAData[["data.counts"]][maxVarRows, ]
  
  progress$update(0.4, "transforming count matrix...")

  assignment <- PCAData[["assignment"]]
  design <- if (n_distinct(assignment$class) > 1){
    "~ class"
  } else {
    "~ 1"
  }

  dds <- DESeq2::DESeqDataSetFromMatrix(
    countData = data.counts,
    colData = assignment,
    design = as.formula(design)
  )

  progress$update(0.6, "variance stabilization...")

  vst <- DESeq2::varianceStabilizingTransformation(dds, fitType = "local")

  progress$update(0.8, "calculating PCA...")

  pcadata <- DESeq2::plotPCA(vst, intgroup = c("class"), returnData = TRUE)
  percentVar <- round(100 * attr(pcadata, "percentVar"))

  colnames(pcadata)[5] <- getOption("xiff.column")

  res <- list(
    data = pcadata,
    genes = nrow(data.counts),
    percentVar = percentVar
  )
  res[[getOption("xiff.name")]] <- ncol(data.counts)
  res
}

#' @export
calcTSNE <- function(TSNEData, geneSource, numGenes = 300, unit = "log2tpm", p = FALSE) {
  if (is.null(TSNEData)) return()

  progress <- ProcessProgress$new("t-SNE", p)
  progress$update(0.1, "sorting genes by variance...")

  data.cross <- TSNEData[[paste0("data.", unit)]]
  maxVarRows <- orderByVariance(data.cross, geneSource, numGenes)
  data.cross <- data.cross[maxVarRows, ]
  
  progress$update(0.5, "calculating t-SNE...")

  perp <- min(50, round(0.5 + ncol(data.cross)/10))
  res <- Rtsne::Rtsne(
    X = t(data.cross),
    perplexity = perp,
    inital_dims = min(50, numGenes)
  )
  myTitle <- glue::glue("perplexity={res$perplexity}  #{getOption('xiff.label')}s={res$N}  #genes={nrow(data.cross)}")

  final_res <- list(
    data = cbind(data.frame(res$Y), TSNEData[["assignment"]]),
    title = myTitle
  )
  final_res[[getOption("xiff.name")]] <- res$N
  final_res
}

#' @export
calcUMAP <- function(UMAPData, geneSource, numGenes = 30, unit = "log2tpm", p = FALSE){
  if (is.null(UMAPData)) return()
  
  progress <- ProcessProgress$new("UMAP", p)
  progress$update(0.3, "sorting genes by variance...")

  data.cross <- UMAPData[[paste0("data.", unit)]]
  maxVarRows <- orderByVariance(data.cross, geneSource, numGenes)
  data.cross <- data.cross[maxVarRows, ]

  progress$update(0.6, "calculating umap...")

  perp <- min(50, round(0.5 + ncol(data.cross)/10))
  res <- umap::umap(t(data.cross), perplexity = perp)

  colname <- getOption("xiff.column")
  umapdata <- res$layout %>% as.data.frame() %>% rename(X1 = 1, X2 = 2) %>% tibble::rownames_to_column(colname)
  cl_genes <- dim(res$data)
  myTitle <- glue::glue("perplexity={perp}  #{getOption('xiff.label')}s={cl_genes[[1]]}  #genes={cl_genes[[2]]}")

  res <- list(
    data = left_join(umapdata, UMAPData[["assignment"]], by = colname),
    title = myTitle
  )
  res[[getOption("xiff.name")]] <- ncol(data.cross)
  res
}

calcPHATE <- function(PHATEdata, geneSource, numGenes = 30, unit = "log2tpm", p = FALSE){
  
  
  if (is.null(PHATEdata)) return()
  
  progress <- ProcessProgress$new("PHATE", p)
  progress$update(0.3, "sorting genes by variance...")
  
  data.cross <- PHATEdata[[paste0("data.", unit)]]
  maxVarRows <- orderByVariance(data.cross, geneSource, numGenes)
  data.cross <- data.cross[maxVarRows, ]
  
  phate <- phateR::phate(t(data.cross))
  
  colname <- getOption("xiff.column")
  phateData <- phate$embedding %>%
    as.data.frame() %>%
    rename(X1 = PHATE1, X2 = PHATE2) %>% 
    tibble::rownames_to_column(colname)
  
  res <- list(
    data = left_join(phateData, PHATEdata[["assignment"]], by = colname)
  )
  res[[getOption("xiff.name")]] <- ncol(data.cross)
  res
  
}
