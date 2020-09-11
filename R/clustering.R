addClustering <- function(df, cluster_method, p = TRUE) {
  if (p) {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "clustering...", value = 0.5)
  }
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
      purrr::map_dfr(as.tibble, .id = "cluster") %>%
      dplyr::arrange(value) %>%
      dplyr::mutate(cluster = as.factor(cluster)) %>%
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

#' @export
calcPCA_expression <- function(PCAData, geneSource, numGenes = 300, p = TRUE) {
  if (is.null(PCAData)) return()
  if (p) {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "sorting genes by variance", value = 0.3)
  }
  data.counts <- PCAData[["data.counts"]]
  maxVarRows <- order(apply(PCAData[["data.log2tpm"]], 1, var), decreasing = TRUE)
  if (geneSource == "varying_genes") maxVarRows <- maxVarRows[1:numGenes]
  data.counts <- data.counts[maxVarRows, ]

  if (p) progress$set(message = "transforming count matrix", value = 0.4)
  dds <- DESeq2::DESeqDataSetFromMatrix(countData = data.counts, colData = PCAData[["assignment"]], design = ~ class)
  if (p) progress$set(message = "variance stabilization", value = 0.6)
  vst <- DESeq2::varianceStabilizingTransformation(dds, fitType = "local")
  if (p) progress$set(message = "calculating PCA", value = 0.8)
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
calcPCA_Score <- function(PCAData, geneSource, numGenes = 300, unit = "score", p = TRUE) {
  if (is.null(PCAData)) return()
  if (p) {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "sorting genes by variance", value = 0.3)
  }

  data_score <- PCAData[[paste0("data.", unit)]]
  maxVarRows <- order(apply(data_score, 1, var), decreasing = TRUE)
  if (geneSource == "varying_genes") maxVarRows <- maxVarRows[1:numGenes]
  data_score <- data_score[maxVarRows, ]

  pcadata <- prcomp(t(data_score), center = TRUE, scale. = TRUE)

  colname <- getOption("xiff.column")
  result <- tibble::tibble(!!colname := rownames(pcadata$x), pcadata$x[, 1:2]) %>%
    dplyr::left_join(PCAData$assignment, by = colname)
  eigs <- pcadata$sdev^2

  res <- list(
    data = result,
    genes = nrow(data_score),
    percentVar = signif(100 * eigs/sum(eigs), 3)
  )
  res[[getOption("xiff.name")]] <- ncol(data_score)
  res
}

#' @export
calcTSNE <- function(TSNEData, geneSource, numGenes = 300, unit = "log2tpm", p = TRUE) {
  if (is.null(TSNEData)) return()
  if (p) {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "sorting genes by variance", value = 0.1)
  }
  data.cross <- TSNEData[[paste0("data.", unit)]]
  maxVarRows <- order(apply(data.cross, 1, var), decreasing = TRUE)
  if (geneSource == "varying_genes") maxVarRows <- maxVarRows[1:numGenes]
  data.cross <- data.cross[maxVarRows, ]

  if (p) progress$set(message = "calculating t-SNE", value = 0.5)

  perp <- min(50, round(0.5 + ncol(data.cross)/10))
  res <- Rtsne::Rtsne(t(data.cross), perplexity = perp, inital_dims = min(50, numGenes))
  myTitle <- glue::glue("perplexity={res$perplexity}  #{getOption('xiff.label)}={res$N}  #genes={nrow(data.cross)}")

  res <- list(
    data = cbind(data.frame(res$Y), TSNEData[["assignment"]]),
    title = myTitle,
    percentVar = signif(100 * eigs/sum(eigs), 3)
  )
  res[[getOption("xiff.name")]] <- res$N
  res
}

#' @export
calcUMAP <- function(UMAPData, geneSource, numGenes = 30, unit = "log2tpm", p = TRUE){
  #save(UMAPData, geneSource, numGenes, unit, file = "calcUMAP.Rdata")
  if (is.null(UMAPData)) return()
  if (p) {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "sorting genes by variance", value = 0.3)
  }
  data.cross <- UMAPData[[paste0("data.", unit)]]
  maxVarRows <- order(apply(data.cross, 1, var), decreasing = TRUE)
  if (geneSource == "varying_genes") maxVarRows <- maxVarRows[1:numGenes]
  data.cross <- data.cross[maxVarRows, ]

  if (p) progress$set(message = "calculating umap", value = 0.6)

  perp <- min(50, round(0.5 + ncol(data.cross)/10))
  res <- umap::umap(t(data.cross), perplexity = perp)

  colname <- getOption("xiff.column")
  umapdata <- res$layout %>% as.data.frame() %>% dplyr::rename(X1 = 1, X2 = 2) %>% tibble::rownames_to_column(colname)
  cl_genes <- dim(res$data)
  myTitle <- glue::glue("perplexity={perp}  #{getOption('xiff.label)}={cl_genes[[1]]}  #genes={cl_genes[[2]]}")

  res <- list(
    data = dplyr::left_join(umapdata, UMAPData[["assignment"]], by = colname),
    title = myTitle
  )
  res[[getOption("xiff.name")]] <- ncol(data.cross)
  res
}
