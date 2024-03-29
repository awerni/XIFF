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

calcPCA_expression <- function(PCAData, geneSource, numGenes = 300, p = FALSE, ...) {
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

  pcadata <- DESeq2::plotPCA(vst, intgroup = c("class"), returnData = TRUE, ...)
  percentVar <- round(100 * attr(pcadata, "percentVar"))

  colnames(pcadata)[5] <- getOption("xiff.column")

  
  myTitle <- glue::glue("#{getOption('xiff.label')}s={ncol(data.counts)}  #genes={nrow(data.counts)}")
  
  res <- list(
    data = pcadata,
    genes = nrow(data.counts),
    percentVar = percentVar,
    title = myTitle
  )
  res[[getOption("xiff.name")]] <- ncol(data.counts)
  res
}

calcTSNE <- function(TSNEData, geneSource, numGenes = 300, unit = "log2tpm", p = FALSE, ...) {
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
    inital_dims = min(50, numGenes),
    ...
  )
  myTitle <- glue::glue("perplexity={res$perplexity}  #{getOption('xiff.label')}s={res$N}  #genes={nrow(data.cross)}")

  final_res <- list(
    data = cbind(data.frame(res$Y), TSNEData[["assignment"]]),
    title = myTitle
  )
  final_res[[getOption("xiff.name")]] <- res$N
  final_res
}

calcUMAP <- function(UMAPData, geneSource, numGenes = 30, unit = "log2tpm", p = FALSE, ...){
  if (is.null(UMAPData)) return()
  
  progress <- ProcessProgress$new("UMAP", p)
  progress$update(0.3, "sorting genes by variance...")

  data.cross <- UMAPData[[paste0("data.", unit)]]
  maxVarRows <- orderByVariance(data.cross, geneSource, numGenes)
  data.cross <- data.cross[maxVarRows, ]

  progress$update(0.6, "calculating umap...")

  perp <- min(50, round(0.5 + ncol(data.cross)/10))
  res <- umap::umap(t(data.cross), perplexity = perp, ...)

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

calcPHATE <- function(PHATEdata, geneSource, numGenes = 30, unit = "log2tpm", p = FALSE, ...){
  
  
  if (is.null(PHATEdata)) return()
  
  progress <- ProcessProgress$new("PHATE", p)
  progress$update(0.3, "sorting genes by variance...")
  
  data.cross <- PHATEdata[[paste0("data.", unit)]]
  maxVarRows <- orderByVariance(data.cross, geneSource, numGenes)
  data.cross <- data.cross[maxVarRows, ]
  
  phate <- phateR::phate(t(data.cross), ...)
  
  myTitle <- glue::glue("#{getOption('xiff.label')}s={ncol(data.cross)}  #genes={nrow(data.cross)}")
  
  colname <- getOption("xiff.column")
  phateData <- phate$embedding %>%
    as.data.frame() %>%
    rename(X1 = PHATE1, X2 = PHATE2) %>% 
    tibble::rownames_to_column(colname)
  
  res <- list(
    data = left_join(phateData, PHATEdata[["assignment"]], by = colname),
    title = myTitle
  )
  res[[getOption("xiff.name")]] <- ncol(data.cross)
  res
  
}


#' Create Expression DimReduction.
#'
#' @param data result of the \code{getExpressionDimRedData} function.
#' @param anno sample annotation.
#' @param method  method for dimension reduction. 
#'        See: \code{dimRedAvailableMethods()} for supported methods.
#' @param clusterMethod clustering method.
#' @param geneSource a method used to obtain genes. If set to \code{varying_genes}
#' then the number of genes will be limited by the \code{numGenes} parameter. Other possible
#' value \code{gene_set} does not limit the number of genes taken into consideration. 
#' @param numGenes maximum number of most varying genes to be preserved if \code{geneSource}
#' equals \code{varying_genes}. Default 30.
#' @param unit sets the unit used for dimension reduction. Possible values: \code{log2tpm} (default)
#' or \code{counts}.
#' @param ... other parameters passed to dimension reduction functions.
#' @param .p progress type, logical or shiny session object
#'
#' @export
#' 
#' @return adds to sample annotation data additional columns:
#' 
#' \itemize{
#'  \item{"X1, X2"}{ - coordinates in 2D space based on the dimension reduction \code{method}}
#'  \item{"cluster"}{ - cluster assignment}
#' }
#' 
#' @examples
#' 
#' # example based on the CLIFF package
#' if(require("CLIFF")) {
#'   
#'   # note - data extraction is done by CLIFF functions
#'   setDbOptions(getSettings())
#'   ca <- CLIFF::exampleClassAssigment()
#'   data <- CLIFF::getExpressionDimRedData(ca, p = TRUE)
#'   anno <- CLIFF::getCellLineAnno("human")
#'   
#'   # data analysis is done by XIFF
#'   dimRedData <- getExpressionDimRed(data, anno)
#'   dimRedData %>% select(celllinename, X1, X2, class, cluster) %>% head
#'   
#'   generateExpressionDimRedPlot(dimRedData)
#' 
#' }
#'
getExpressionDimRed <- function(data,
                                anno,
                                method = XIFF::dimRedAvailableMethods(),
                                clusterMethod = c(
                                  "pam k = automatic", "pam k = 2", "pam k = 3",
                                  "pam k = 4", 
                                  "affinity propagation",
                                  "k-means k = automatic", "k-means k = 2",
                                  "k-means k = 3", "k-means k = 4"
                                ),
                                geneSource = c("varying_genes", "gene_set"),
                                numGenes   = 30,
                                unit = "log2tpm",
                                ...,
                                .p = TRUE) {
  
  
  method <- match.arg(method, XIFF::dimRedAvailableMethods())
  geneSource <- match.arg(geneSource, c("varying_genes", "gene_set"))
  clusterMethod <- clusterMethod[[1]]
  
  result <- switch(
    EXPR = method,
    tsne = {
      progressText <- "plot t-SNE"
      d <- calcTSNE(data, geneSource, numGenes, unit, p = .p, ...)
    },
    pca = {
      progressText <- "plot PCA"
      d <- calcPCA_expression(data, geneSource, numGenes, p = .p, ...)
    },
    umap = {
      progressText <- "plot umap"
      d <- calcUMAP(data, geneSource, numGenes, unit, p = .p, ...)
    },
    phate = {
      progressText <- "plot PHATE"
      d <- calcPHATE(data, geneSource, numGenes, unit, p = .p, ...)
    }
  )
  
  result$data <- addClustering(result$data, clusterMethod, p = .p)
  
  result$data <- left_join(result$data, anno, by = getOption("xiff.column"))
  
  resultTable <- result$data
  attr(resultTable, "title") <- result$title
  attr(resultTable, getOption("xiff.name")) <- result[[getOption("xiff.name")]]
  attr(resultTable, "progressText") <- progressText
  attr(resultTable, "method") <- method
  
  
  resultTable
}


#' @export
#' @rdname generateExpressionDimRedPlot
getExpressionDimRedPlot<- function(dimRedResult,
                                   colorCol = "class",
                                   labelCol = NULL,
                                   fontSize = 10) {
  
  .Deprecated("generateExpressionDimRedPlot")
  generateExpressionDimRedPlot(dimRedResult, colorCol, labelCol, fontSize)
  
}

#' Generate Expression Dimension Reduction Plot
#'
#' @param dimRedResult result of \code{getExpressionDimRed}
#' @param colorCol column name for coloring.
#' @param labelCol column name for labels.
#' @param fontSize font size for labels
#' 
#' @rdname generateExpressionDimRedPlot
#' @export
#' 
#' @export
#' 
#' @examples
#' 
#' # example based on the CLIFF package
#' if(require("CLIFF")) {
#'   
#'   # note - data extraction is done by CLIFF functions
#'   setDbOptions(getSettings())
#'   ca <- CLIFF::exampleClassAssigment()
#'   data <- CLIFF::getExpressionDimRedData(ca, p = TRUE)
#'   anno <- CLIFF::getCellLineAnno("human")
#'   
#'   # data analysis is done by XIFF
#'   dimRedData <- getExpressionDimRed(data, anno)
#'   generateExpressionDimRedPlot(dimRedData)
#'   
#'   geneExpr <- getCelllineDataGeneExpressionById("ENSG00000133703", ca) %>%
#'     mutate(log2tpm = log2(tpm))
#'   anno2 <- inner_join(anno, geneExpr, by = "celllinename")
#'   dimRedData <- getExpressionDimRed(data, anno2)
#'   generateExpressionDimRedPlot(dimRedData, colorCol = "cluster")
#'   
#'   # continous value
#'   generateExpressionDimRedPlot(dimRedData, colorCol = "log2tpm")
#' }
#' 
generateExpressionDimRedPlot <- function(dimRedResult,
                                    colorCol = "class",
                                    labelCol = NULL,
                                    fontSize = 10) {
  
  attrs <- attributes(dimRedResult)
  stopifnot(colorCol %in% colnames(dimRedResult))
  dimRedResult <- dimRedResult %>%
    mutate(class = !!rlang::sym(colorCol))
  
  if(!is.null(labelCol)) {
    stopifnot(labelCol %in% colnames(dimRedResult))
    showLabels <- TRUE
    dimRedResult <- dimRedResult %>%
      mutate(plotlabel =  !!rlang::sym(labelCol))
  } else {
    showLabels <- FALSE
    dimRedResult <- dimRedResult %>%
      mutate(plotlabel =  NA)
  }
  
  dt <- list(data = dimRedResult, attrs[[getOption("xiff.name")]])
  names(dt)[2] <- getOption("xiff.name")
  
  generateDimRedPlot(
    data = dt,
    progressText = attrs$progressText,
    colorCol = colorCol,
    showLabels = showLabels,
    fontSize = fontSize)$pl
}

