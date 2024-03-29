% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering.R
\name{getExpressionDimRedPlot}
\alias{getExpressionDimRedPlot}
\alias{generateExpressionDimRedPlot}
\title{Generate Expression Dimension Reduction Plot}
\usage{
getExpressionDimRedPlot(
  dimRedResult,
  colorCol = "class",
  labelCol = NULL,
  fontSize = 10
)

generateExpressionDimRedPlot(
  dimRedResult,
  colorCol = "class",
  labelCol = NULL,
  fontSize = 10
)
}
\arguments{
\item{dimRedResult}{result of \code{getExpressionDimRed}}

\item{colorCol}{column name for coloring.}

\item{labelCol}{column name for labels.}

\item{fontSize}{font size for labels}
}
\description{
Generate Expression Dimension Reduction Plot
}
\examples{

# example based on the CLIFF package
if(require("CLIFF")) {
  
  # note - data extraction is done by CLIFF functions
  setDbOptions(getSettings())
  ca <- CLIFF::exampleClassAssigment()
  data <- CLIFF::getExpressionDimRedData(ca, p = TRUE)
  anno <- CLIFF::getCellLineAnno("human")
  
  # data analysis is done by XIFF
  dimRedData <- getExpressionDimRed(data, anno)
  generateExpressionDimRedPlot(dimRedData)
  
  geneExpr <- getCelllineDataGeneExpressionById("ENSG00000133703", ca) \%>\%
    mutate(log2tpm = log2(tpm))
  anno2 <- inner_join(anno, geneExpr, by = "celllinename")
  dimRedData <- getExpressionDimRed(data, anno2)
  generateExpressionDimRedPlot(dimRedData, colorCol = "cluster")
  
  # continous value
  generateExpressionDimRedPlot(dimRedData, colorCol = "log2tpm")
}

}
