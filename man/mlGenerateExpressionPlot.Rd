% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/machine_learning.R
\name{mlGenerateExpressionPlot}
\alias{mlGenerateExpressionPlot}
\title{Generate Expression Plot for Machine Learning Model}
\usage{
mlGenerateExpressionPlot(model, df, ca, plotType = "point", gene)
}
\arguments{
\item{model}{MLXIFF object}

\item{df}{result of \code{mlGetTpmData}}

\item{ca}{classAssigment object}

\item{plotType}{plot type}

\item{gene}{list with gene symbol and ensg}
}
\value{
ggplot2
}
\description{
Generate Expression Plot for Machine Learning Model
}
\details{
this plot is mostly used in the ShinyApplication
}
\examples{

\dontrun{

df <- mlGetTpmData(model, "ENSG00000147889", annoFocus)
gene <- list(symbol = getGeneSymbol("ENSG00000147889"), ensg = "ENSG00000147889")
mlGenerateExpressionPlot(model, df, trainingSet, gene = gene)
}

}
