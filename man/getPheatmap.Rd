% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_utils.R
\name{getPheatmap}
\alias{getPheatmap}
\title{Wrapper for pheatmap function}
\usage{
getPheatmap(
  mat,
  ...,
  silent = TRUE,
  show_rownames = nrow(mat) < 100,
  show_colnames = ncol(mat) < 100
)
}
\arguments{
\item{mat}{numeric matrix of the values to be plotted.}

\item{...}{other arguments passed to pheatmap}

\item{silent}{do not draw the plot (useful when using the gtable output)}

\item{show_rownames}{boolean specifying if column names are be shown.}

\item{show_colnames}{boolean specifying if column names are be shown.}
}
\description{
Wrapper for pheatmap function
}
\details{
restores default graphical device after calling pheatmap function
}
