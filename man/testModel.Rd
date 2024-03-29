% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ml_utils.R
\name{testModel}
\alias{testModel}
\title{Test model on Test Data}
\usage{
testModel(
  model,
  testSet,
  anno,
  itemColumn = getOption("xiff.column"),
  classColumn = NULL
)
}
\arguments{
\item{model}{machine learning model created with XIFF}

\item{testSet}{test set (classAssignment object with)}

\item{anno}{items annotation data frame.}

\item{itemColumn}{for advanced users only. 
Name of the column which contains information about items.}

\item{classColumn}{for advanced users only. 
Name of the column which contains information about classes.}
}
\value{
MLModelTestsResults object.
}
\description{
Test model on Test Data
}
