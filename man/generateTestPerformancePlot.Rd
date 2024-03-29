% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/machine_learning_plots.R
\name{generateTestPerformancePlot}
\alias{generateTestPerformancePlot}
\title{Generate Apply Performance Plot}
\usage{
generateTestPerformancePlot(df)
}
\arguments{
\item{df}{MLModelTestsResult object or 
the result of \code{generateTestPerformanceData}}
}
\value{
ggplot object
}
\description{
Generate Apply Performance Plot
}
\examples{

confMatrix <- as.table(matrix(
  c(19L, 9L, 2L, 66L),
  nrow = 2,
  dimnames = list(
    Prediction = c("positive", "negative"),
    Reference =  c("positive", "negative")
  )
))
df <- generateTestPerformanceData(confMatrix)
generateTestPerformancePlot(df)

}
