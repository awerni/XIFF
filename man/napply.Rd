% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{napply}
\alias{napply}
\title{mapply with vector names as argument}
\usage{
napply(X, FUN, ...)
}
\arguments{
\item{X}{named vector}

\item{FUN}{function with at least two parameters}

\item{...}{other arguments passed to mapply}
}
\value{
vector resulted from mapply
}
\description{
mapply with vector names as argument
}
\examples{

x <- setNames(1:3, c("A", "B", "C"))
napply(x, function(x, name) paste(name, x^2, sep = ": "))

}
