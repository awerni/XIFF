% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classAssigment-helpers.R
\name{classAssignment2df}
\alias{classAssignment2df}
\title{Convert classAssigment to data.frame containing class assignment column}
\usage{
classAssignment2df(ca)
}
\arguments{
\item{ca}{classAssigment object}
}
\value{
data.frame
}
\description{
Convert classAssigment to data.frame containing class assignment column
}
\examples{

ca <- classAssignment(cl1 = 1:5, cl2 = 6:10)
classAssignment2df(ca)

}
