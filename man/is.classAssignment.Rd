% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classAssigment-helpers.R
\name{is.classAssignment}
\alias{is.classAssignment}
\alias{assertClassAssignment}
\title{Functions to check if an object is a classAssignment}
\usage{
is.classAssignment(x)

assertClassAssignment(x)
}
\arguments{
\item{x}{any R object}
}
\value{
logical value, TRUE if x is an classAssignment object
}
\description{
Functions to check if an object is a classAssignment
}
\details{
Note that this functions uses 'duck typing', so if an object 
can be used without any conversion as a \code{classAssignment}, then
the returned value is \code{TRUE}.
}
\examples{

is.classAssignment(list()) # FALSE
is.classAssignment(list(x = 1, y = 2)) # FALSE
is.classAssignment(classAssignment(list(x = 1, y = 2))) # TRUE - classAssignment object
is.classAssignment(list(class1 = 1, class2 = 2)) # TRUE - duck typing

}
