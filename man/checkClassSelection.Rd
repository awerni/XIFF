% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{checkClassSelection}
\alias{checkClassSelection}
\title{Check if both classes in classAssigment object are not empty}
\usage{
checkClassSelection(cs)
}
\arguments{
\item{cs}{classAssigment object}
}
\value{
TRUE if class1 and class2 are not empty, FALSE otherwise

TODO: check the intent
}
\description{
Check if both classes in classAssigment object are not empty
}
\examples{

ca <- exampleClassAssigment()
checkClassSelection(ca)
ca$class2 <- c()
checkClassSelection(ca)

}
