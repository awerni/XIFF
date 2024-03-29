% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_utils.R
\name{getStashedData}
\alias{getStashedData}
\title{Get Stashed Data from database}
\usage{
getStashedData(hash)
}
\arguments{
\item{hash}{6-character long hash of stashed data}
}
\value{
data.frame with stashed data or NULL if there's no data for given 
hash
}
\description{
Get Stashed Data from database
}
\examples{

if(require("CLIFF")) {
  CLIFF::setDbOptions()
  head(getStashedData("000000"))
}

}
