% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_bioinfo.R
\name{getGSEACollection}
\alias{getGSEACollection}
\title{Get GSEA Collection}
\usage{
getGSEACollection(species = "human")
}
\arguments{
\item{species}{name of the species}
}
\value{
named vector with GSEA Collections
}
\description{
Get GSEA Collection
}
\examples{

if(require("CLIFF")) {
  CLIFF::setDbOptions()
  getGSEACollection()
}

}
