% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/common_bioinfo.R
\name{selectBestFeaturesBoruta}
\alias{selectBestFeaturesBoruta}
\title{Use Bortua algorithm for feature selection.}
\usage{
selectBestFeaturesBoruta(
  df,
  threshold = c("Confirmed", "Tentative"),
  maxFeatures = Inf,
  threads = getOption("xiff.boruta.threads", 2),
  .otherParams = list()
)
}
\arguments{
\item{df}{}

\item{threshold}{}

\item{maxFeatures}{maximal number of features to be returned.}

\item{threads}{number of threads to be used by Boruta algorithm.}
}
\value{
list with data.frame containing only the selected variable
}
\description{
Use Bortua algorithm for feature selection.
}
\examples{

data("feature_selection_data", package = "XIFF")
featureFit <- selectBestFeaturesBoruta(feature_selection_data)
plot(featureFit$fit, las = 1, horizontal = TRUE)

}
