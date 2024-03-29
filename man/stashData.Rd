% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_utils.R
\name{stashData}
\alias{stashData}
\title{Stash data and use them in apps}
\usage{
stashData(df)
}
\arguments{
\item{df}{data.frame data to store}
}
\value{
character string, the dataset hash
}
\description{
For expert usage only. You have to have writing permissions to the DB.
Returned hash may be used in the Restore selection input mode.
}
\examples{
\dontrun{
setDbOptions(getSettings())
options(dbuser = Sys.getenv("USER"))
options(dbpass = NA)
df <- data.frame(celllinename = c("a", "b", "c"), property = c(1, 2, 3))
hash <- stashData(df)
df2 <- getStashedData(hash) # you can reach the data anywhere the DB is available
}
}
