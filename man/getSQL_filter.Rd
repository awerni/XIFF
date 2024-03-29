% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_utils.R
\name{getSQL_filter}
\alias{getSQL_filter}
\title{Create SQL Where filter}
\usage{
getSQL_filter(filter_col, filter_options)
}
\arguments{
\item{filter_col}{name of the column}

\item{filter_options}{vector of elements to be but in the 'IN' expression}
}
\value{
null if filter_options is empty or SQL 
string with part of sql where expression.
}
\description{
Create SQL Where filter
}
\examples{

getSQL_filter("column", c("A", "B"))
getSQL_filter("column", NULL)

}
