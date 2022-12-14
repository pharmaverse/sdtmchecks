% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is_sas_na}
\alias{is_sas_na}
\title{Check if the elements could be of any SAS missing data variant}
\usage{
is_sas_na(x)
}
\arguments{
\item{x}{vector with data}
}
\value{
logical vector
}
\description{
Missing Data from sas7bdat imported datesets could be imported in different
ways we currently check for 'NA', NA, '.', and ''.
}
\examples{
is_sas_na(c(1,2,NA))

is_sas_na(c("a", NA, "NA", ""))
}
\keyword{internal}
