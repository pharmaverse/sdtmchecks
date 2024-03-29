% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_ce_missing_month.R
\name{check_ce_missing_month}
\alias{check_ce_missing_month}
\title{Check for clinical events dates with year and day known but month unknown}
\usage{
check_ce_missing_month(CE, preproc = identity, ...)
}
\arguments{
\item{CE}{Clinical Events SDTM dataset with variables USUBJID, CETERM, and at
least one of the following date variables: CESTDTC, CEENDTC, CEDTC}

\item{preproc}{An optional company specific preprocessing script}

\item{...}{Other arguments passed to methods}
}
\value{
boolean value if check failed or passed with 'msg' attribute if the
test failed
}
\description{
Check for missing month when clinical events dates
(CESTDTC, CEENDTC, CEDTC) have a known year and day
}
\examples{

CE <- data.frame(
 USUBJID = c(1, 2, 3, 4),
 CETERM = c("Headache", "Nausea", "Dizziness", "Fever"),
 CESTDTC = c("2023---01", "2023-01-15", "2023-02-01", "2023-02-10"),
 CEENDTC = c("2023-01-02", "2023---01", "2023-02-02", "2023-02-12"),
 CEDTC = c("2023--01", "", "", ""),
 CESEV = c("Mild", "Moderate", "Mild", "Severe"),
 CESPID = "FORMNAME-R:13/L:13XXXX",
 stringsAsFactors=FALSE
 )

check_ce_missing_month(CE)
check_ce_missing_month(CE,preproc=roche_derive_rave_row)

CE <- data.frame(
 USUBJID = c(1, 2, 3, 4),
 CETERM = c("Headache", "Nausea", "Dizziness", "Fever"),
 CESTDTC = c("2023-01-01", "2023-01-15", "2023-02-01", "2023-02-10"),
 CEENDTC = c("2023-01-02", "2023-01-16", "2023-02-02", "2023-02-12"),
 CEENDTC = "",
 CESEV = c("Mild", "Moderate", "Mild", "Severe"),
 CESPID = "FORMNAME-R:13/L:13XXXX",
 stringsAsFactors=FALSE
 )

check_ce_missing_month(CE)

CE$CETERM = NULL

check_ce_missing_month(CE)

}
\author{
Ryan Marinelli
}
