% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_ae_aedthdtc_aesdth.R
\name{check_ae_aedthdtc_aesdth}
\alias{check_ae_aedthdtc_aesdth}
\title{Check AEs with AEDTHDTC value but AESDTH not "Y"}
\usage{
check_ae_aedthdtc_aesdth(AE, preproc = identity, ...)
}
\arguments{
\item{AE}{Adverse Event SDTM dataset with variables USUBJID, AEDTHDTC,
AESDTH, AEDECOD, and AESTDTC}

\item{preproc}{An optional company specific preprocessing script}

\item{...}{Other arguments passed to methods}
}
\value{
Boolean value for whether the check passed or failed, with 'msg'
attribute if the check failed
}
\description{
This check looks for AE entries with an AEDTHDTC (death date)
value and AESDTH not equal to "Y"
}
\examples{


AE <- data.frame(USUBJID = c(1:5),
                 AEDTHDTC = c(rep("2020-01-01",3), "NA", NA),
                 AESDTH = c(rep("", 2), "Y", rep("", 2)),
                 AEDECOD = letters[1:5],
                 AESTDTC = "2020-01-01",
                 AESPID = "FORMNAME-R:13/L:13XXXX",
                 stringsAsFactors=FALSE)

check_ae_aedthdtc_aesdth(AE)
check_ae_aedthdtc_aesdth(AE,preproc=roche_derive_rave_row)

}
\author{
Shumei Chi
}
