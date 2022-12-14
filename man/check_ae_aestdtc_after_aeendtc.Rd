% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_ae_aestdtc_after_aeendtc.R
\name{check_ae_aestdtc_after_aeendtc}
\alias{check_ae_aestdtc_after_aeendtc}
\title{Check that all AE start dates are on or before AE end dates}
\usage{
check_ae_aestdtc_after_aeendtc(AE, preproc = identity, ...)
}
\arguments{
\item{AE}{Adverse Event SDTM dataset with variables USUBJID,AETERM,AEDECOD,AESTDTC,AEENDTC}

\item{preproc}{An optional company specific preprocessing script}

\item{...}{Other arguments passed to methods}
}
\value{
boolean value if check failed or passed with 'msg' attribute if the
test failed
}
\description{
This check identifies AESTDTC values that are after AEENDTC values
}
\examples{

AE <- data.frame(
 USUBJID = 1:12,
 AETERM = "SOME AE TERM",
 AEDECOD = "SOME AE PT",
 AESTDTC = c("2017-01-01","2017-01-03","2017-01-01T14:26","2017","2017-02","2017"      ,""    ,
             "2017"   ,"2017-01-01T14:26","2017-01-01T14:26","2017-01-01T14","2017-01-01T14:26:02")
             ,
 AEENDTC = c("2017-01-01","2017-01-02","2017-01-01T14:25","2015","2017-01","2016-01-01",
             "2000","2017-02","2017-01-01"      ,"2017-01","2017-01-01T13","2017-01-01T14:26:01")
             ,
 AESPID = "FORMNAME-R:19/L:19XXXX",
 stringsAsFactors=FALSE
)

check_ae_aestdtc_after_aeendtc(AE)
check_ae_aestdtc_after_aeendtc(AE,preproc=roche_derive_rave_row)

AE$AETERM <- NULL
check_ae_aestdtc_after_aeendtc(AE)

}
\author{
Sara Bodach
}
