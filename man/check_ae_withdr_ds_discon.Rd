% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_ae_withdr_ds_discon.R
\name{check_ae_withdr_ds_discon}
\alias{check_ae_withdr_ds_discon}
\title{Check if an AE leading to drug being withdrawn is reflected in DS}
\usage{
check_ae_withdr_ds_discon(AE, DS, TS, preproc = identity, ...)
}
\arguments{
\item{AE}{Adverse Events SDTM dataset with variables USUBJID, AEACN}

\item{DS}{Disposition SDTM dataset with variables USUBJID, DSCAT, DSSCAT}

\item{TS}{Trial Summary SDTM dataset with variables TSPARMCD, TSVAL}

\item{preproc}{An optional company specific preprocessing script}

\item{...}{Other arguments passed to methods}
}
\value{
boolean value if check failed or passed with 'msg' attribute if the
test failed
}
\description{
This checks that if there is an AE with AEACN="DRUG WITHDRAWN" then there should be a treatment discontinuation
record indicated by DS.DSSCAT
}
\examples{

AE <- data.frame(
 USUBJID = 1:6,
 AEACN = c("DRUG WITHDRAWN",NA,NA,NA,NA,NA),
 AETOXGR = c(NA,NA,NA,NA,"5",NA),
 AEDECOD=c("NAUSEA","HEADACHE"),
 AESPID = "FORMNAME-R:5/L:5XXXX"
)
DS <- data.frame(
 USUBJID = 1:3,
 DSCAT="DISPOSITION EVENT",
 DSSCAT="STUDY TREATMENT",
 DSDECOD=c("COMPLETED","ADVERSE EVENT","DEATH")
)

 TS <- data.frame(
 TSPARMCD="TRT",
 TSVAL="CHECK"
)

check_ae_withdr_ds_discon(AE,DS,TS)
check_ae_withdr_ds_discon(AE,DS,TS,preproc=roche_derive_rave_row)

DS$DSSCAT = NULL

check_ae_withdr_ds_discon(AE,DS,TS)

}
\author{
Yuliia Bahatska
}
