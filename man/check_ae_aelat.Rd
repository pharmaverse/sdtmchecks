% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_ae_aelat.R
\name{check_ae_aelat}
\alias{check_ae_aelat}
\title{Check if AESOC has Eye, and Affected Eye is missing}
\usage{
check_ae_aelat(AE, preproc = identity, ...)
}
\arguments{
\item{AE}{Adverse Event Dataset for Ophtho Study with variables USUBJID, AELAT, AESOC,
AEDECOD, AETERM, AESTDTC (if present), AESPID (if present)}

\item{preproc}{An optional company specific preprocessing script}

\item{...}{Other arguments passed to methods}
}
\description{
This check looks if AESOC has Eye, and AELAT is missing.
}
\examples{

AE <- data.frame(
   USUBJID = 1:5,
   AESTDTC = 1:5,
   AELOC   = c("", "EYE", "eye", "", "EYE"),
   AELAT   = c("Left", "","left", "RIGHT", ""),
   AETERM  = c("A", "B", "A", "B", "A"),
   AEDECOD = c("A", "B", "A", "B", "A"),
   AESOC   = c("Eye", "Eye","Eye Disorder","Eye Disorder", "Eye"),
   AESPID  = "FORMNAME-R:19/L:19XXXX",
   stringsAsFactors = FALSE)

check_ae_aelat(AE)
check_ae_aelat(AE,preproc=roche_derive_rave_row)

AE <- data.frame(
   USUBJID = 1:5,
   AESTDTC = 1:5,
   AELAT   = c("Left", "","Bilateral", "", ""),
   AETERM  = c("A", "B", "A", "B", "A"),
   AEDECOD = c("A", "B", "A", "B", "A"),
   AESOC   = c("Eye", "Eye","Eye Disorder","Eye Disorder", "Eye"),
   stringsAsFactors = FALSE)

check_ae_aelat(AE)
check_ae_aelat(AE,preproc=roche_derive_rave_row)


}
\author{
Monarch Shah (HackR 2021 Team Eye)
}
