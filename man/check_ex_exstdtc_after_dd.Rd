% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_ex_exstdtc_after_dd.R
\name{check_ex_exstdtc_after_dd}
\alias{check_ex_exstdtc_after_dd}
\title{Check for EX dates occurring after death date}
\usage{
check_ex_exstdtc_after_dd(AE, DS, EX)
}
\arguments{
\item{AE}{Adverse Event SDTM dataset with variables USUBJID,
AEDTHDTC, AESTDTC, AEDECOD, and AETERM}

\item{DS}{Disposition SDTM dataset with variables USUBJID,
DSSTDTC, DSDECOD, and DSTERM}

\item{EX}{Exposure SDTM dataset with variables USUBJID,
EXSTDTC, EXTRT, and EXDOSE}
}
\value{
Boolean value for whether the check passed or failed, with 'msg'
attribute if the check failed
}
\description{
This check looks for EX dates that occur after death date
}
\examples{

AE <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
                 AEDTHDTC = c(rep("", 4), "2016-01-01"),
                 AESTDTC = rep("2016-01-01", 5),
                 AEDECOD = LETTERS[1:5], AETERM = LETTERS[1:5],
                 stringsAsFactors = FALSE)

DS <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
                 DSSTDTC = rep("2016-01-02", 5),
                 DSDECOD = c(LETTERS[1:4], "death"),
                 DSTERM = letters[1:5],
                 stringsAsFactors = FALSE)

EX <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
                 EXSTDTC = rep("2015-12-31", 5),
                 EXTRT = LETTERS[1:5],
                 EXDOSE = 1:5,
                 stringsAsFactors = FALSE)

check_ex_exstdtc_after_dd(AE, DS, EX)

EX$EXSTDTC[1] <- "2016-01-03"
EX$USUBJID[1] <- EX$USUBJID[5]

check_ex_exstdtc_after_dd(AE, DS, EX)


}
\author{
Nina Ting Qi
}
