% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_ds_sc_strat.R
\name{check_ds_sc_strat}
\alias{check_ds_sc_strat}
\title{Check if randomized patients are missing stratification factor data}
\usage{
check_ds_sc_strat(DS, SC)
}
\arguments{
\item{DS}{Subject Dispostion Dataset with variable USUBJID, DSDECOD, DSSTDTC}

\item{SC}{Subject Characteristics Dataset with variables USUBJID, SCTEST, SCTESTCD, SCCAT, SCORRES}
}
\value{
boolean value if check failed or passed with 'msg' attribute if the test failed
}
\description{
Check if Study is randomized (DS.DSDECOD == "RANDOMIZED" or "RANDOMIZATION"), and subject Characteristics Domain (SC) has no stratification factors reported.
}
\examples{

ds <- data.frame(USUBJID = c(1,2,2),
                 DSDECOD = c("RANDOMIZATION", "RANDOMIZED", "Randomized"),
                 DSSTDTC = c("2021-01-01", "2021-01-02", "2021-02-01"))
sc <- data.frame(USUBJID  = c(1,1,1,2,2,2),
                 SCCAT = rep("STRATIFICATION", 6),
                 SCTESTCD = c("STRAT 1", "STRAT 2", "STRAT 3", "STRAT 1", "STRAT 2", "STRAT 3"),
                 SCTEST   = c("Factor 1", "Factor 2", "Factor 3",
                              "Factor 1", "Factor 2", "Factor 3"),
                 SCORRES  = c("US", "Left", "Score > x", "RoW", "Right", "Score < x"),
                 stringsAsFactors = FALSE)

check_ds_sc_strat(ds, sc)

ds <- data.frame(USUBJID = c(1,2,2),
                 DSDECOD = c("RANDOMIZATION", "RANDOMIZED", "Randomized"),
                 DSSTDTC = c("2021-01-01", "2021-01-02", "2021-02-01"))
sc <- data.frame(USUBJID  = c(1,1,1),
                 SCCAT = rep("STRATIFICATION", 3),
                 SCTESTCD = c("STRAT 1", "STRAT 2", "STRAT 3"),
                 SCTEST   = c("Factor 1", "Factor 2", "Factor 3"),
                 SCORRES  = c("US", "Left", NA),
                 stringsAsFactors = FALSE)

check_ds_sc_strat(ds, sc)

ds <- data.frame(USUBJID = c(1,2),
                 DSDECOD = c("Open Label", "Open Label"),
                 DSSTDTC = c("2021-01-01", "2021-01-02"))
sc <- data.frame(USUBJID  = c(1,1,1,2,2,2),
                 SCCAT = rep("No STRATIFICATION", 6),
                 SCTESTCD = c("STRAT 1", "STRAT 2", "STRAT 3", "STRAT 1", "STRAT 2", "STRAT 3"),
                 SCTEST   = c("Factor 1", "Factor 2", "Factor 3",
                              "Factor 1", "Factor 2", "Factor 3"),
                 SCORRES  = c("US", "Left", NA, "RoW", "Right", "Score < x"),
                 stringsAsFactors = FALSE)

check_ds_sc_strat(ds, sc)


}
\author{
Monarch Shah
}
