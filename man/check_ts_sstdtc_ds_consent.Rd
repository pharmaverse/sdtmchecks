% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_ts_sstdtc_ds_consent.R
\name{check_ts_sstdtc_ds_consent}
\alias{check_ts_sstdtc_ds_consent}
\title{Check for missing SSTDTC (Study Start Date) in TS}
\usage{
check_ts_sstdtc_ds_consent(DS, TS)
}
\arguments{
\item{DS}{Disposition SDTM dataset with variables DSCAT, DSSCAT, DSDECOD, DSSTDTC}

\item{TS}{Trial Summary SDTM dataset with variables TSPARMCD, TSPARM, TSVAL}
}
\value{
boolean value if check failed or passed with 'msg' attribute if the
test failed
}
\description{
This check looks for missing SSTDTC (Study Start Date) in TS;
if it's present, check that the date matches the earliest informed consent among
any subject enrolled in the study. The FDA Technical Rejection Criteria for
Study Data - effective September 15, 2021 requires Study Start Date
(https://www.fda.gov/media/100743/download). If missing, no data queries are
needed - this would be updating the assignment in the TS domain.
}
\examples{

TS1 <- data.frame(
 STUDYID = 1,
 TSPARMCD = "SSTDTC",
 TSPARM = "Study Start Date",
 TSVAL = "2017-01-01",
 TSVAL1 = "",
 TSVAL2 = ""
)

TS2 <- data.frame(
 STUDYID = 2,
 TSPARMCD = "AEDICT",
 TSPARM = "Study Start Date",
 TSVAL = "MedDRA v23.0",
 TSVAL1 = ""
)

TS3 <- data.frame(
 STUDYID = 3,
 TSPARMCD = "SSTDTC",
 TSPARM = "Study Start Date",
 TSVAL = ""
)

TS4 <- data.frame(
 STUDYID = 1,
 TSPARMCD = "SSTDTC",
 TSPARM = "Study Start Date",
 TSVAL = "2020-01-02",
 TSVAL1 = "",
 TSVAL2 = ""
)

TS5 = rbind(TS1, TS4)

TS6 <- data.frame(
 STUDYID = 1,
 TSPARMCD = "SSTDTC",
 TSPARM = "Study Start Date",
 TSVAL = "2020-01",
 TSVAL1 = "",
 TSVAL2 = ""
)

DS1 <- data.frame(
 USUBJID = c(1,1,2,3,4),
 DSCAT   = rep("PROTOCOL MILESTONE", 5),
 DSSCAT   = rep("PROTOCOL MILESTONE", 5),
 DSDECOD = c("INFORMED CONSENT OBTAINED", "OTHER", "PHYSICIAN DECISION",
             "OTHER", "INFORMED CONSENT OBTAINED"),
 DSSTDTC = c("2021-01-01", "2021-01-02", "2021-01-02", "2021-01-02", "2020-01-02"),
 stringsAsFactors = FALSE
)

check_ts_sstdtc_ds_consent(DS=DS1, TS=TS1)
check_ts_sstdtc_ds_consent(DS=DS1, TS=TS2)
check_ts_sstdtc_ds_consent(DS=DS1, TS=TS3)
check_ts_sstdtc_ds_consent(DS=DS1, TS=TS4)
check_ts_sstdtc_ds_consent(DS=DS1, TS=TS5)
check_ts_sstdtc_ds_consent(DS=DS1, TS=TS6)

}
\author{
Sara Bodach
}
