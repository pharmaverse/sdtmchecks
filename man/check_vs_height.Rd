% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_vs_height.R
\name{check_vs_height}
\alias{check_vs_height}
\title{Check for missing height values}
\usage{
check_vs_height(VS, DM)
}
\arguments{
\item{VS}{Vital Signs SDTM dataset with variables USUBJID,VSTEST,VSTESTCD,VSSTRESN,VISIT}

\item{DM}{Demographics SDTM dataset with variable USUBJID}
}
\value{
boolean value if check failed or passed with 'msg' attribute if the
test failed
}
\description{
This check looks for both records where height is missing
as well as DM patients with no height records at all
}
\examples{

DM <- data.frame(
 STUDYID = 1,
 USUBJID = 1:10
)

VS <- data.frame(
 STUDYID = 1,
 USUBJID = 1:10,
 VSTEST = "HEIGHT",
 VSTESTCD = "HEIGHT",
 VSSTRESN = 1:10,
 VISIT = 1:10
)

check_vs_height(VS,DM)

DM <- data.frame(
 STUDYID = 1,
 USUBJID = 1:11
)

VS$VSSTRESN[1] = NA
VS$VSSTRESN[2] = "NA"
VS$VSSTRESN[3] = ""
VS$VSSTRESN[4] = "."

check_vs_height(VS,DM)


}
\author{
Sara Bodach
}
