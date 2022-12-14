% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_ae_aeacnoth_ds_stddisc_covid.R
\name{check_ae_aeacnoth_ds_stddisc_covid}
\alias{check_ae_aeacnoth_ds_stddisc_covid}
\title{Check if patient with COVID-related AE leading to STUDY discon
and also has STUDY Discon record in DS}
\usage{
check_ae_aeacnoth_ds_stddisc_covid(AE, DS, covid_df = NULL)
}
\arguments{
\item{AE}{Adverse Events SDTM dataset with variables USUBJID,AEDECOD,AEACNOT* (can be multiple vars)}

\item{DS}{Discon SDTM dataset with variables USUBJID,DSSCAT,DSDECOD}

\item{covid_df}{Dataframe of AE terms identify covid, contains variable REFTERM}
}
\value{
boolean value if check failed or passed with 'msg' attribute if the
test failed
}
\description{
Flag if patient has a record with COVID-related AE where AE.AEDECOD matches covid.REFTERM
and leads to STUDY discon
(AE.AEACNOT*=SUBJECT DISCONTINUED FROM STUDY) but has no STUDY Discon record in DS
(DS.DSSCAT=STUDY DISCONTINUATION)
}
\examples{


covid_df = data.frame(REFTERM = c("COVID-19",
                                  "CORONAVIRUS POSITIVE"
                                  )
                     )

AE <- data.frame(
 USUBJID = 1:5,
 AEDECOD = c("covid-19", "covid-19", "covid-19","Some AE", "CORONAVIRUS POSITIVE" ),
 AEACNOTH=c("SUBJECT DISCONTINUED FROM STUDY",
            "NONE",
            "NONE", 
            "SUBJECT DISCONTINUED FROM STUDY",
            "NONE"),
 AEACNOTH1=c("SUBJECT DISCONTINUED FROM STUDY", 
             "NONE",
             "SUBJECT DISCONTINUED FROM STUDY",
             "NONE", 
             "SUBJECT DISCONTINUED FROM STUDY"),
 AEACNOTH2=c("SUBJECT DISCONTINUED FROM STUDY", 
             "NONE",
             "NONE",
             "SUBJECT DISCONTINUED FROM STUDY",
             "NONE")
)

DS <- data.frame(
 USUBJID = 1:3,
 DSSCAT=c("TREATMENT DISCONTINUATION","STUDY DISCONTINUATION","STUDY DISCONTINUATION"),
 DSDECOD="DISCON REASON"
)

check_ae_aeacnoth_ds_stddisc_covid(AE,DS,covid_df = covid_df)

}
\author{
Natalie Springfield
}
