#' @title Check for COVID-19 AE leading to Study Discon without DS Study Discon
#'
#' @description Flag if patient has a COVID-19 AE where AE.AEDECOD 
#' matches a COVID-19 preferred term event action of Study Discontinuation 
#' (AE.AEACNOT* includes "DISCONTINUED FROM STUDY") but missing Study Discontinuation
#' record in DS (DS.DSSCAT includes "STUDY" and "DISCON" and excludes "DRUG" and 
#' "TREATMENT")
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AEDECOD, 
#' AEACNOT* (can be multiple variables)
#' @param DS Disposition SDTM dataset with variables USUBJID, DSSCAT, DSDECOD
#' @param covid_df dataframe of AE terms identify COVID-19, contains variable REFTERM
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select rename semi_join mutate distinct
#'
#' @author Natalie Springfield
#' 
#' @family COVID
#' 
#' @keywords COVID
#'
#' @examples
#' 
#' covid_df = data.frame(REFTERM = c("COVID-19",
#'                                   "CORONAVIRUS POSITIVE"
#'                                   )
#'                      )
#'
#' AE <- data.frame(
#'  USUBJID = 1:5,
#'  AEDECOD = c("covid-19", "covid-19", "covid-19","Some AE", "CORONAVIRUS POSITIVE" ),
#'  AEACNOTH=c("SUBJECT DISCONTINUED FROM STUDY",
#'             "NONE",
#'             "NONE", 
#'             "SUBJECT DISCONTINUED FROM STUDY",
#'             "NONE"),
#'  AEACNOTH1=c("SUBJECT DISCONTINUED FROM STUDY", 
#'              "NONE",
#'              "SUBJECT DISCONTINUED FROM STUDY",
#'              "NONE", 
#'              "SUBJECT DISCONTINUED FROM STUDY"),
#'  AEACNOTH2=c("SUBJECT DISCONTINUED FROM STUDY", 
#'              "NONE",
#'              "NONE",
#'              "SUBJECT DISCONTINUED FROM STUDY",
#'              "NONE")
#' )
#'
#' DS <- data.frame(
#'  USUBJID = 1:3,
#'  DSSCAT=c("TREATMENT DISCONTINUATION", 
#'  "STUDY DISCONTINUATION",
#'  "STUDY DISCONTINUATION"),
#'  DSDECOD="DISCON REASON"
#' )
#'
#' check_ae_aeacnoth_ds_stddisc_covid(AE,DS,covid_df = covid_df)
#'

check_ae_aeacnoth_ds_stddisc_covid <- function(AE,DS,covid_df = NULL) {

    if(is.null(covid_df)){
        
        message("check_ae_aeacnoth_ds_stddisc_covid: Check not run, did not detect COVID-19 preferred terms")
        fail("Check not run, did not detect COVID-19 preferred terms") 
        
    }else if( AE %lacks_any% c("USUBJID","AEDECOD","AEACNOTH")) {

        fail(lacks_msg(AE, c("USUBJID","AEDECOD","AEACNOTH")))

    }else if( DS %lacks_any% c("USUBJID", "DSSCAT", "DSDECOD")) {

        fail(lacks_msg(DS, c("USUBJID", "DSSCAT","DSDECOD")))

    } else{

        # Select unique COVID AE TERMS from COVID.RData
        # apply uppercasing
        covid_ae <- covid_df %>%
            mutate(REFTERM=toupper(REFTERM)) %>%
            select(REFTERM) %>%
            distinct(REFTERM) %>%
            rename(AEDECOD=REFTERM)

        attr(covid_ae$AEDECOD, "label") <- "Dictionary-Derived Term"
        #attributes(covid_ae$AEDECOD)

        # Select AE records where AE.AEDECOD matches COVID-related terms COVID_AE.AEDECOD
        # apply uppercasing
        attr(AE$AEDECOD, "label") <- "Dictionary-Derived Term"
        ae0 <- AE %>%
            mutate(AEDECOD=toupper(AEDECOD)) %>%
            semi_join(covid_ae, by = "AEDECOD")

        #attributes(ae0$AEDECOD)

        # Select column variables matching AEACNOT* (i.e. AEACNOTH, AEACNOT*1, AEACNOT*2...)
        aeacnoth_colnm<-colnames(ae0)[which(startsWith(colnames(ae0),"AEACNOT"))]

        # Select AE recs leading to STUDY DISCON use variables AEACNOT*
        ae1<-ae0[Reduce('|',lapply(ae0[aeacnoth_colnm], grepl, pattern="DISCONTINUED FROM STUDY" )), ]


        # Select DS recs with STUDY DISCON
        ds0 <- subset(DS, (grepl("STUDY", toupper(DS$DSSCAT)) &
                               grepl("DISCON", toupper(DS$DSSCAT)) &
                               !grepl("DRUG", toupper(DS$DSSCAT)) &
                               !grepl("TREATMENT", toupper(DS$DSSCAT)) ),
                      select=c("USUBJID", "DSDECOD", "DSSCAT" ))


        # keep patients in AE who lack DS record
        mydfprep<- merge(ae1, unique(ds0), "USUBJID", all.x=TRUE )
        mydf <- subset(mydfprep, is_sas_na(mydfprep$DSSCAT),
                       select=c("USUBJID", "AEDECOD", "DSDECOD", "DSSCAT", aeacnoth_colnm))


        ### Return pass if all COVID-related AE recs leading to STUDY discon had corresponding DISCON rec
        if(nrow(mydf)==0){
            pass()
            ### Otherwise return subset dataframe/message
        }else if(nrow(mydf)>0){
            fail((paste("Found", length(unique(mydf$USUBJID)),
                        "patient(s) with COVID-related AE(s) leading to Study Discon, but no corresponding Study Discon in DS. ")),
                 mydf)
        }
    }
}
