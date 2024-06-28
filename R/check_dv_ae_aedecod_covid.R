#' @title Check for consistency between DV and AE for COVID-19 events
#'
#' @description If a patient has a DV record indicating COVID-19 then they 
#' should also have COVID-related AE where AE.AEDECOD matches covid.REFTERM.
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AEDECOD
#' @param DV Protocol Deviation SDTM dataset with variables USUBJID, DVREAS
#' @param covid_terms A length >=1 vector of AE terms identifying COVID-19 (case does not matter)
#'
#' @return boolean value if check returns 0 obs, otherwise return subset dataframe.
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#'
#' @author Natalie Springfield
#' 
#' @family COVID
#' 
#' @keywords COVID
#'
#' @examples
#' 
#' AE <- data.frame(
#'  USUBJID = 1:6,
#'  AEDECOD = c("pandemic", "covid-19","some AE","some AE","CORONAVIRUS POSITIVE","UNMAPPED")
#' )
#'
#' DV <- data.frame(
#'  USUBJID = 1:6,
#'  DVREAS=c("SUSPECTED EPIDEMIC/PANDEMIC INFECTION",
#'           "UNKNOWN",
#'           "SUSPECTED EPIDEMIC/PANDEMIC INFECTION",
#'           "OTHER",
#'           "SUSPECTED EPIDEMIC/PANDEMIC INFECTION",
#'           "SUSPECTED EPIDEMIC/PANDEMIC INFECTION")
#' )
#'
#' check_dv_ae_aedecod_covid(AE,DV)
#'
#'
#' # Pass specific covid terms
#' 
#' check_dv_ae_aedecod_covid(AE,DV,covid_terms=c("COVID-19", "CORONAVIRUS POSITIVE","PANDEMIC"))
#'



check_dv_ae_aedecod_covid <- function(AE,DV,covid_terms=c("COVID-19", "CORONAVIRUS POSITIVE")){

    if(is.null(covid_terms)|
       (!is.null(covid_terms) & !is.character(covid_terms))|
       (!is.null(covid_terms) & is.character(covid_terms) & length(covid_terms)<1)|
       (!is.null(covid_terms) & is.character(covid_terms) & length(covid_terms)>=1 & all(is_sas_na(covid_terms)))
    ){
        
        fail("Check not run, did not detect COVID-19 preferred terms.  Character vector of terms expected.") 
        
    }else if( AE %lacks_any% c("USUBJID","AEDECOD")){

        fail(lacks_msg(AE, c("USUBJID","AEDECOD")))

    }else if( DV %lacks_any% c("USUBJID","DVREAS")){

        fail(lacks_msg(DV, c("USUBJID","DVREAS")))

    } else{
        
        #let used know terms used
        if(identical(covid_terms,c("COVID-19", "CORONAVIRUS POSITIVE"))){
            outmsg=paste("Default terms used for identifying Covid AEs:",paste(covid_terms,collapse=","))
        }else{
            outmsg=""
        }

        # Select AE recs where uppercased AE.AEDECOD matches COVID-related terms COVID_AE.AEDECOD
        ae0 <- AE %>%
            filter(toupper(AEDECOD) %in% toupper(covid_terms)) %>% 
            select(USUBJID,AEDECOD)


        # Select DV recs where DVREAS='CONFIRMED OR SUSPECTED EPIDEMIC/PANDEMIC INFECTION'
        dv0 <- subset(DV, grepl('SUSPECTED EPIDEMIC/PANDEMIC INFECTION',toupper(DV$DVREAS)),
                      select=c("USUBJID", "DVREAS" ))


        # Keep patients in DV who lack AE record
        mydfprep <- merge(unique(dv0), unique(ae0), by="USUBJID" , all.x=TRUE)
        mydf <- subset(mydfprep, is_sas_na(mydfprep$AEDECOD))

        ### Return pass if all records with COVID-related DV.DVREAS had corresponding AE.AEDECOD
        if(nrow(mydf)==0){
            pass()
            ### Return subset dataframe/message if there are records with COVID-related DV.DVREAS
            ### but no AE records
        }else if(nrow(mydf)>0){
            fail( paste("Found", length(unique(mydf$USUBJID)),
                         "patient(s) with COVID-related Protocol Deviation, but no AE record with COVID terms. ",
                        outmsg
                        ),
                  mydf
            )

        }
    }
}
