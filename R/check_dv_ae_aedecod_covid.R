#' @title Check for consistency between DV and AE for COVID-19 events
#'
#' @description If a patient has a DV record indicating COVID-19 then they 
#' should also have COVID-related AE where AE.AEDECOD matches covid.REFTERM.
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AEDECOD
#' @param DV Protocol Deviation SDTM dataset with variables USUBJID, DVREAS
#' @param covid_df Dataframe of AE terms identify COVID-19, contains variable REFTERM
#'
#' @return boolean value if check returns 0 obs, otherwise return subset dataframe.
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
#'  USUBJID = 1:6,
#'  AEDECOD = c("covid-19", "covid-19","some AE","some AE","CORONAVIRUS POSITIVE","UNMAPPED")
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
#' check_dv_ae_aedecod_covid(AE,DV,covid_df)
#'



check_dv_ae_aedecod_covid <- function(AE,DV,covid_df=NULL){

    if(is.null(covid_df)){
        
        message("check_dv_ae_aedecod_covid: Check not run, did not detect COVID-19 preferred terms")
        fail("Check not run, did not detect COVID-19 preferred terms") 
        
    }else if( AE %lacks_any% c("USUBJID","AEDECOD")){

        fail(lacks_msg(AE, c("USUBJID","AEDECOD")))

    }else if( DV %lacks_any% c("USUBJID","DVREAS")){

        fail(lacks_msg(DV, c("USUBJID","DVREAS")))

    } else{

        # Select unique uppercased COVID AE TERMS from COVID.RData
        covid_ae <- covid_df %>%
            mutate(REFTERM=toupper(REFTERM)) %>%
            select(REFTERM) %>%
            distinct(REFTERM) %>%
            rename(AEDECOD=REFTERM)

        attr(covid_ae$AEDECOD, "label") <- "Dictionary-Derived Term"
        #attributes(covid_ae$AEDECOD)

        # Select AE recs where uppercased AE.AEDECOD matches COVID-related terms COVID_AE.AEDECOD
        attr(AE$AEDECOD, "label") <- "Dictionary-Derived Term"
        ae0 <- AE %>%
            select(USUBJID,AEDECOD) %>%
            mutate(AEDECOD=toupper(AEDECOD)) %>%
            semi_join(covid_ae, by = "AEDECOD")


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
            fail( (paste("Found", length(unique(mydf$USUBJID)),
                         "patient(s) with COVID-related Protocol Deviation, but no AE record with COVID terms. ")),
                  mydf
            )

        }
    }
}
