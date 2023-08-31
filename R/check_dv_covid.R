#' @title Check for consistency in COVID-19 DV variables, DVREAS and DVEPRELI
#'
#' @description This check looks for inconsistency between DVREAS and DVEPRELI.
#' If DVREAS indicates a COVID-19 related deviation, then DVEPRELI should not be 
#' missing and vice versa. This check applies to studies using the Protocol 
#' Deviation Management System (PDMS).
#'
#' @param DV Protocol Deviations SDTM dataset with variables USUBJID, DVREAS,
#' DVEPRELI
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the 
#' test failed
#'
#' @importFrom dplyr %>% filter select
#'
#' @export
#'
#' @author Mij Rahman
#' 
#' @family COVID
#' 
#' @keywords COVID
#'
#' @examples
#'
#' DV <- data.frame(
#'     USUBJID = 1:3,
#'     DVEPRELI = c("Y","N","Y"),
#'     DVREAS=c("EPIDEMIC/PANDEMIC INFECTION","EPIDEMIC/PANDEMIC INFECTION",""),
#'     stringsAsFactors=FALSE
#' )
#'
#' check_dv_covid(DV)
#'


check_dv_covid <- function(DV){

    ###First check that required variables exist and return a message if they don't
    if(DV %lacks_any% c("USUBJID","DVREAS","DVEPRELI")){

        fail(lacks_msg(DV, c("USUBJID","DVREAS","DVEPRELI")))

    }else{

        # Select DV recs where DVREAS='CONFIRMED OR SUSPECTED EPIDEMIC/PANDEMIC INFECTION' or DVEPRELI=Y and check consistency
        mydf <- DV %>%
            select(c("USUBJID","DVREAS","DVEPRELI")) %>%
            filter( ( DVEPRELI != "Y" & !is_sas_na(DVREAS)) | (DVEPRELI=="Y" & is_sas_na(DVREAS) ) )

        ###Print to report

        ### Return pass if all records with COVID-related DV.DVREAS had corresponding DV.DVEPRELI or vice versa
        if(nrow(mydf)==0){
            pass()
            ### Return subset /message if there are records with COVID-related inconsistencies with DV.DVREAS/DVEPRELI
        }else if(nrow(mydf)>0){
            fail( (paste("Found ", length(unique(mydf$USUBJID)),
                         "patient(s) with COVID-related Protocol Deviation inconsistencies. ")),
                  mydf)
        }
    }
}
