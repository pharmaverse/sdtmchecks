#' @title Check non-missing DEAD status date in SS and an according DM record with death date
#'        where status date is greater or equal to death date
#'
#' @description This check looks for non-missing SS.SSDTC when SS.SSSTRESC='DEAD' and
#'              Subject Status Date/Time of Assessments is less than
#'              Start Date/Time of Disposition Event(SS.SSDTC < DS.DSSTDTC)
#'
#' @param SS Subject Status SDTM dataset with variables USUBJID, SSDTC, SSSTRESC, VISIT
#' @param DM Demographics SDTM dataset with variables USUBJID, DTHDTC
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr left_join filter %>%
#'
#' @author Vira Vrakina
#'
#' @examples
#'
#' SS <- data.frame(
#'  USUBJID = 1:5,
#'  SSDTC = "2020-01-02",
#'  SSSTRESC  = c("DEAD","DEAD","ALIVE","DEAD","ALIVE"),
#'  VISIT = "DAY 10"
#' )
#'
#' DM <- data.frame(
#'  USUBJID = 1:5,
#'  DTHDTC = "2020-01-02"
#' )
#'
#' check_ss_ssdtc_dead_dthdtc(SS, DM)
#'
#' SS <- data.frame(
#'  USUBJID = 1:5,
#'  SSDTC = "2020-01-02",
#'  SSSTRESC  = c("DEAD","DEAD","ALIVE","DEAD","ALIVE"),
#'  VISIT = "FOLLOW-UP"
#' )
#'
#' DM <- data.frame(
#'  USUBJID = 1:5,
#'  DTHDTC = c("2020-01-01","2020-01-02","2020-01-03","2020-01-04","2020-01-02")
#' )
#'
#' check_ss_ssdtc_dead_dthdtc(SS, DM)

check_ss_ssdtc_dead_dthdtc <- function(SS, DM) {

    ###First check that required variables exist and return a message if they don't
    if(SS %lacks_any% c("USUBJID","SSDTC","SSSTRESC", "VISIT")){

        fail(lacks_msg(SS, c("USUBJID","SSDTC","SSSTRESC", "VISIT")))

    }else if(DM %lacks_any% c("USUBJID", "DTHDTC")){

        fail(lacks_msg(DM, c("USUBJID", "DTHDTC")))

    }else{

        myss <- subset(SS, !is_sas_na(SS$SSDTC) & SS$SSSTRESC == 'DEAD', c("USUBJID","SSDTC","SSSTRESC", "VISIT"))
        mydm <- subset(DM, !is_sas_na(DM$DTHDTC), c("USUBJID", "DTHDTC"))
        mydf <- myss %>%
                left_join(mydm, by="USUBJID") %>%
                filter(SSDTC < DTHDTC)

        ###Print to report

        ### Return message if no records
        if(nrow(mydf)==0 ){
            pass()

            ### Return subset dataframe if there are records where SS.SSDTC < DM.DTHDTC
        }else if(nrow(mydf)>0){
            fail(paste(length(unique(mydf$USUBJID)),
                       " patient(s) with DEAD status where in SS domain where SS date is less than death date in DM domain. ",sep=""),
                 mydf)
        }
    }
}
