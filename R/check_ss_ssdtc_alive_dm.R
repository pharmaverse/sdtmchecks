#' @title Check non-missing last ALIVE status date in SS is before than death date in DM
#'
#' @description This check looks for non-missing SS.SSDTC when SS.SSORRES contains 'ALIVE' and
#'              Subject Status Date/Time of Assessments is greater then
#'              Start Date/Time of Disposition Event(SS.SSDTC > DS.DSSTDTC)
#'
#' @param SS Subject Status SDTM dataset with variables USUBJID, SSDTC, SSORRES, SSTESTCD, VISIT
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
#'  SSTESTCD = "SURVSTAT",
#'  SSORRES  = c("DEAD","DEAD","ALIVE","DEAD","ALIVE"),
#'  VISIT = "WEEK 4"
#' )
#'
#'
#' DM <- data.frame(
#'  USUBJID = 1:5,
#'  DTHDTC = "2020-01-03"
#' )
#'
#' check_ss_ssdtc_alive_dm(SS, DM)
#'
#' SS <- data.frame(
#'  USUBJID = 1:5,
#'  SSDTC = "2020-01-04",
#'  SSTESTCD = "SURVSTAT",
#'  SSORRES  = c("DEAD","DEAD","ALIVE","DEAD","ALIVE"),
#'  VISIT = "WEEK 4"
#' )
#'
#' DM <- data.frame(
#'  USUBJID = 1:5,
#'  DTHDTC = c("2020-01-04", "2020-01-05", "2020-01-03", "2020-01-04", "2020-01-05")
#' )
#'
#' check_ss_ssdtc_alive_dm(SS, DM)
#'

check_ss_ssdtc_alive_dm <- function(SS, DM) {

    ###First check that required variables exist and return a message if they don't
    if(SS %lacks_any% c("USUBJID","SSDTC","SSORRES", "SSTESTCD", "VISIT")){

        fail(lacks_msg(SS, c("USUBJID","SSDTC","SSORRES", "SSTESTCD", "VISIT")))

    }else if(DM %lacks_any% c("USUBJID", "DTHDTC")){

        fail(lacks_msg(DM, c("USUBJID", "DTHDTC")))

    }else{

        myss <- subset(SS, !is_sas_na(SS$SSDTC) & SS$SSTESTCD == 'SURVSTAT' & grepl("ALIVE", SS$SSORRES), c("USUBJID","SSDTC","SSORRES","SSTESTCD", "VISIT"))
        mydm <- subset(DM, !is_sas_na(DM$DTHDTC), c("USUBJID", "DTHDTC"))
        mydf <- myss %>%
            left_join(mydm, by="USUBJID") %>%
            filter(SSDTC > DTHDTC)

        ###Print to report

        ### Return message if no records
        if(nrow(mydf)==0 ){
            pass()

            ### Return subset dataframe if there are records where SS.SSDTC > DM.DTHDTC
        }else if(nrow(mydf)>0){
            fail(paste(length(unique(mydf$USUBJID)),
                       " patient(s) with ALIVE status date in SS domain later than death date in DM domain. ",sep=""),
                 mydf)
        }
    }

}
