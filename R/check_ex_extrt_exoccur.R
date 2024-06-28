#' @title Check for EX records where EXTRT is missing
#'
#' @description This check looks for EX records where EXTRT is missing but EXOCCUR=Y (or EXOCCUR doesn't exist) and returns a data frame
#'
#' @param EX Exposure domain with variables USUBJID, EXSTDTC, EXTRT, EXDOSE
#'
#' @author Betty Wang
#'
#' @importFrom dplyr %>% select filter
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @examples
#'
#' EX <- data.frame(
#' USUBJID = 1:10,
#' EXTRT = 1:10,
#' EXOCCUR = c(rep("Y",5), rep("",5)),
#' EXSTDTC = "2016-01-01",
#' EXDOSE = 1:10,
#' stringsAsFactors=FALSE
#' )
#'
#' EX$EXTRT[1]=""
#' EX$EXTRT[2]="NA"
#' EX$EXTRT[3]=NA
#' EX$EXTRT[6]=""
#' EX$EXTRT[7]="NA"
#' EX$EXTRT[8]=NA
#'
#' check_ex_extrt_exoccur(EX)
#'
#' EX$EXOCCUR=NULL
#'
#' check_ex_extrt_exoccur(EX)
#'
#'

check_ex_extrt_exoccur <- function (EX){

    if (EX %lacks_any% c("USUBJID","EXSTDTC","EXTRT","EXDOSE")) {

        fail(lacks_msg(EX, c("USUBJID","EXSTDTC","EXTRT","EXDOSE")))

    } else if(EX %has_all% c("EXOCCUR")){

        ### Subset EX to only records with missing EXTRT but EXOCCUR=Y
        mydf <- EX %>%
            select(USUBJID,EXSTDTC,EXTRT,EXOCCUR,EXDOSE) %>%
            filter(
                is_sas_na(EXTRT),
                EXOCCUR=="Y"
            )
        mymsg = "Patients with Missing EXTRT where EXOCCUR=Y"
    }else{

        ### Subset EX to only records with missing EXTRT where EXOCCUR doesn't exist
        mydf <- EX %>%
            select(USUBJID,EXSTDTC,EXTRT,EXDOSE) %>%
            filter(
                is_sas_na(EXTRT)
            )
        mymsg = "Patients with Missing EXTRT. "
    }

        ###Print to report

        ### Return message if no records with missing EXTRT but EXOCCUR=Y
        if (nrow(mydf)==0) {
            pass()
        } else {
            fail(
                mymsg,
                data = mydf
            )
        }
    }
