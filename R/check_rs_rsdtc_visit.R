#' @title Check missing RSDTC and VISIT
#'
#' @description This check looks for missing RSDTC or VISIT values when
#' RSORRES is not missing and RSSTAT not equal to "NOT DONE" in RS dataset
#' and returns a data frame. Only applies to assessments by investigator.
#'
#' @param RS Disease Response SDTM dataset with variables USUBJID, RSDTC,
#' RSORRES, VISIT, RSSTAT
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#'
#' @author Peggy Wen, Stella Banjo (HackR 2021)
#'
#' @examples
#'
#' RS <- data.frame(
#' USUBJID = 1:10,
#' RSDTC = 1:10,
#' RSORRES = "THING",
#' VISIT = "C1D1",
#' RSSTAT = 1:10,
#' RSEVAL = c("NA","","IRF","investigator",rep("INVESTIGATOR",6)),
#' stringsAsFactors=FALSE
#' )
#'
#' RS$RSDTC[1]=""
#' RS$RSDTC[2]="NA"
#' RS$RSDTC[3]=NA
#' RS$VISIT[3]=""
#' RS$VISIT[4]="NA"
#' RS$VISIT[5]=NA
#' check_rs_rsdtc_visit(RS)
#'
#' RS$RSORRES[1]=""
#' check_rs_rsdtc_visit(RS)
#'
#' RS$RSORRES[4] = "THING 1"
#' RS$RSORRES[5] = "THING 2"
#'
#' check_rs_rsdtc_visit(RS)
#'
#'

check_rs_rsdtc_visit <- function(RS){

    ###First check that required variables exist and return a message if they don't
    if(RS %lacks_any% c("USUBJID", "RSDTC", "RSORRES", "VISIT", "RSSTAT")){

        fail(lacks_msg(RS, c("USUBJID", "RSDTC", "RSORRES", "VISIT", "RSSTAT")))

    } else{

        if(RS %lacks_any% "RSEVAL"){
        ### Subset to only records with missing RSDTC or missing VISIT
        mydf <- RS %>%
            filter((is_sas_na(RSDTC) | is_sas_na(VISIT)) &!is_sas_na(RSORRES) & toupper(RSSTAT) != "NOT DONE") %>%
            select(USUBJID, RSDTC, RSORRES, VISIT, RSSTAT) 
                
        }else{
            mydf <- RS %>%
                filter((is_sas_na(RSDTC) | is_sas_na(VISIT)) &!is_sas_na(RSORRES) & toupper(RSSTAT) != "NOT DONE" & (toupper(RSEVAL) == "INVESTIGATOR" | is_sas_na(RSEVAL))   ) %>%
                select(USUBJID, RSDTC, RSORRES, VISIT, RSSTAT,RSEVAL)
                
        }

        rownames(mydf)=NULL


        if (nrow(mydf)==0) {
            pass()
        } else{
            fail(paste("There are",nrow(mydf),
                       "records with missing RSDTC or VISIT. "), mydf)
        }
    }
}
