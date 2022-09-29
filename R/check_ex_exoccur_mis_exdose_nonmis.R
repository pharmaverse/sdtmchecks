#' @title Check for missing EXOCCUR but EXDOSE not missing
#'
#' @description Checks for exposure records with missing EXOCCUR but EXDOSE not missing
#'
#' @param EX Exposure dataframe with variables USUBJID, EXTRT, EXDOSE, EXOCCUR, EXSTDTC
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% select filter
#'
#' @author Iris Zhao
#'
#' @examples
#'
#'  EX <- data.frame(
#'  USUBJID = 1:10,
#'  EXTRT = rep(1,10),
#'  EXOCCUR = c(rep(1,2),rep(NA,4),rep(2,4)),
#'  EXDOSE = c(rep(NA,4),rep(1,6)),
#'  EXSTDTC = 1:10
#' )
#'
#' EX$EXOCCUR[6]="NA"
#' EX$EXOCCUR[7]=""
#' EX$EXOCCUR[8]=NA
#'
#' check_ex_exoccur_mis_exdose_nonmis(EX)
#'


check_ex_exoccur_mis_exdose_nonmis <- function(EX){

    if(EX %lacks_any% c("USUBJID", "EXTRT", "EXOCCUR", "EXDOSE", "EXSTDTC")){

      fail(lacks_msg(EX, c("USUBJID", "EXTRT", "EXOCCUR", "EXDOSE", "EXSTDTC")))

    }

    else{
        if(sum(is_sas_na(EX$EXOCCUR))==0){
          pass()
        }

        else if(sum(is_sas_na(EX$EXOCCUR))>0){

            ex2 <- EX %>%
              select("USUBJID","EXTRT","EXOCCUR","EXDOSE","EXSTDTC") %>%
              filter(is_sas_na(EX$EXOCCUR) & !is_sas_na(EX$EXDOSE))
            rownames(ex2)=NULL

            if (nrow(ex2)>0){

                fail(paste("There are", nrow(ex2), "EX records with EXOCCUR missing but EXDOSE not missing. "), ex2)

            } else {
                pass()
            }
        }
    }
}
