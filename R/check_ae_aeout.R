#' @title Check for inconsistency between AE outcome (AEOUT) and death date (AEDTHDTC)
#'
#' @description This check looks for AEs with Death date(AEDTHDTC) but outcome
#' (AEOUT) is not FATAL and conversely AEs with no death date (AEDTHDTC) but
#' outcome (AEOUT) is fatal
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AEDTHDTC, AEOUT
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#'
#' @export
#'
#' @author Shumei Chi
#'
#' @examples
#'
#' AE <- data.frame(
#'     USUBJID = 1:8,
#'     AEDTHDTC = c(NA, "NA", "2015-03-12", "2017-01-22", "1999-11-07","",NA, "2020-01-01"),
#'     AEOUT = c("", "", "","FATAL","RECOVERED/RESOLVED","FATAL","FATAL", NA),
#'     AESPID = "FORMNAME-R:13/L:13XXXX",
#'     stringsAsFactors = FALSE
#' )
#'
#' check_ae_aeout(AE)
#' check_ae_aeout(AE,preproc=roche_derive_rave_row)
#'
#' AE$AEDTHDTC <- NULL
#' check_ae_aeout(AE)
#'
#' AE$AEOUT <- NULL
#' check_ae_aeout(AE)
#'

check_ae_aeout <- function(AE,preproc=identity,...){

    if (AE %lacks_any% c("USUBJID", "AEDTHDTC", "AEOUT")) {

        fail(lacks_msg(AE, c("USUBJID", "AEDTHDTC", "AEOUT" )))

    } else {

        #Apply company specific preprocessing function
        AE = preproc(AE,...)

        df <- AE %>%
            filter((!is_sas_na(AEDTHDTC) & (AEOUT != "FATAL" | is_sas_na(AEOUT)) ) | (AEOUT=="FATAL" & is_sas_na(AEDTHDTC))) %>%
            select(any_of(c("USUBJID","AEDTHDTC","AEOUT","RAVE")))

        if( nrow(df) > 0 ){
            fail(paste(nrow(df), "AE(s) with inconsistent AEDTHDTC and AEOUT found. "), df)
        } else {
            pass()
        }
    }
} 
