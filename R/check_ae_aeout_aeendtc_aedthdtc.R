#' @title Check for AE outcome (AEOUT) of 'FATAL' with non-missing resolution
#' date that is not equal to the death date
#'
#' @description This check looks for AEs with outcome of 'FATAL' but AE
#' resolution date is not equal to AE death date.
#' Note that these datapoints are not collected the same way for all trials -
#' some trials leave AEENDTC missing if it was unresolved at death date. Confirm
#' within your team before querying this issue.
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AETERM,
#' AEDTHDTC, AEENDTC, AEOUT
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#'
#' @author Sara Bodach, Stella Banjo(HackR 2021)
#'
#' @examples
#'
#' AE <- data.frame(
#'     USUBJID = 1:10,
#'     DOMAIN = "AE",
#'     AEDTHDTC = c(NA, "NA", rep("2015-03-12",4), NA, NA, "2020-01-01", ""),
#'     AEENDTC = c(NA, "NA", rep("2015-03-12",4), NA, "2020-01-01", NA, ""),
#'     AEOUT = c("", "", "","FATAL","RECOVERED/RESOLVED", rep("FATAL",5)),
#'     AETERM = 1:10,
#'     AESPID = "FORMNAME-R:13/L:13XXXX",
#'     stringsAsFactors = FALSE
#' )
#'
#' check_ae_aeout_aeendtc_aedthdtc(AE)
#' check_ae_aeout_aeendtc_aedthdtc(AE,preproc=roche_derive_rave_row)
#'
#' AE$AESPID <- NULL
#' check_ae_aeout_aeendtc_aedthdtc(AE)
#'
#' AE$AEDTHDTC <- NULL
#' AE$AEOUT <- NULL
#' check_ae_aeout_aeendtc_aedthdtc(AE)
#'

check_ae_aeout_aeendtc_aedthdtc <- function(AE,preproc=identity,...) {

    if (AE %lacks_any% c("USUBJID", "AETERM", "AEENDTC", "AEDTHDTC", "AEOUT")) {

        fail(lacks_msg(AE, c("USUBJID", "AETERM", "AEENDTC", "AEDTHDTC", "AEOUT" )))

    }else {

        #Apply company specific preprocessing function
        AE = preproc(AE,...)

            mydf <- AE %>%
                select(any_of(c("USUBJID", "AETERM", "AEENDTC", "AEDTHDTC", "AEOUT", "RAVE"))) %>%
                filter(AEOUT == 'FATAL') %>%
                filter(AEENDTC != AEDTHDTC | is_sas_na(AEENDTC))
            rownames(mydf)=NULL

       ## Add note
        mydf$NOTE <- "**QUERY ONLY IF TEAM AGREES**"

        if( nrow(mydf) > 0 ){
            fail(paste(nrow(mydf), " AE(s) with AEOUT = 'FATAL' but AEDTHDTC and AEENDTC inconsistent. ",sep=""),
            mydf)
        } else {
            pass() # pass if AEOUT is FATAL but AEDTHDTC and AEENDTC are consistent
        }
    }
}
