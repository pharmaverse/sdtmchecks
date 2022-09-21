#' @title Check for inconsistency between AE outcome (AEOUT) and AE death date (AEDTHDTC)
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
#'     STUDYID = "Study1",
#'     USUBJID = 1:7,
#'     AEDTHDTC = c(NA, "NA", "2015-03-12", "2017-01-22", "1999-11-07","",NA),
#'     AEOUT = c("", "", "","FATAL","RECOVERED/RESOLVED","FATAL","FATAL"),
#'     AESPID = rep("FORMNAME-R:13/L:13XXXX",7),
#'     stringsAsFactors = FALSE
#' )
#'
#' check_ae_aeout(AE)
#' check_ae_aeout(AE,preproc=roche_derive_rave_row)
#'
#' AE$AESPID <- NULL
#' check_ae_aeout(AE,preproc=roche_derive_rave_row)
#'
#' AE$AEDTHDTC <- NULL
#' check_ae_aeout(AE)
#'
#' AE$AEOUT <- NULL
#' check_ae_aeout(AE)
#'

check_ae_aeout <- function(AE,preproc=identity,...) {

  if (AE %lacks_any% c("USUBJID", "AEDTHDTC", "AEOUT")) {

    fail(lacks_msg(AE, c("USUBJID", "AEDTHDTC", "AEOUT" )))

  } else {

    df <- preproc(AE,...) %>% #Apply company specific preprocessing function
      filter((!is_sas_na(AEDTHDTC) & AEOUT != "FATAL") | (AEOUT=="FATAL" & is_sas_na(AEDTHDTC))) %>%
      select(any_of(c("USUBJID","AEDTHDTC","AEOUT","RAVE")))

    if( nrow(df) > 0 ){
      fail(paste("AE has ", nrow(df), " record(s) with inconsistent AEDTHDTC and AEOUT. ",
                 sep=""), df)
    } else {
      pass()
    }
  }
}
