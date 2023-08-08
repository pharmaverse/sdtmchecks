#' @title Check for missing AETOXGR and/or AESEV values
#'
#' @description This check looks for missing AETOXGR and/or AESEV values and
#'   returns a data frame. If both variables exist it returns records where both
#'   are missing.
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AESTDTC,
#'   AEDECOD, AETERM, and AETOXGR (or AESEV)
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
#' @author Will Harris, Stella Banjo (HackR 2021)
#'
#' @examples
#' # test with sample data
#'
#'
#' AE <- data.frame(
#'  USUBJID = 1:3,
#'  DOMAIN = c(rep("AE", 3)),
#'  AESEQ = 1:3,
#'  AESTDTC = 1:3,
#'  AETERM = c("FLU COUGH", "HEADACHE", "FEVER"),
#'  AEDECOD = c("", "Headache", "Fever"),
#'  AETOXGR = 1:3,
#'  AESEV = 1:3,
#'  AESPID = "FORMNAME-R:16/L:16XXXX",
#'  stringsAsFactors = FALSE
#' )
#'
#' check_ae_aetoxgr(AE)
#'
#' AE$AETOXGR[1] <- NA
#' check_ae_aetoxgr(AE)
#'
#' AE$AESEV[1] <- NA
#' check_ae_aetoxgr(AE,preproc=roche_derive_rave_row)
#'
#' AE$AETOXGR <- NULL
#' check_ae_aetoxgr(AE,preproc=roche_derive_rave_row)
#'
#' AE$AESPID <- NULL
#' check_ae_aetoxgr(AE,preproc=roche_derive_rave_row)
#'
#' AE$AESEV <- NULL
#' check_ae_aetoxgr(AE)
#'
#' AE$AEDECOD <- NULL
#' check_ae_aetoxgr(AE)
#'
#'


check_ae_aetoxgr <- function(AE,preproc=identity,...) {

    #Apply company specific preprocessing function
    AE = preproc(AE,...)

    if (AE %lacks_any% c("USUBJID", "AETERM", "AESTDTC", "AEDECOD")) {

        fail(lacks_msg(AE, c("USUBJID", "AETERM", "AESTDTC", "AEDECOD")))

    }else if (AE %has_all% c("AETOXGR", "AESEV")) {

        has_na <- is_sas_na(AE$AETOXGR) & is_sas_na(AE$AESEV)
        if (any(has_na)) {
            df <- AE[which(has_na),] %>%
                select(any_of(c("USUBJID", "AETERM", "AESTDTC", "AEDECOD", "AETOXGR", "AESEV","RAVE")))

            fail("AE has records where both AESEV and AETOXGR have missing values. ", df)
        } else {
            pass() #No records where AETOXGR and AESEV both have missing values.
        }

    }else if (AE %lacks_all% c("AETOXGR", "AESEV")) {

        fail("AE is missing both the AETOXGR and AESEV variable.")

    }else {

        toxgr_var <- if (AE %has_all% "AETOXGR") "AETOXGR" else "AESEV"
        has_na <- is_sas_na(AE[[toxgr_var]])

        if (any(has_na)) {

            df <- AE[,intersect(names(AE),c("USUBJID", "RAVE", "AETERM", "AESTDTC", "AEDECOD",toxgr_var))] %>%
                filter(has_na)

            fail(paste("AE has", nrow(df), "record(s) with missing", toxgr_var), df)

        } else {
            pass()
        }

    }
}
