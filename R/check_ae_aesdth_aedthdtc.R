#' @title Check AEs with AESDTH of "Y" but No AEDTHDTC Value
#'
#' @description This check looks for AE entries with AESDTH of "Y"
#'   but no AEDTHDTC (death date) value
#'
#' @param AE Adverse Event SDTM dataset with variables USUBJID, AEDTHDTC,
#'   AESDTH, AETERM, AEDECOD and AESTDTC
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#'
#' @return Boolean value for whether the check passed or failed, with 'msg'
#'   attribute if the check failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#'
#' @author Shumei Chi
#'
#' @examples
#'
#'
#' AE <- data.frame(
#' USUBJID = c(1:7), 
#' AEDECOD = c(letters[1:5], "", NA), 
#' AETERM = letters[1:7],
#' AESDTH = c(NA, rep("", 4), "Y", "Y"),
#' AEDTHDTC = c(1:5, "2020", "2020-01-02"),
#' AESTDTC = c(1:7),
#' AESPID = "FORMNAME-R:5/L:5XXXX",
#' stringsAsFactors=FALSE)
#' 
#' # expect pass
#' check_ae_aesdth_aedthdtc(AE)
#' check_ae_aesdth_aedthdtc(AE,preproc=roche_derive_rave_row)
#' 
#' # expect fail 
#' AE1 <- AE
#' AE1$AEDTHDTC[3] <- NA
#' AE1$AESDTH[3] <- "Y"
#' 
#' check_ae_aesdth_aedthdtc(AE1)
#' check_ae_aesdth_aedthdtc(AE1,preproc=roche_derive_rave_row)
#' 
#' # expect fail 
#' AE2 <- AE1
#' AE2$AEDTHDTC[4] <- ""
#' AE2$AESDTH[4] <- "Y"
#' 
#' check_ae_aesdth_aedthdtc(AE2)
#' check_ae_aesdth_aedthdtc(AE2,preproc=roche_derive_rave_row)
#' 
#' # non-required variable missing
#' AE2$AESPID <- NULL
#' check_ae_aesdth_aedthdtc(AE2)
#' check_ae_aesdth_aedthdtc(AE2,preproc=roche_derive_rave_row)
#' 
#' # required variable missing 
#' AE2$AESDTH <- NULL
#' check_ae_aesdth_aedthdtc(AE2)
#' check_ae_aesdth_aedthdtc(AE2,preproc=roche_derive_rave_row)
#'

check_ae_aesdth_aedthdtc <- function(AE,preproc=identity,...) {

    # Checks if required variables are present
    if (AE %lacks_any% c("USUBJID", "AEDTHDTC", "AESDTH", "AETERM", "AEDECOD", "AESTDTC")) {

        fail(lacks_msg(AE, c("USUBJID", "AEDTHDTC", "AESDTH", "AETERM", "AEDECOD", "AESTDTC")))

    } else {

        #Apply company specific preprocessing function
        AE = preproc(AE,...)

        # Rows where AESDTH is "Y"
        has_aesdth <- AE[["AESDTH"]]=="Y"

        # Rows where AEDTHDTC is NA
        no_aedthdtc <- is_sas_na(AE[["AEDTHDTC"]])

        # Subsets AE to select variables and rows where
        # AESDTH = "Y" and AEDTHDTC does not have a value
        df <- AE %>%
            select(any_of(c("USUBJID", "AETERM", "AEDECOD", "AESTDTC", "AESDTH", "AEDTHDTC","RAVE"))) %>%
            filter(has_aesdth, no_aedthdtc)

        # Outputs a resulting message depending on whether there are instances
        # where AESDTH = "Y" and AEDTHDTC does not have a value
        if (nrow(df)==0) {

            pass()

         } else {

             fail(paste0("AE has ", nrow(df), " record(s) with AESDTH equal to 'Y' where AEDTHDTC does not have a value. "), df)
        }
    }
}
