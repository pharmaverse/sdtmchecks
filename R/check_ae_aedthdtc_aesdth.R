#' @title Check AEs with AEDTHDTC value but AESDTH not "Y"
#'
#' @description This check looks for AE entries with an AEDTHDTC (death date)
#'   value and AESDTH not equal to "Y"
#'
#' @param AE Adverse Event SDTM dataset with variables USUBJID, AEDTHDTC,
#'   AESDTH, AEDECOD, AETERM, and AESTDTC
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return Boolean value for whether the check passed or failed, with 'msg'
#'   attribute if the check failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
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
#' AESDTH = "Y",
#' AEDTHDTC = "2020-01-02",
#' AESTDTC = c(1:7),
#' AESPID = "FORMNAME-R:5/L:5XXXX",
#' stringsAsFactors=FALSE)
#' 
#' # expect pass
#' check_ae_aedthdtc_aesdth(AE)
#' check_ae_aedthdtc_aesdth(AE,preproc=roche_derive_rave_row)
#' 
#' # expect fail 
#' AE1 <- AE
#' AE1$AESDTH[3] <- "N"
#' check_ae_aedthdtc_aesdth(AE1)
#' check_ae_aedthdtc_aesdth(AE1,preproc=roche_derive_rave_row)
#' 
#' # expect fail with AESDTH = NA
#' AE2 <- AE
#' AE2$AESDTH[4] <- NA
#' check_ae_aedthdtc_aesdth(AE2)
#' check_ae_aedthdtc_aesdth(AE2,preproc=roche_derive_rave_row)
#' 
#' # non-required variable missing
#' AE2$AESPID <- NULL
#' check_ae_aedthdtc_aesdth(AE2)
#' check_ae_aedthdtc_aesdth(AE2,preproc=roche_derive_rave_row)
#' 
#' # required variable missing 
#' AE2$AESDTH <- NULL
#' check_ae_aedthdtc_aesdth(AE2)
#' check_ae_aedthdtc_aesdth(AE2,preproc=roche_derive_rave_row)
#'
#'
#'


check_ae_aedthdtc_aesdth <- function(AE,preproc=identity,...) {

    # Checks if required variables are present
    if (AE %lacks_any% c("USUBJID", "AEDTHDTC", "AETERM", "AESDTH", "AEDECOD", "AESTDTC")) {

        fail(lacks_msg(AE, c("USUBJID", "AEDTHDTC", "AETERM", "AESDTH", "AEDECOD", "AESTDTC")))

    } else {

        #Apply company specific preprocessing function
        AE = preproc(AE,...)

        # Rows where AEDTHDTC is not NA
        has_aedthdtc <- !(is_sas_na(AE[["AEDTHDTC"]]))

        # Rows where AESDTH != "Y", with expanded logic for NA values
        no_aesdth <- !(AE[["AESDTH"]]=="Y") | (is_sas_na(AE[["AESDTH"]]))

        # Subsets AE to select variables and rows where
        # AESDTH != "Y" and AEDTHDTC has a value
        df <- AE %>%
            select(any_of(c("USUBJID", "AETERM", "AEDECOD", "AESTDTC","AEDTHDTC","AESDTH","RAVE"))) %>%
            filter(has_aedthdtc, no_aesdth)

        rownames(df) = NULL

        # Outputs a resulting message depending on whether there are instances
        # where AESDTH != "Y" and AEDTHDTC having a value
        if (nrow(df)==0) {

            pass()

        } else {
            
            fail(paste0("AE has ", nrow(df), " record(s) with AESDTH not equal to 'Y' where AEDTHDTC has a value. "), df)

        }
    }
}
