#' @title Check AEs with AESDTH of "Y" but No AEDTHDTC Value
#'
#' @description This check looks for AE entries with AESDTH of "Y"
#'   but no AEDTHDTC (death date) value
#'
#' @param AE Adverse Event SDTM dataset with variables USUBJID, AEDTHDTC,
#'   AESDTH, AEDECOD and AESTDTC
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
#' AE <- data.frame(USUBJID = c(1:5), AEDTHDTC = c(1:2, NA, "NA", 3),
#'                  AESDTH = c("", rep("Y",2), rep("", 2)),
#'                  AEDECOD = letters[1:5], AESTDTC = c(1:5),
#'                  AESPID = "FORMNAME-R:5/L:5XXXX",
#'                  stringsAsFactors=FALSE)
#'
#' check_ae_aesdth_aedthdtc(AE)
#' check_ae_aesdth_aedthdtc(AE,preproc=roche_derive_rave_row)
#'

check_ae_aesdth_aedthdtc <- function(AE,preproc=identity,...) {

    # Checks if required variables are present
    if (AE %lacks_any% c("USUBJID", "AEDTHDTC", "AESDTH", "AEDECOD", "AESTDTC")) {

        fail(lacks_msg(AE, c("USUBJID", "AEDTHDTC", "AESDTH", "AEDECOD", "AESTDTC")))

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
            select(any_of(c("USUBJID", "AEDECOD", "AESTDTC", "AESDTH", "AEDTHDTC","RAVE"))) %>%
            filter(has_aesdth, no_aedthdtc)

        # Outputs a resulting message depending on whether there are instances
        # where AESDTH = "Y" and AEDTHDTC does not have a value
        if (nrow(df)==0) {

            pass()

         } else {

             fail("AE has AESDTH equal to 'Y' where AEDTHDTC does not have a value. ", df)
        }
    }
}
