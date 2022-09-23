#' @title Check AEs with AEDTHDTC value but AESDTH not "Y"
#'
#' @description This check looks for AE entries with an AEDTHDTC (death date)
#'   value and AESDTH not equal to "Y"
#'
#' @param AE Adverse Event SDTM dataset with variables USUBJID, AEDTHDTC,
#'   AESDTH, AEDECOD, and AESTDTC
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
#' AE <- data.frame(USUBJID = c(1:5),
#'                  AEDTHDTC = c(rep("2020-01-01",3), "NA", NA),
#'                  AESDTH = c(rep("", 2), "Y", rep("", 2)),
#'                  AEDECOD = letters[1:5],
#'                  AESTDTC = "2020-01-01",
#'                  AESPID = "FORMNAME-R:13/L:13XXXX",
#'                  stringsAsFactors=FALSE)
#'
#' check_ae_aedthdtc_aesdth(AE)
#' check_ae_aedthdtc_aesdth(AE,preproc=roche_derive_rave_row)
#'

check_ae_aedthdtc_aesdth <- function(AE,preproc=identity,...) {

    # Checks if required variables are present
    if (AE %lacks_any% c("USUBJID", "AEDTHDTC", "AESDTH", "AEDECOD", "AESTDTC")) {

        fail(lacks_msg(AE, c("USUBJID", "AEDTHDTC", "AESDTH", "AEDECOD", "AESTDTC")))

    } else {

        #Apply company specific preprocessing function
        AE = preproc(AE,...)

        # Rows where AEDTHDTC is not NA
        has_aedthdtc <- !(is_sas_na(AE[["AEDTHDTC"]]))

        # Rows where AESDTH != "Y"
        no_aesdth <- !(AE[["AESDTH"]]=="Y")

        # Subsets AE to select variables and rows where
        # AESDTH != "Y" and AEDTHDTC has a value
        df <- AE %>%
            select(any_of(c("USUBJID", "AEDECOD", "AESTDTC","AEDTHDTC","AESDTH","RAVE"))) %>%
            filter(has_aedthdtc, no_aesdth)

        rownames(df) = NULL

        # Outputs a resulting message depending on whether there are instances
        # where AESDTH != "Y" and AEDTHDTC having a value
        if (nrow(df)==0) {

            pass()

        } else {

            fail("AE has AESDTH not equal to 'Y' where AEDTHDTC has a value. ", df)

        }
    }
}
