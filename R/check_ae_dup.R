#' @title Check for duplicate AE entries
#'
#' @description Identifies duplicated AE entries based on USUBJID, AETERM,
#'   AEDECOD, AESTDTC, AEENDTC, AEMODIFY (if present), AELAT (if present) and AETOXGR or AESEV
#'
#' @param AE AE SDTM dataset with variables USUBJID, AETERM, AEDECOD,
#'   AESTDTC, AEENDTC, and AETOXGR or AESEV
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @importFrom dplyr %>% group_by_all filter select n
#' @importFrom tidyselect any_of
#'
#' @author Edgar Manukyan
#'
#' @examples
#'
#' AE <- data.frame(USUBJID = c(1), AESTDTC = c("2020-01-01","2020-01-01","2020-02-01","2020-03-01"),
#'                  AEENDTC = rep("2020-02-01",4), AEDECOD = letters[c(1,1:3)],
#'                  AETERM = letters[c(1,1:3)], AETOXGR = c(1,1:3),
#'                  AESPID="FORMNAME-R:5/L:5XXXX",
#'                  stringsAsFactors=FALSE)
#'
#' check_ae_dup(AE)
#'
#'
#'

check_ae_dup <- function(AE){

    # Checks whether required variables are in dataset
    if (AE %lacks_any% c("USUBJID", "AEDECOD", "AESTDTC", "AEENDTC", "AETERM")) {

        fail(lacks_msg(AE, c("USUBJID", "AEDECOD", "AESTDTC", "AEENDTC", "AETERM")))

    } else if (AE %has_all% c("AETOXGR", "AESEV")) {

        fail("AE has both variables: AETOXGR and AESEV.")

    } else if (AE %lacks_all% c("AETOXGR", "AESEV")) {

        fail("AE is missing both the AETOXGR and AESEV variable.")

    } else {

        # Use either AETOXGR or AESEV, depending on which is in the AE dataset
        toxgr_var <- if(AE %has_all% "AETOXGR") "AETOXGR" else "AESEV"
        lat_var <- if (AE %has_all% "AELAT") "AELAT" else NULL

        if (AE %lacks_any% c("AEMODIFY")){
            # When AEMODIFY not in AE
            # Subsets to duplicated entries only
            df <- AE %>%
                select(USUBJID, AETERM, AEDECOD, AESTDTC, AEENDTC, any_of(c(toxgr_var,lat_var))) %>%
                group_by_all() %>%
                filter(n()>1)
        }else {
            # When AEMODIFY in AE
            # Subsets to duplicated entries only
            df <- AE %>%
                select(USUBJID, AETERM, AEDECOD, AESTDTC, AEENDTC, AEMODIFY, any_of(c(toxgr_var,lat_var))) %>%
                group_by_all() %>%
                filter(n()>1)
        }

        # Outputs a resulting message depending on whether there are duplicates
        if (nrow(df) != 0) {

            fail("AE has duplicated entries. ", df)

        } else {

            pass()

        }
    }
}

