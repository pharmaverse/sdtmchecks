#' @title Check DM where ARM is not equal to ACTARM
#'
#' @description This check looks for DM entries where ARM is not equal to ACTARM
#'
#' @param DM Demographics SDTM dataset with variables USUBJID, ARM, and ACTARM
#'
#' @return Boolean value for whether the check passed or failed, with 'msg'
#'   attribute if the check failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#'
#' @author Ying Yuen
#'
#' @examples
#'
#' DM <- data.frame(USUBJID = 1:5,
#'                  ARM = c(letters[1:3], letters[5:6]),
#'                  ACTARM = letters[1:5],
#'                  stringsAsFactors = FALSE)
#'
#' check_dm_actarm_arm(DM)
#'

check_dm_actarm_arm <- function(DM) {

    if (DM %lacks_any% c("USUBJID", "ARM", "ACTARM")) {

        fail(lacks_msg(DM, c("USUBJID", "ARM", "ACTARM")))

    } else {

        df <- DM %>%
            select("USUBJID", "ARM", "ACTARM") %>%
            filter(DM[["ARM"]] != DM[["ACTARM"]])

        if (nrow(df)==0) {

            pass()

        } else {

            fail("DM has entries where ARM != ACTARM. ", df)

        }
    }
}
