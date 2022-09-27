#' @title Check DS with death record but no death date
#'
#' @description This check looks for patients in DS who have a record indicating
#'   death but no corresponding record with death date in DS.
#'   For example, "Survival Follow Up" records often have no death dates, so for
#'   a data cut to be applied properly, you have to impute that missing
#'   death date from another record where its not missing (e.g. Study Discon form)
#'
#' @param DS Disposition SDTMv dataset with variables USUBJID, DSDECOD,
#'   DSSCAT and DSSTDTC
#'
#' @return Boolean value for whether the check passed or failed, with 'msg'
#'   attribute if the test failed
#'
#' @importFrom dplyr %>% filter select
#'
#' @export
#'
#' @author Will Harris
#'
#' @examples
#'
#' DS <- data.frame(STUDYID = rep(1, 5),
#'                  USUBJID = c(1, 1, 1, 2, 3),
#'                  DSDECOD = c("DEATH", "DEATH", rep("", 3)),
#'                  DSSCAT = LETTERS[1:5],
#'                  DSSTDTC = c("", "2016-01-01", "", "", "2016-01-02"),
#'                  stringsAsFactors = FALSE)
#'
#' check_ds_dsdecod_dsstdtc(DS)
#'
#' DS$DSSTDTC[2] <- ""
#'
#' check_ds_dsdecod_dsstdtc(DS)
#'
#'
check_ds_dsdecod_dsstdtc <- function(DS) {

    if (DS %lacks_any% c("USUBJID", "DSDECOD", "DSSCAT", "DSSTDTC")) {

        fail(lacks_msg(DS, c("USUBJID", "DSDECOD", "DSSCAT", "DSSTDTC")))

    } else {

        #Get all patients with a death date
        has_death_date <- DS %>%
            filter(DSDECOD == "DEATH" & !is_sas_na(DSSTDTC)) %>%
            select("USUBJID","DSDECOD","DSSTDTC") %>%
            unique()

        #Get all patients with a death record that aren't in the list of patients with a death date
        df <- DS %>%
            filter(DSDECOD == "DEATH" & !(USUBJID %in% has_death_date$USUBJID)) %>%
            select("USUBJID","DSDECOD","DSSTDTC") %>%
            unique()


        if (nrow(df)==0) {

            pass()

        } else {

            fail("DS has death recorded with no death date records. ", df)

        }
    }
}
