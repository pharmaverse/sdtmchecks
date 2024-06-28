#' @title Check if randomized patients are missing stratification factor data
#'
#' @description Check if Study is randomized (DS.DSDECOD == "RANDOMIZED" or "RANDOMIZATION"), and subject Characteristics Domain (SC) has no stratification factors reported.
#'
#' @param DS Subject Dispostion Dataset with variable USUBJID, DSDECOD, DSSTDTC
#' @param SC Subject Characteristics Dataset with variables USUBJID, SCTEST, SCTESTCD, SCCAT, SCORRES
#'
#' @importFrom dplyr %>% filter mutate select left_join group_by
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @author Monarch Shah
#'
#' @examples
#'
#' ds <- data.frame(USUBJID = c(1,2,2),
#'                  DSDECOD = c("RANDOMIZATION", "RANDOMIZED", "Randomized"),
#'                  DSSTDTC = c("2021-01-01", "2021-01-02", "2021-02-01"))
#' sc <- data.frame(USUBJID  = c(1,1,1,2,2,2),
#'                  SCCAT = rep("STRATIFICATION", 6),
#'                  SCTESTCD = c("STRAT 1", "STRAT 2", "STRAT 3", "STRAT 1", "STRAT 2", "STRAT 3"),
#'                  SCTEST   = c("Factor 1", "Factor 2", "Factor 3",
#'                               "Factor 1", "Factor 2", "Factor 3"),
#'                  SCORRES  = c("US", "Left", "Score > x", "RoW", "Right", "Score < x"),
#'                  stringsAsFactors = FALSE)
#'
#' check_ds_sc_strat(ds, sc)
#'
#' ds <- data.frame(USUBJID = c(1,2,2),
#'                  DSDECOD = c("RANDOMIZATION", "RANDOMIZED", "Randomized"),
#'                  DSSTDTC = c("2021-01-01", "2021-01-02", "2021-02-01"))
#' sc <- data.frame(USUBJID  = c(1,1,1),
#'                  SCCAT = rep("STRATIFICATION", 3),
#'                  SCTESTCD = c("STRAT 1", "STRAT 2", "STRAT 3"),
#'                  SCTEST   = c("Factor 1", "Factor 2", "Factor 3"),
#'                  SCORRES  = c("US", "Left", NA),
#'                  stringsAsFactors = FALSE)
#'
#' check_ds_sc_strat(ds, sc)
#'
#' ds <- data.frame(USUBJID = c(1,2),
#'                  DSDECOD = c("Open Label", "Open Label"),
#'                  DSSTDTC = c("2021-01-01", "2021-01-02"))
#' sc <- data.frame(USUBJID  = c(1,1,1,2,2,2),
#'                  SCCAT = rep("No STRATIFICATION", 6),
#'                  SCTESTCD = c("STRAT 1", "STRAT 2", "STRAT 3", "STRAT 1", "STRAT 2", "STRAT 3"),
#'                  SCTEST   = c("Factor 1", "Factor 2", "Factor 3",
#'                               "Factor 1", "Factor 2", "Factor 3"),
#'                  SCORRES  = c("US", "Left", NA, "RoW", "Right", "Score < x"),
#'                  stringsAsFactors = FALSE)
#'
#' check_ds_sc_strat(ds, sc)
#'
#'


check_ds_sc_strat <- function(DS, SC) {


    if (DS %lacks_any% c("USUBJID", "DSDECOD", "DSSTDTC")) {

        fail(lacks_msg(DS, c("USUBJID", "DSDECOD","DSSTDTC")))

    }

    else if (SC %lacks_any% c("USUBJID", "SCTEST", "SCTESTCD", "SCCAT", "SCORRES")) {

        fail(lacks_msg(SC, c("USUBJID","SCTEST","SCTESTCD","SCCAT","SCORRES")))

    }

    else {

        DS = DS %>%
            filter(toupper(DSDECOD) %in% c("RANDOMIZED", "RANDOMIZATION")) %>%
            mutate(DS_RANDDT = as.Date(DSSTDTC)) %>%
            select(USUBJID, DS_RANDDT) %>%
            group_by(USUBJID) %>%
            slice(which.min(DS_RANDDT))

        SC = SC %>%
            select(USUBJID, SCTESTCD, SCTEST, SCCAT, SCORRES) %>%
            filter(toupper(SCCAT) == "STRATIFICATION")


        if (nrow(DS) == 0) {

            fail("Only applicable for randomized studies. Based on DS, no records were found indicating randomized patients.")

        } else if(nrow(SC) == 0) {

            fail("Study is randomized but no records in SC indicating stratification factors. ")

        }

        else {

            mydf1 = left_join(DS, SC, by="USUBJID")
            mydf1 = mydf1 %>%
                mutate(MISFLAG = ifelse(is.na(SCORRES), 1, 0)) %>%
                filter(MISFLAG == 1) %>%
                select(-SCCAT, -MISFLAG)

            mydf1[is.na(mydf1)] <- " "

            if ((nrow(mydf1) == 0 )) {
                pass()
            } else {
                fail(paste0(length(unique(mydf1$USUBJID)), " patient(s) for randomized study where stratification factors are missing. "), mydf1 )
            }
        }
    }
}
