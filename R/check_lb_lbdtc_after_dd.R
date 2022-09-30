#' @title Check for LB dates occurring after death date
#'
#' @description This check looks for LB dates that occur after death date
#'
#' @param AE Adverse Event SDTM dataset with variables USUBJID,
#'   AEDTHDTC, AESTDTC, AEDECOD, and AETERM
#'
#' @param DS Disposition SDTM dataset with variables USUBJID,
#'   DSSTDTC, DSDECOD, and DSTERM
#'
#' @param LB Laboratory Test Findings SDTM dataset with variables USUBJID,
#'   LBDTC, LBTESTCD, and LBORRES
#'
#' @return Boolean value for whether the check passed or failed, with 'msg'
#'   attribute if the check failed
#'
#' @export
#'
#' @importFrom dplyr %>% arrange filter full_join left_join mutate select slice
#'
#' @author Nina Ting Qi
#'
#' @examples
#'
#' AE <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
#'                  AEDTHDTC = c(rep("", 4), "2016-01-01"),
#'                  AESTDTC = rep("2016-01-01", 5),
#'                  AEDECOD = LETTERS[1:5], AETERM = LETTERS[1:5],
#'                  stringsAsFactors = FALSE)
#'
#' DS <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
#'                  DSSTDTC = rep("2016-01-02", 5),
#'                  DSDECOD = c(LETTERS[1:4], "death"),
#'                  DSTERM = letters[1:5],
#'                  stringsAsFactors = FALSE)
#'
#' LB <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
#'                  LBDTC = rep("2015-12-31", 5),
#'                  LBTESTCD = letters[1:5],
#'                  LBORRES = 1:5,
#'                  stringsAsFactors = FALSE)
#'
#' check_lb_lbdtc_after_dd(AE, DS, LB)
#'
#' LB$LBDTC[1] <- "2016-01-03"
#' LB$USUBJID[1] <- LB$USUBJID[5]
#'
#' check_lb_lbdtc_after_dd(AE, DS, LB)
#'
#'

check_lb_lbdtc_after_dd <- function(AE, DS, LB) {

    lacks_msgs <- NULL # For storing lack_msg from all required SDTM datasets

    if (AE %lacks_any% c("USUBJID", "AEDTHDTC", "AESTDTC", "AEDECOD", "AETERM")) {

        lacks_msgs <- c(lacks_msgs, lacks_msg(AE, c("USUBJID", "AEDTHDTC", "AESTDTC", "AEDECOD", "AETERM")))

    }

    if (DS %lacks_any% c("USUBJID", "DSSTDTC", "DSDECOD", "DSTERM")) {

        lacks_msgs <- c(lacks_msgs, lacks_msg(DS, c("USUBJID", "DSSTDTC", "DSDECOD", "DSTERM")))

    }

    if (LB %lacks_any% c("USUBJID", "LBDTC", "LBTESTCD", "LBORRES")) {

        lacks_msgs <- c(lacks_msgs, lacks_msg(LB, c("USUBJID", "LBDTC", "LBTESTCD", "LBORRES")))

    }

    if (length(lacks_msgs) > 0) {

        fail(msg = paste0(lacks_msgs, collapse = ". "))

    } else {

        AE$AEDTHDTC <- impute_day01(AE$AEDTHDTC)
        AE$AESTDTC <- impute_day01(AE$AESTDTC)
        DS$DSSTDTC <- impute_day01(DS$DSSTDTC)
        LB$LBDTC <- impute_day01(LB$LBDTC)

        # Get earliest death date by USUBJID
        ae_dd <- AE %>%
            select(USUBJID, AEDTHDTC) %>%
            filter(!is_sas_na(AEDTHDTC)) %>%
            arrange(USUBJID, AEDTHDTC)

        ae_dd <- unique(ae_dd)

        ds_dd <- DS %>%
            filter((grepl("DEATH", DSDECOD, ignore.case = TRUE) | grepl("DEATH", DSTERM, ignore.case = TRUE)),
                   !is_sas_na(DSSTDTC)) %>%
            select(USUBJID, DSSTDTC) %>%
            arrange(USUBJID, DSSTDTC)

        ds_dd <- unique(ds_dd)

        death_dates <- full_join(ae_dd, ds_dd, by = "USUBJID")


        if (nrow(death_dates) == 0) {

            pass() # If no death dates, then check automatically passes

        } else {

            death_dates <- death_dates %>%
                mutate(EARLIEST_DTHDTC = pmin(as.Date(AEDTHDTC), as.Date(DSSTDTC), na.rm = T))

            suppressWarnings(
                mydf0 <- LB %>%
                    filter(USUBJID %in% death_dates[["USUBJID"]], !is_sas_na(LBDTC), !is_sas_na(LBORRES)) %>%
                    select(USUBJID, LBDTC, LBTESTCD) %>%
                    left_join(death_dates, by = "USUBJID")
            )
            mydf <- mydf0 %>%
                filter(as.Date(EARLIEST_DTHDTC) < as.Date(LBDTC))

            if (nrow(mydf) == 0) {

                pass()

            } else {

                fail("Patient(s) with LB occurring after death date. ", mydf)

            }
        }
    }
}
