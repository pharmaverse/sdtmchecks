#' @title Check for EX dates occurring after death date
#'
#' @description This check looks for EX dates that occur after death date
#'
#' @param AE Adverse Event SDTM dataset with variables USUBJID,
#'   AEDTHDTC, AESTDTC, AEDECOD, and AETERM
#'
#' @param DS Disposition SDTM dataset with variables USUBJID,
#'   DSSTDTC, DSDECOD, and DSTERM
#'
#' @param EX Exposure SDTM dataset with variables USUBJID,
#'   EXSTDTC, EXTRT, and EXDOSE
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
#' EX <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
#'                  EXSTDTC = rep("2015-12-31", 5),
#'                  EXTRT = LETTERS[1:5],
#'                  EXDOSE = 1:5,
#'                  stringsAsFactors = FALSE)
#'
#' check_ex_exstdtc_after_dd(AE, DS, EX)
#'
#' EX$EXSTDTC[1] <- "2016-01-03"
#' EX$USUBJID[1] <- EX$USUBJID[5]
#'
#' check_ex_exstdtc_after_dd(AE, DS, EX)
#'
#'

check_ex_exstdtc_after_dd <- function(AE, DS, EX) {

    lacks_msgs <- NULL # For storing lack_msg from all required SDTM datasets

    if (AE %lacks_any% c("USUBJID", "AEDTHDTC", "AESTDTC", "AEDECOD", "AETERM")) {

        lacks_msgs <- c(lacks_msgs, lacks_msg(AE, c("USUBJID", "AEDTHDTC", "AESTDTC", "AEDECOD", "AETERM")))

    }

    if (DS %lacks_any% c("USUBJID", "DSSTDTC", "DSDECOD", "DSTERM")) {

        lacks_msgs <- c(lacks_msgs, lacks_msg(DS, c("USUBJID", "DSSTDTC", "DSDECOD", "DSTERM")))

    }

    if (EX %lacks_any% c("USUBJID", "EXSTDTC", "EXTRT", "EXDOSE")) {

        lacks_msgs <- c(lacks_msgs, lacks_msg(EX, c("USUBJID", "EXSTDTC", "EXTRT", "EXDOSE")))

    }

    if (length(lacks_msgs) > 0) {

        fail(msg = paste0(lacks_msgs, collapse = ". "))

    } else {

        AE$AEDTHDTC <- impute_day01(AE$AEDTHDTC)
        AE$AESTDTC <- impute_day01(AE$AESTDTC)
        DS$DSSTDTC <- impute_day01(DS$DSSTDTC)
        EX$EXSTDTC <- impute_day01(EX$EXSTDTC)

        # Get earliest death date by USUBJID
        ae_dd <- AE %>%
            select(USUBJID, AEDTHDTC) %>%
            filter(!is_sas_na(AE[["AEDTHDTC"]])) %>%
            arrange(USUBJID, AEDTHDTC)

        ae_dd <- unique(ae_dd)

        ds_dd <- DS %>%
            filter((grepl("DEATH", DS$DSDECOD, ignore.case = TRUE) |
                        grepl("DEATH", DS$DSTERM, ignore.case = TRUE)),
                   !is_sas_na(DS[["DSSTDTC"]])) %>%
            select(USUBJID, DSSTDTC) %>%
            arrange(USUBJID, DSSTDTC)

        ds_dd <- unique(ds_dd)

        death_dates <- full_join(ae_dd, ds_dd, by = "USUBJID")


        if (nrow(death_dates) == 0) {

            pass() # If no death dates, then check automatically passes

        } else {

            death_dates <- death_dates %>%
                mutate(EARLIEST_DTHDTC = pmin(as.Date(death_dates$AEDTHDTC), as.Date(death_dates$DSSTDTC), na.rm = T))

            df <- EX %>%
                filter(EX$USUBJID %in% death_dates[["USUBJID"]],
                       !is_sas_na(EX$EXSTDTC),
                       !is_sas_na(EX$EXTRT),
                       !is_sas_na(EX$EXDOSE))

            if ("EXOCCUR" %in% names(EX)) {

                df <- df %>% filter(df$EXOCCUR == "Y")

            }

            suppressWarnings(
                df <- df %>% select("USUBJID", "EXSTDTC") %>%
                    left_join(death_dates, by = "USUBJID")
            )
            df <- df %>%
                filter(as.Date(df$EARLIEST_DTHDTC) < as.Date(df$EXSTDTC))

            if (nrow(df) == 0) {

                pass()

            } else {

                fail("Patient(s) with EX occurring after death date. ", df)

            }
        }
    }
}
