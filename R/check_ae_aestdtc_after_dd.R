#' @title Check for AE dates occurring after death date
#'
#' @description This check looks for AE dates that occur after death date
#'
#' @param AE Adverse Event SDTM dataset with variables USUBJID,
#'   AEDTHDTC, AESTDTC, AEDECOD, AETERM, AESPID (optional)
#' @param DS Disposition SDTM dataset with variables USUBJID,
#'   DSSTDTC, DSDECOD, DSTERM, DSSPID (optional)
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% arrange filter left_join full_join mutate select slice everything
#' @importFrom tidyselect any_of
#'
#' @return Boolean value for whether the check passed or failed, with 'msg'
#'   attribute if the check failed
#'
#' @export
#'

#' @author Nina Ting Qi
#'
#' @examples
#'
#' AE <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
#'                  AEDTHDTC = c(rep("", 4), "2016-01-01"),
#'                  AESTDTC = rep("2016-01-01", 5),
#'                  AEDECOD = c("","", rep("Myocarditis",3)),
#'                  AETERM = c("INJURY", rep("MYOCARDITIS", 4)),
#'                  AESPID = "FORMNAME-R:19/L:19XXXX",
#'                  stringsAsFactors = FALSE)
#'
#' DS <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
#'                  DSSTDTC = rep("2016-01-02", 5),
#'                  DSDECOD = c(LETTERS[1:4], "death"),
#'                  DSSPID = "XXX-R:0",
#'                  DSTERM = letters[1:5],
#'                  stringsAsFactors = FALSE)
#'
#' check_ae_aestdtc_after_dd(AE,DS)
#'
#' AE$AESTDTC[1] <- "2016-01-03"
#' AE$USUBJID[1] <- AE$USUBJID[5]
#'
#' check_ae_aestdtc_after_dd(AE, DS,preproc=roche_derive_rave_row)
#'
#' AE$AESPID <- NULL
#' check_ae_aestdtc_after_dd(AE, DS)
#'
#' DS$DSSPID <- NULL
#' check_ae_aestdtc_after_dd(AE, DS)
#'
#' AE$AESTDTC <- NULL
#' check_ae_aestdtc_after_dd(AE, DS)
#'

check_ae_aestdtc_after_dd <- function(AE, DS, preproc=identity,...) {

    lacks_msgs <- NULL # For storing lack_msg from all required SDTM datasets

    if (AE %lacks_any% c("USUBJID", "AEDTHDTC", "AESTDTC", "AEDECOD", "AETERM")) {

        lacks_msgs <- c(lacks_msgs, lacks_msg(AE, c("USUBJID", "AEDTHDTC", "AESTDTC", "AEDECOD", "AETERM")))

    }

    if (DS %lacks_any% c("USUBJID", "DSSTDTC", "DSDECOD", "DSTERM")) {

        lacks_msgs <- c(lacks_msgs, lacks_msg(DS, c("USUBJID", "DSSTDTC", "DSDECOD", "DSTERM")))

    }

    if (length(lacks_msgs) > 0) {

        fail(msg = paste0(lacks_msgs, collapse = ". "))

    } else {

        #Apply company specific preprocessing function
        AE = preproc(AE,...)
        DS = preproc(DS,...)

        AE$AEDTHDTC <- impute_day01(AE$AEDTHDTC)
        AE$AESTDTC <- impute_day01(AE$AESTDTC)
        DS$DSSTDTC <- impute_day01(DS$DSSTDTC)

        ### Subset AE to fewer variables
        AE <- AE %>%
            select(any_of(c("USUBJID", "AETERM", "AEDECOD", "AEDTHDTC", "AESTDTC","RAVE")))


        # Get earliest death date by USUBJID
        ae_dd <- AE %>%
            select(USUBJID, AEDTHDTC) %>%
            filter(!is_sas_na(AE[["AEDTHDTC"]])) %>%
            arrange(USUBJID, AEDTHDTC)

        ae_dd <- unique(ae_dd)



        ### Subset DS to fewer variables
        DS <- DS %>%
            select(any_of(c("USUBJID", "RAVE", "DSSTDTC", "DSDECOD", "DSTERM","RAVE")))

        ds_dd <- DS %>%
            filter((grepl("DEATH", DS$DSDECOD, ignore.case = TRUE) | grepl("DEATH", DS$DSTERM, ignore.case = TRUE)),
                   !is_sas_na(DS[["DSSTDTC"]])) %>%
            select(-DSDECOD, -DSTERM)

        ds_dd <- unique(ds_dd)

        death_dates <- full_join(ae_dd, ds_dd, by = "USUBJID")

        if (nrow(death_dates) == 0) {

            pass() # If no death dates, then check automatically passes

        } else {

            death_dates <- death_dates %>%
                mutate(EARLIEST_DTHDTC = pmin(as.Date(death_dates$AEDTHDTC), as.Date(death_dates$DSSTDTC), na.rm = T))

            df1 <- AE %>%
                filter(AE$USUBJID %in% death_dates[["USUBJID"]], !is_sas_na(AE$AESTDTC)) %>%
                select(-AEDECOD, -AEDTHDTC)

            suppressWarnings(
                df2 <- df1 %>%
                    left_join(death_dates, by = "USUBJID", suffix = c(".AE", ".DS"))
            )
            df <- df2 %>%
                filter(as.Date(df2$EARLIEST_DTHDTC) < as.Date(df2$AESTDTC)) %>%
                select(USUBJID, AETERM, AESTDTC, AEDTHDTC, everything())

            if (nrow(df) == 0) {

                pass()

            } else {

                fail(paste0(length(unique(df$USUBJID)),
                            " patient(s) with AE occurring after death date. "), df)
            }
        }
    }
}
