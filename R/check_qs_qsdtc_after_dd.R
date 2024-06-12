#' @title Check for QS dates occurring after death date
#'
#' @description This check looks for QS dates that occur after death date
#'
#' @param AE Adverse Event SDTM dataset with variables USUBJID,
#'   AEDTHDTC, AESTDTC, AEDECOD, and AETERM
#'
#' @param DS DS Disposition SDTM dataset with variables USUBJID,
#'   DSSTDTC, DSDECOD, and DSTERM
#'
#' @param QS Questionnaire Test Findings SDTM dataset with variables
#'   USUBJID, QSDTC, QSCAT, and QSORRES
#'
#' @importFrom dplyr %>% arrange filter full_join left_join mutate select n_distinct
#'   distinct case_when
#'
#' @importFrom tidyselect any_of
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @author Monarch Shah
#'
#' @examples
#'
#' AE <- data.frame(USUBJID = c(1,1,1,2,2,2),
#'                  AEDTHDTC = c("", "", "2016-01-01", "", "2016-01", "2016-01-01"),
#'                  AESTDTC = "2016-01-01",
#'                  AEDECOD = LETTERS[1:6],
#'                  AETERM = LETTERS[1:6],
#'                  stringsAsFactors = FALSE)
#'
#' DS <- data.frame(USUBJID = c(1,1,1,2,2,2),
#'                  DSSTDTC = "2016-01-01",
#'                  DSDECOD = c("A", "B", "death", "AC", "BC", "death"),
#'                  DSTERM = letters[1:6],
#'                  stringsAsFactors = FALSE)
#'
#' QS <- data.frame(USUBJID = c(1,1,1,2,2,2),
#'                  QSDTC   = c("2015-06-30", "2015-09-30", "2015-12-30",
#'                              "2015-06-30", "2015-09-30", "2015-12-30"),
#'                  QSCAT   = "A",
#'                  QSORRES =  LETTERS[1:6],
#'                  QSSTAT  = "",
#'                  VISIT  =  c("Week 1", "Week 12", "Week 24", "Week 1", "Week 12", "Week 24"),
#'                  QSSTRESC = LETTERS[1:6],
#'                  stringsAsFactors = FALSE)
#'
#' check_qs_qsdtc_after_dd(AE, DS, QS)
#'
#' QS$QSDTC[3:5] <- "2016-01-03"
#' check_qs_qsdtc_after_dd(AE, DS, QS)
#'
#' QS$QSSTAT[3] <- "Not Done"
#' check_qs_qsdtc_after_dd(AE, DS, QS)
#'
#' DS$DSSTDTC <- NULL
#' check_qs_qsdtc_after_dd(AE, DS, QS)
#'
#' AE1 <- data.frame(USUBJID = 1,
#'                   AEDTHDTC = "",
#'                   AESTDTC = c("2015-11-01", "2016-02-01"),
#'                   AEDECOD = "Rash",
#'                   AETERM = "RASH",
#'                   stringsAsFactors = FALSE)
#'
#' DS1 <- data.frame(USUBJID = 1,
#'                   DSSTDTC = "2016-01",
#'                   DSCAT = c("DISPOSITION EVENT", "OTHER"),
#'                   DSSCAT = c('STUDY COMPLETION/EARLY DISCONTINUATION', ''),
#'                   DSDECOD = "DEATH",
#'                   DSTERM = c("DEATH", "DEATH DUE TO PROGRESSIVE DISEASE"),
#'                   stringsAsFactors = FALSE)
#'
#' QS1 <- data.frame(USUBJID = 1,
#'                   QSDTC   = c("2015-06-30", "2016-01-15", "2016-01-15"),
#'                   QSCAT   = rep("EQ-5D-5L"),
#'                   QSORRES = "1",
#'                   QSSTAT  = "",
#'                   VISIT  =  c("Week 1", "Week 12", "Week 12"),
#'                   QSSTRESC = "1",
#'                   stringsAsFactors = FALSE)
#'
#' check_qs_qsdtc_after_dd(AE=AE1, DS=DS1, QS=QS1)
#'
#' AE1$AEDTHDTC[1:2] <- "2015-07-01"
#' check_qs_qsdtc_after_dd(AE=AE1, DS=DS1, QS=QS1)


check_qs_qsdtc_after_dd <- function(AE, DS, QS) {

    lacks_msgs <- NULL # For storing lack_msg from all required SDTM datasets

    if (AE %lacks_any% c("USUBJID", "AEDTHDTC", "AESTDTC", "AEDECOD", "AETERM")) {
        lacks_msgs <- c(lacks_msgs, lacks_msg(AE, c("USUBJID", "AEDTHDTC", "AESTDTC", "AEDECOD", "AETERM")))
    }
    if (DS %lacks_any% c("USUBJID", "DSSTDTC", "DSDECOD", "DSTERM")) {
        lacks_msgs <- c(lacks_msgs, lacks_msg(DS, c("USUBJID", "DSSTDTC", "DSDECOD", "DSTERM")))
    }
    if (QS %lacks_any% c("USUBJID", "QSDTC", "QSCAT", "QSORRES")) {
        lacks_msgs <- c(lacks_msgs, lacks_msg(QS, c("USUBJID", "QSDTC", "QSCAT", "QSORRES")))
    }

    if (length(lacks_msgs) > 0) {

        fail(msg = paste0(lacks_msgs, collapse = ". "))

    } else {
        # Add day of "01" to dates that are in the format of "yyyy-mm"
        AE$AEDTIMP  <- impute_day01(AE$AEDTHDTC)
        AE$AESTDTIMP<- impute_day01(AE$AESTDTC)
        DS$DSSTDTIMP<- impute_day01(DS$DSSTDTC)
        QS$QSDTC    <- impute_day01(QS$QSDTC)

        # Get earliest death date by USUBJID
        ae_dd <- AE %>%
            filter(!is_sas_na(AEDTIMP)) %>%
            distinct(USUBJID, AEDTHDTC, AEDTIMP) %>%
            arrange(USUBJID, AEDTIMP)

        ds_dd <- DS %>%
            filter((grepl("DEATH", DSDECOD, ignore.case = TRUE) | grepl("DEATH", DSTERM, ignore.case = TRUE)),
                   !is_sas_na(DSSTDTIMP)) %>%
            distinct(USUBJID, DSSTDTC, DSSTDTIMP) %>%
            arrange(USUBJID, DSSTDTIMP)

        death_dates <- full_join(ae_dd, ds_dd, by = "USUBJID")

        if (nrow(death_dates) == 0) {

            pass() # If no death dates, then check automatically passes

        } else {

            death_dates <- death_dates %>%
                mutate(EARLIEST_DTHDT = pmin(as.Date(AEDTIMP), as.Date(DSSTDTIMP), na.rm = T)) %>%
                mutate(EARLIEST_DTHDTC = case_when(
                    as.Date(AEDTIMP) < as.Date(DSSTDTIMP) ~ AEDTHDTC,
                    as.Date(AEDTIMP) > as.Date(DSSTDTIMP) ~ DSSTDTC,
                    TRUE ~ DSSTDTC
                ))

            if  (QS %has_all% c("QSSTAT")) {
            suppressWarnings(
                mydf0 <- QS %>%
                    filter(grepl("NOT DONE", QSSTAT, ignore.case = TRUE) == FALSE & USUBJID %in% death_dates[["USUBJID"]] &
                               !is_sas_na(QSDTC) & !is_sas_na(QSORRES)) %>%
                    left_join(death_dates, by = "USUBJID")
            ) }
            else {
            suppressWarnings(
                mydf0 <- QS %>%
                    filter(USUBJID %in% death_dates[["USUBJID"]], !is_sas_na(QSDTC) , !is_sas_na(QSORRES)) %>%
                    left_join(death_dates, by = "USUBJID")
            ) }

            mydf <- mydf0 %>%
                filter(as.Date(EARLIEST_DTHDT) < as.Date(QSDTC)) %>%
                select(any_of(c("USUBJID",  "QSDTC", "VISIT",  "QSEVAL",  "QSCAT", "AEDTHDTC", "DSSTDTC", "EARLIEST_DTHDTC")))

            if (nrow(mydf) == 0) {
                pass()
            } else {
                fail(paste0(n_distinct(mydf$USUBJID), " unique patient(s) with ", nrow(mydf), " QS record(s) occurring after death date. "), (mydf %>% distinct()))
            }
        }
    }
}


