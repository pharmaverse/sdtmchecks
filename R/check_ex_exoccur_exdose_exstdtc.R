#' @title Check for Invalid EXDOSE (Dose per Administration) and
#'   Missing/Incomplete EXSTDTC (Start Date) Values for valid exposures
#'
#' @description This check looks for valid exposures (EXOCCUR=Y or doesn't exist) but
#'   EXDOSE (dose per administration) is not > 0 (>= 0 in case of placebo) and/or
#'   EXSTDTC (start date/treatment date) is missing or incomplete in the EX
#'   (exposure) SDTM domain
#'
#' @param EX Exposure SDTM dataset with variables USUBJID, VISIT, VISITNUM,
#'   EXOCCUR, EXTRT, EXDOSE, EXSTDTC and EXENDTC
#'
#' @return Boolean value for whether the check passed or failed, with 'msg'
#'   attribute if the check failed
#'
#' @importFrom dplyr %>% filter mutate select matches
#'
#' @export
#'
#' @author Sara Bodach
#'
#' @examples
#'
#' EX <- data.frame(USUBJID = LETTERS[1:5],
#'                  VISIT = paste0("Visit ", 1:5),
#'                  VISITNUM = 1:5,
#'                  EXOCCUR = c('Y', rep('', 4)),
#'                  EXTRT = LETTERS[1:5],
#'                  EXDOSE = 1:5,
#'                  EXSTDTC = c('2010-01-01', rep('', 4)),
#'                  EXENDTC = c('2010-01-01', rep('', 4)),
#'                  stringsAsFactors = FALSE)
#'
#' EX$EXOCCUR[2] <- 'Y'
#' EX$EXSTDTC[2] <- '2011'
#' EX$EXDOSE[1] <- 0
#'
#' check_ex_exoccur_exdose_exstdtc(EX)
#'
#' EX$VISIT <- NULL
#'
#' check_ex_exoccur_exdose_exstdtc(EX)
#'
#'
check_ex_exoccur_exdose_exstdtc <- function(EX) {

    ###Check that required variables exist and return a message if they don't
    if (EX %lacks_any% c("USUBJID", "VISIT", "VISITNUM", "EXTRT", "EXDOSE", "EXSTDTC", "EXENDTC")) {

        fail(lacks_msg(EX, c("USUBJID", "VISIT", "VISITNUM", "EXTRT", "EXDOSE", "EXSTDTC", "EXENDTC")))

    } else {

        ###First bifurcate EX into a df based on occurrence of EXOCCUR
        if ("EXOCCUR" %in% names(EX)) {
            df <- EX %>% filter(EXOCCUR == "Y")
        }
        else {
            df <- EX
        }

        df$startdate = as.POSIXct(df$EXSTDTC, format = "%Y-%m-%d")

        # Filter by NA date, NA EXDOSE value, or invalid EXDOSE value
        df <- df %>%
            filter(is_sas_na(df$startdate) | is_sas_na(df$EXDOSE) |
                       ((grepl("(PLACEBO|DUMMY)", df$EXTRT) & df$EXDOSE < 0) | (!grepl("(PLACEBO|DUMMY)", df$EXTRT) & df$EXDOSE <= 0))) %>%
            # select based off REGEX matching
            select(matches(
                match =("USUBJID$|VISIT$|VISITNUM$|EXOCCUR$|EXTRT$|EXDOSE$|EXSTDTC$|EXENDTC$")
            ))


    if (nrow(df) != 0){

        fail(paste0("There are ",nrow(df)," EX records with invalid dosing amount or missing full treatment administration date. "), df)

    } else {

        pass()

        }
    }
}


