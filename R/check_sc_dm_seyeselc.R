#' @title Check SC Study Eye Selection assignments among DM patients
#'
#' @description Check if SC.SCCAT = "STUDY EYE SELECTION" and SC.SCTESTCD = "FOCID",
#' then SC.SCORRES should have "OS", "OD", or "OU" values. Flag if subject is in
#' DM and without an associated SC.SCORRES value or the STUDY EYE SELECTION value
#' is not "OS", "OD", or "OU".
#'
#' @param DM Subject Demographics SDTM dataset with variable USUBJID
#' @param SC Subject Characteristics SDTM dataset for Ophtho Study with variables
#' USUBJID, SCTESTCD, SCTEST, SCCAT, SCORRES, SCDTC
#'
#' @importFrom dplyr %>% filter mutate select
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @family OPHTH
#'
#' @keywords OPHTH
#'
#' @author Monarch Shah (HackR 2021 Team Eye)
#'
#' @examples
#'
#' dm <- data.frame(USUBJID = c(1,2))
#' sc <- data.frame(USUBJID  = c(1,1,1,2,2,2),
#'                  SCTEST   = c("Eye Meeting Eligibility Criteria",
#'                               "Focus of Study-Specific Interest",
#'                               " ",
#'                               "Eye Meeting Eligibility Criteria",
#'                               "Focus of Study-Specific Interest", " "),
#'                  SCTESTCD = c("ELIGEYE", "FOCID", "", "ELIGEYE", "FOCID", ""),
#'                  SCCAT    = c("STUDY EYE SELECTION",
#'                               "STUDY EYE SELECTION",
#'                               "",
#'                               "STUDY EYE SELECTION",
#'                               "STUDY EYE SELECTION", ""),
#'                  SCORRES  = c("LEFT", "OS", "", "RIGHT", "OD", ""),
#'                  SCDTC    = rep("2021-01-01", 6),
#'                  stringsAsFactors = FALSE)
#'
#' check_sc_dm_seyeselc(SC=sc, DM=dm)
#'
#' dm <- data.frame(USUBJID = c(1,2,3,4))
#' sc <- data.frame(USUBJID  = c(1,1,1,2,2,2),
#'                  SCTEST   = c("Eye Meeting Eligibility Criteria",
#'                               "Focus of Study-Specific Interest",
#'                               " ",
#'                               "Eye Meeting Eligibility Criteria",
#'                               "Focus of Study-Specific Interest",
#'                               " "),
#'                  SCTESTCD = c("ELIGEYE", "FOCID", "", "ELIGEYE", "FOCID", ""),
#'                  SCCAT    = c("STUDY EYE SELECTION",
#'                               "STUDY EYE SELECTION",
#'                               "",
#'                               "STUDY EYE SELECTION",
#'                               "STUDY EYE SELECTION", ""),
#'                  SCORRES  = c("LEFT", "OS", "", "RIGHT", "", ""),
#'                  SCDTC    = rep("2021-01-01", 6),
#'                  stringsAsFactors = FALSE)
#'
#' check_sc_dm_seyeselc(SC=sc, DM=dm)
#'
#'


check_sc_dm_seyeselc <- function(DM, SC) {

    if (DM %lacks_any% c("USUBJID")) {

        fail(lacks_msg(DM, c("USUBJID")))

    }

    else if (SC %lacks_any% c("USUBJID", "SCTEST", "SCTESTCD", "SCCAT", "SCORRES", "SCDTC")) {

        fail(lacks_msg(SC, c("USUBJID", "SCTEST", "SCTESTCD", "SCCAT", "SCORRES", "SCDTC")))
    }

    else {

        DM = DM %>% select(USUBJID)

        SC = SC %>% select(USUBJID, SCTESTCD, SCTEST, SCCAT, SCORRES, SCDTC) %>%
                          mutate(MISFLAG =  ifelse((grepl("FOCID", SCTESTCD, ignore.case=TRUE) == TRUE) &
                                                    (!(SCORRES %in% c("OD", "OS"))), 1, 0))

        mydf = left_join(DM, SC, by="USUBJID")

        mydf = mydf %>% filter(MISFLAG == 1 | is.na(SCORRES)) %>% select(-MISFLAG)

        if ((nrow(mydf) > 0 ) == FALSE) {
            pass()
        } else {
            fail(paste0(nrow(mydf), " record(s) with Study Eye Selection missing. "), mydf )
        }
    }
}

