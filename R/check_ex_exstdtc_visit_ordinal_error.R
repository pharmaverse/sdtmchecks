#' @title Check that all EX start dates are earlier than last
#' visit's (possible datetime data entry error)
#'
#' @description This check identifies EXSTDTC values that are
#' earlier than last visit's. Unscheduled visits are excluded.
#'
#' @param EX Exposure dataset with variables USUBJID, EXTRT, VISITNUM, VISIT, EXDTC, optional variable EXOCCUR
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author James Zhang
#'
#' @examples
#'
#' # no case
#' EX <- data.frame(USUBJID = 101:102,
#'                 EXTRT = rep(c("A", "B"), 5),
#'                 EXSTDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
#'                  "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
#'                 VISITNUM = rep(1:5,2),
#'                 VISIT = rep(c("Cycle 1", "Cycle 2", "Cycle 3", "Cycle 4", "uNscheDuledd"), 2),
#'                 stringsAsFactors = FALSE)
#' check_ex_exstdtc_visit_ordinal_error(EX)
#'
#' # adding cases with earlier date
#' EX$EXSTDTC[EX$USUBJID == 101 & EX$VISIT == "Cycle 4"] <- "2017-01-10T08:25"
#' EX$EXSTDTC[EX$USUBJID == 102 & EX$VISIT == "Cycle 2"] <- "2017-01-01T06:25"
#' check_ex_exstdtc_visit_ordinal_error(EX)
#'
#' # adding cases with duplicated date
#' EX$EXSTDTC[EX$USUBJID == 101 & EX$VISIT == "Cycle 5"] <- "2017-01-10T08:25"
#' EX$EXSTDTC[EX$USUBJID == 102 & EX$VISIT == "Cycle 3"] <- "2017-01-01T06:25"
#' check_ex_exstdtc_visit_ordinal_error(EX)
#'


check_ex_exstdtc_visit_ordinal_error <- function(EX){

    class(EX) <- 'data.frame'
    vars = c("USUBJID", "EXTRT", "VISITNUM", "VISIT", "EXSTDTC")


    ### First check that required variables exist and return a message if they don't
    if (EX %lacks_any% vars) {

        fail(lacks_msg(EX, vars))

    ### Dont run if VISITNUM is all missing
    } else if (length(unique(EX[["VISITNUM"]]))<=1) {

        fail(msg="VISITNUM exists but only a single value")

    } else {
        if ("EXOCCUR" %in% names(EX)) { EX <- subset(EX,EX$EXOCCUR == "Y",)}
        mydf2 <- dtc_dupl_early(dts = subset(EX,!grepl("UNSCHEDU",toupper(EX$VISIT)),), vars = vars,
                                ### groupby variables used for grouping and visit.order derivation
                                groupby = vars[c(1, 2)],
                                dtc = vars[5],
                                ### variables used for ordering before visit.order derivation
                                vars[1], vars[2], vars[3], vars[4], vars[5])

        ### Subset if Vis_order not equal Dtc_order
        myout <- mydf2[!is.na(mydf2$check.flag), ]

        ### Different check already doing dups
        myout = subset(myout,myout$check.flag != "Duplicated",)

        ### Print to report

        ### Return message if no records with EXSTDTC per VISITNUM
        if (nrow(myout) == 0) {
            pass()
            ### Return subset dataframe if there are records with Possible EXSTDTC data entry error
        } else if (nrow(myout) > 0) {
            rownames(myout) = NULL
            fail(paste("EX has ", nrow(myout), " records with Possible EXSTDTC data entry error. ", sep = ""), myout)
        }
    }
}
