#' @title Check that all ECG datetimes are earlier than last
#' visit's (possible datetime data entry error)
#'
#' @description This check identifies EGDTC values that are
#' earlier than last visit's. Unscheduled visits are excluded.
#'
#' @param EG ECG Test Results SDTM dataset with variables USUBJID, VISITNUM, VISIT, EGDTC, EGSTAT
#'
#' @return boolean value if check failed or passed with 'msg' attribute if
#' the test failed
#'
#' @export
#'
#' @author James Zhang
#'
#' @examples
#'
#' # No case
#' EG<- data.frame(USUBJID = 101:102,
#'                 EGDTC=rep(c("2017-01-01T08:25", "2017-01-05T09:25",
#'                 "2017-01-15T10:25", "2017-01-20T08:25", "2017-01-25T08:25"), 2),
#'                 VISITNUM=rep(1:5,2),
#'                 VISIT=rep(c("Screening", "Cycle 1", "Cycle 2",
#'                 "Cycle 3","UNschedUled"),2),
#'                 EGSTAT="",
#'                 stringsAsFactors=FALSE)
#' check_eg_egdtc_visit_ordinal_error(EG)
#'
#' # Cases with earlier datetime
#' EG$EGDTC[EG$USUBJID == 101 & EG$VISIT == "Cycle 3"] <- "2017-01-10T08:25"
#' EG$EGDTC[EG$USUBJID == 102 & EG$VISIT == "Cycle 1"] <- "2017-01-01T06:25"
#' check_eg_egdtc_visit_ordinal_error(EG)
#'
#' # Cases with duplicated datetime
#' EG$EGDTC[EG$USUBJID == 101 & EG$VISIT == "Cycle 3"] <- "2017-01-15T10:25"
#' EG$EGDTC[EG$USUBJID == 102 & EG$VISIT == "Cycle 2"] <- "2017-01-01T06:25"
#' check_eg_egdtc_visit_ordinal_error(EG)
#'
#' # Not checking duplicates
#' EG<- data.frame(USUBJID = rep("101",6),
#'                 EGDTC=rep("2017-01-01T08:25", 6),
#'                 VISITNUM=rep(1:2,3),
#'                 VISIT=rep("Screening",6),
#'                 EGSTAT="",
#'                 stringsAsFactors=FALSE)
#'
#' check_eg_egdtc_visit_ordinal_error(EG)
#'


check_eg_egdtc_visit_ordinal_error <- function(EG){

    class(EG) <- 'data.frame'
    vars = c("USUBJID", "VISITNUM", "VISIT", "EGDTC","EGSTAT")

    ### First check that required variables exist and return a message if they don't
    if (EG %lacks_any% vars) {

        fail(lacks_msg(EG, vars))

    ### Dont run if VISITNUM is all missing
    } else if (length(unique(EG[["VISITNUM"]]))<=1) {

        fail(msg="VISITNUM exists but only a single value. ")

    } else {

        myout <- as.data.frame(dtc_dupl_early(dts = subset(EG,EG$EGSTAT !="NOT DONE" & !grepl("UNSCHEDU",toupper(EG$VISIT)),), vars = vars,
                                ### groupby variables used for grouping and visit.order derivation
                                groupby = vars[c(1)],
                                dtc = vars[4],
                                ### variables used for ordering before visit.order derivation
                                vars[1], vars[2], vars[3], vars[4]))

        if(nrow(myout)>0){
        ### Subset if Vis_order not equal Dtc_order
        myout <- myout[!is.na(myout$check.flag), ]

        ### Remove dup checking, its finding screening/cycle 1 have same date which is acceptable in some situations
        myout = subset(myout,myout$check.flag != "Duplicated",)
        }

        ###Print to report

        ### Return message if no records with EGDTC per VISITNUM
        if (nrow(myout) == 0) {
            pass()
            ### Return subset dataframe if there are records with Possible EGDTC data entry error
        } else if (nrow(myout) > 0 ) {
            rownames(myout) = NULL
            fail(paste("EG has ",nrow(myout)," records with Possible EGDTC data entry error. ", sep = ""), myout)
        }
    }
}
