#' @title Check that all LB dates are duplicated or earlier than last
#' visit's (possible datetime data entry error)
#'
#' @description This check identifies LBDTC values that are duplicated or
#' earlier than last visit's. Records with LBSTAT == 'NOT DONE' and unscheduled
#' visits (VISIT with the string "UNSCHEDU") and treatment discon visits
#' (VISIT with the string "TREATMENT OR OBSERVATION FU COMP EARLY DISC") are excluded.
#'
#' @param LB SDTM dataset with variables USUBJID, VISITNUM, VISIT, LBDTC, LBTESTCD, LBSTAT
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author Simon Luo
#'
#' @examples
#'
#' # no case
#' LB1 <- data.frame(USUBJID = c(rep("101", 5), rep("102", 5)),
#'                 LBCAT = "Hematology",
#'                 LBDTC = rep(c(
#'                 "2017-01-01T08:25",
#'                 "2017-01-05T09:25",
#'                 "2017-01-15T10:25",
#'                 "2017-01-20T08:25",
#'                 "2017-01-25T08:25"), 2),
#'                 VISITNUM = rep(1:5,2),
#'                 VISIT = rep(c(
#'                 "Visit 1",
#'                 "Visit 2",
#'                 "Visit 3",
#'                 "UNSCheduled!!!",
#'                 "VIsit 5"), 2),
#'                 LBSTAT = c(rep("", 9), "NOT DONE"),
#'                 stringsAsFactors = FALSE)
#'
#' check_lb_lbdtc_visit_ordinal_error(LB1)
#'
#' LB2 = LB1
#' LB2$LBCAT = "Virology"
#' LB3 <- rbind(LB1, LB2)
#' check_lb_lbdtc_visit_ordinal_error(LB3)
#'
#' # adding cases with earlier date
#' LB3$LBDTC[LB3$USUBJID == 101 & LB3$VISIT == "Visit 3"] <- "2016-01-10T08:25"
#' LB3$LBDTC[LB3$USUBJID == 102 & LB3$VISIT == "Visit 2"] <- "2016-01-01T06:25"
#' check_lb_lbdtc_visit_ordinal_error(LB = LB3)
#'
#' # adding cases with duplicated date
#' LB3$LBDTC[LB3$USUBJID == 102 & LB3$VISIT == "Visit 3"] <- "2017-01-15T10:25"
#' LB3 <- LB3[order(LB3$USUBJID, LB3$VISITNUM, LB3$LBDTC),]
#' check_lb_lbdtc_visit_ordinal_error(LB = LB3)
#'
#' # check if all NOT DONE
#' LB4 = LB3
#' LB4$LBSTAT = "NOT DONE"
#' check_lb_lbdtc_visit_ordinal_error(LB = LB4)
#'
#' # check dropping a required variable
#' LB4$LBSTAT = NULL
#' check_lb_lbdtc_visit_ordinal_error(LB = LB4)
#'

check_lb_lbdtc_visit_ordinal_error <- function(LB){

    class(LB) <- 'data.frame'
    vars = c("USUBJID", "VISITNUM", "VISIT", "LBDTC", "LBSTAT" )

    ### First check that required variables exist and return a message if they don't
    if (LB %lacks_any% vars) {

        fail(lacks_msg(LB, vars))

        ### Dont run if VISITNUM is all missing
    } else if (length(unique(LB[["VISITNUM"]]))<=1) {

        fail(msg="VISITNUM exists but only a single value. ")

    }  else {

        #only keep records not indicated as NOT DONE and drop Unscheduled and Tx Discon visits
        subsetdf = subset(LB, LB$LBSTAT != "NOT DONE"  & !grepl("UNSCHEDU|TREATMENT OR OBSERVATION FU COMP EARLY DISC", toupper(LB$VISIT)))


        if(nrow(subsetdf)>0){

            mydf2 <- dtc_dupl_early(
                dts = subsetdf,
                vars = vars,
                ### groupby variables used for grouping and visit.order derivation
                groupby = vars[c(1)],
                dtc = vars[4],
                ### variables used for ordering before visit.order derivation
                vars[1],
                vars[2],
                vars[3],
                vars[4]
            )

            ### Subset if Vis_order not equal Dtc_order
            myout <- mydf2[!is.na(mydf2$check.flag), ]

            ### Add sorting
            myout <- myout[order(myout$USUBJID, myout$VISITNUM, myout$LBDTC),]

            ### Print to report

            ### Return message if no records with QSDTC per VISITNUM
            if (nrow(myout) == 0) {
                pass()
                ### Return subset dataframe if there are records with Possible LBDTC data entry error
            } else if (nrow(myout) > 0) {
                rownames(myout) = NULL
                fail(paste("LB has ", nrow(myout), " record(s) with Possible LBDTC data entry error. ", sep = ""), myout)
            }

        } else{fail("No lab records when subset to exclude NOT DONE.")}

    }
}

