#' @title Check that all TU  dates are duplicated or earlier than last
#' visit's (possible datetime data entry error)
#'
#' @description This check identifies TUDTC values that are duplicated or
#' earlier than last visit's. Unscheduled visits are excluded.
#'
#' @param TU Tumor Identification SDTM dataset with variables USUBJID,TUORRES ,TULOC, VISITNUM, VISIT, TUDTC, TUEVAL
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author Jingyuan Chen
#'
#' @examples
#'
#' # no case
#' TU <- data.frame(USUBJID = 101:102,
#'                 TUORRES = rep(c("NEW", "TARGET"), 5),
#'                 TULOC=rep(c("BONE","LIVER"),5),
#'                 TUDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
#'                  "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
#'                 VISITNUM = rep(1:5,2),
#'                 VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "Visit 4","VIsit 5"), 2),
#'                 TUEVAL="INVESTIGATOR",
#'                 stringsAsFactors = FALSE)
#' check_tu_tudtc_visit_ordinal_error(TU)
#'
#' # adding cases with earler date
#' TU$TUDTC[TU$USUBJID == 101 & TU$VISIT == "Visit 4"] <- "2017-01-10T08:25"
#' TU$TUDTC[TU$USUBJID == 102 & TU$VISIT == "Visit 2"] <- "2017-01-01T06:25"
#' check_tu_tudtc_visit_ordinal_error(TU)
#'
#' # adding cases with duplicated date
#' TU$TUDTC[TU$USUBJID == 101 & TU$VISIT == "Visit 5"] <- "2017-01-10T08:25"
#' TU$TUDTC[TU$USUBJID == 102 & TU$VISIT == "Visit 3"] <- "2017-01-01T06:25"
#'  check_tu_tudtc_visit_ordinal_error(TU)


check_tu_tudtc_visit_ordinal_error <- function(TU){
    class(TU) <- 'data.frame'
    vars = c("USUBJID", "TUORRES","TULOC", "VISITNUM", "VISIT", "TUDTC","TUEVAL")
    ### First check that required variables exist and return a message if they don't
    if (TU %lacks_any% vars) {

        fail(lacks_msg(TU, vars))

    ### Dont run if VISITNUM is all missing
    } else if (length(unique(TU[["VISITNUM"]]))<=1) {

        fail(msg="VISITNUM exists but only a single value. ")

    }    else {

        subsetdf = subset(TU, TU$TUEVAL=="INVESTIGATOR" & !grepl("UNSCHEDU",toupper(TU$VISIT)),)

        if(nrow(subsetdf)>0){

        mydf2 <- dtc_dupl_early(
            dts = subsetdf,
            vars = vars,
            ### groupby variables used for grouping and visit.order derivation
            groupby = vars[c(1, 2, 3)],
            dtc = vars[6],
            ### variables used for ordering before visit.order derivation
            vars[1],
            vars[2],
            vars[3],
            vars[4],
            vars[5],
            vars[6]
        )

        ### Subset if Vis_order not equal Dtc_order
        myout <- mydf2[!is.na(mydf2$check.flag), ]

        ### Print to report

        ### Return message if no records with EXSTDTC per VISITNUM
        if (nrow(myout) == 0) {
            pass()
            ### Return subset dataframe if there are records with Possible EXSTDTC data entry error
        } else if (nrow(myout) > 0) {
            rownames(myout) = NULL
            fail(paste("TU has ", nrow(myout), " records with Possible TUDTC data entry error. ", sep = ""), myout)
        }

        } else{fail("No records when subset to only INV records. ")}


    }
}
