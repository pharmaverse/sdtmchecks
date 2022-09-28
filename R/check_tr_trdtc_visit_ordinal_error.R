#' @title Check that all TR dates by INV are duplicated or earlier than
#' last visit's (possible date entry error)
#'
#' @description This check identifies TRDTC values when TREVAL == 'INVESTIGATOR'
#'  are duplicated or earlier than last visit's. Unscheduled and 'NOT DONE' visits
#'  are excluded.
#'
#' @param TR Tumor Response Measurement SDTM dataset with variables USUBJID,
#' VISITNUM, VISIT, TRDTC, TREVAL, TRSTAT
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
#' # no case
#' TR<- data.frame(USUBJID = 101:102,
#'                 TRSEQ=rep(1:5,2),
#'                 TRDTC=rep(c("2017-01-01T08:25", "2017-01-05T09:25",
#'                    "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
#'                 VISITNUM=rep(1:5,2),
#'                 VISIT=rep(c("Screening", "Cycle 1", "Cycle 2","Cycle 3","Follow-up"),2),
#'                 TREVAL="INVESTIGATOR",
#'                 TRSTAT="",
#'                 stringsAsFactors=FALSE)
#' check_tr_trdtc_visit_ordinal_error(TR)
#'
#' # Cases with earler datetime
#' TR$TRDTC[TR$USUBJID == 101 & TR$VISIT == "Cycle 3"] <- "2017-01-02T08:25"
#' TR$TRDTC[TR$USUBJID == 102 & TR$VISIT == "Cycle 1"] <- "2017-01-01T06:25"
#' check_tr_trdtc_visit_ordinal_error(TR)
#'

check_tr_trdtc_visit_ordinal_error <- function(TR){

    class(TR) <- 'data.frame'
    vars = c("USUBJID", "VISITNUM", "VISIT", "TRDTC","TREVAL","TRSTAT")

    ### First check that required variables exist and return a message if they don't
    if (TR %lacks_any% vars) {

        fail(lacks_msg(TR, vars))

    ### Dont run if VISITNUM is all missing
    } else if (length(unique(TR[["VISITNUM"]]))<=1) {

        fail(msg="VISITNUM exists but only a single value. ")

    } else {

        subsetdf = subset(TR, TR$TREVAL=="INVESTIGATOR" & TR$TRSTAT !="NOT DONE" & !grepl("UNSCHEDU",toupper(TR$VISIT)),)

        if(nrow(subsetdf)>0){

        mydf2 <- dtc_dupl_early(dts = subsetdf, vars = vars,
                                ### groupby variables used for grouping and visit.order derivation
                                groupby = vars[c(1)],
                                dtc = vars[4],
                                ### variables used for ordering before visit.order derivation
                                vars[1], vars[2], vars[3], vars[4])

        ### Subset if Vis_order not equal Dtc_order
        myout <- mydf2[!is.na(mydf2$check.flag), ]

        ### Different check already doing dups
        myout = subset(myout,myout$check.flag != "Duplicated",)

        ###Print to report

        ### Return message if no records with TRDTC per VISITNUM
        if (nrow(myout) == 0) {
            pass()
            ### Return subset dataframe if there are records with Possible TRDTC data entry error
        } else if (nrow(myout) > 0) {
            rownames(myout) = NULL
            fail(paste("TR has ",nrow(myout)," records with Possible TRDTC data entry error. ",sep = ""), myout)
        }
        } else{fail("No records when subset to only INV records. ")}
    }
}
