#' @title Check that all QS dates are duplicated or earlier than last
#' visit's (possible datetime data entry error)
#'
#' @description This check identifies QSDTC values that are duplicated or
#' earlier than last visit's. Unscheduled visits are excluded.
#'
#' @param QS SDTM dataset with variables USUBJID, QSCAT, QSORRES, VISITNUM, VISIT, QSDTC
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
#' QS1 <- data.frame(USUBJID = c(rep(101, 5), rep(102, 5)),
#'                 QSCAT = "DLQI",
#'                 QSDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
#'                  "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
#'                 VISITNUM = rep(1:5,2),
#'                 VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "UNSCheduled!!!","VIsit 5"), 2),
#'                 stringsAsFactors = FALSE)
#'
#' QS2 = QS1
#' QS2$QSCAT = "SKINDEX-29"
#'
#' QS <- rbind(QS1, QS2)
#' check_qs_qsdtc_visit_ordinal_error(QS)
#'
#' # adding cases with earlier date
#' QS$QSDTC[QS$USUBJID == 101 & QS$VISIT == "Visit 3"] <- "2017-01-10T08:25"
#' QS$QSDTC[QS$USUBJID == 102 & QS$VISIT == "Visit 2"] <- "2017-01-01T06:25"
#' check_qs_qsdtc_visit_ordinal_error(QS)
#'
#' # adding cases with duplicated date
#' QS$QSDTC[QS$USUBJID == 102 & QS$VISIT == "Visit 3"] <- "2017-01-01T06:25"
#'  check_qs_qsdtc_visit_ordinal_error(QS)
#'

check_qs_qsdtc_visit_ordinal_error <- function(QS){
    class(QS) <- 'data.frame'
    vars = c("USUBJID", "QSCAT", "VISITNUM", "VISIT", "QSDTC")
    ### First check that required variables exist and return a message if they don't
    if (QS %lacks_any% vars) {
        
        fail(lacks_msg(QS, vars))
        
        ### Dont run if VISITNUM is all missing
    } else if (length(unique(QS[["VISITNUM"]]))<=1) {
        
        fail(msg="VISITNUM exists but only a single value. ")
        
    }  else {
        
        mydf2 <- dtc_dupl_early(
            dts = subset(QS,!grepl("UNSCHEDU",toupper(QS$VISIT)),),
            vars = vars,
            ### groupby variables used for grouping and visit.order derivation
            groupby = vars[c(1, 2)],
            dtc = vars[5],
            ### variables used for ordering before visit.order derivation
            vars[1],
            vars[2],
            vars[3],
            vars[4],
            vars[5]
        )
        
        ### Subset if Vis_order not equal Dtc_order
        myout <- mydf2[!is.na(mydf2$check.flag), ]
        
        ### Print to report
        
        ### Return message if no records with QSDTC per VISITNUM
        if (nrow(myout) == 0) {
            pass()
            ### Return subset dataframe if there are records with Possible QSDTC data entry error
        } else if (nrow(myout) > 0) {
            rownames(myout) = NULL
            fail(paste("QS has ", nrow(myout), " records with Possible QSDTC data entry error. ", sep = ""), myout)
        }
    }
}