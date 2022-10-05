#' @title Check to confirm that there is a reason for a questionnaire being marked as not done
#'
#' @description This code flags when QSSTAT Completion Status is marked as NOT DONE but QSREASND Reason Not Performed is not populated.
#'             Some but not all questionnaires in a study may collect Reason Not Performed information, so there may be instances of
#'             false positives in which no data correction is required. While QSREASND is a permissible variable, this scenario will be
#'             flagged in P21.
#'
#' @param QS Questionnaire SDTMv dataset with USUBJID, QSCAT, QSDTC, QSSTAT, QSREASND, VISIT (optional) variables
#'
#' @return boolean value if check returns 0 obs, otherwise return subset dataframe.
#'
#' @export
#'
#' @author Katie Patel, Bonita Viegas Monteiro, Tom Stone (HackR 2021 Team WeRawesome)
#'
#' @examples
#'
#' QS <- data.frame(USUBJID = c(1,1,1,2,2,2),
#'                  QSDTC   = c("2015-06-30", "2015-09-30", "2015-12-30", 
#'                              "2015-06-30", "2015-09-30", "2015-12-30"),
#'                  QSCAT   = "A",
#'                  VISIT  =  c("Week 1", "Week 12", "Week 24", "Week 1", "Week 12", "Week 24"),
#'                  QSSTAT  = c("Not Done","NOT DONE","not done", rep("",3)),
#'                  QSREASND = c("Reasons",rep("",5)),
#'                  stringsAsFactors = FALSE)
#'
#'  check_qs_qsstat_qsreasnd(QS)
#'
#'  QS$QSSTAT=NULL
#'
#'  check_qs_qsstat_qsreasnd(QS)
#'
#'


check_qs_qsstat_qsreasnd <- function(QS) {

    if (QS %lacks_any% c("USUBJID", "QSCAT", "QSDTC", "QSSTAT", "QSREASND")) {
        fail(lacks_msg(QS, c("USUBJID", "QSCAT", "QSDTC", "QSSTAT", "QSREASND")))
    }
    else {

        select_vars = intersect(names(QS),c("USUBJID", "QSCAT", "QSDTC","VISIT", "QSSTAT", "QSREASND"))

        check_results <- subset(QS,
                                trimws(toupper(QSSTAT)) == 'NOT DONE' & is_sas_na(QSREASND),
                                select = select_vars)
        
        if (nrow(check_results) == 0) {
            pass()
        }
        else {
            return(fail(paste(
                "Completion status for",
                nrow(check_results),
                "record(s) is 'NOT DONE' but Reason Not Performed not given. "), 
                check_results))
        }
    }
}
