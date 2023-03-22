#' @title Check DS with multiple death records with death dates, where death dates do not match
#'
#' @description This check looks for patients in DS who have multiple records indicating
#'   death, with non-missing mismatching death dates in DSSTDTC.
#'   
#' @param DS Disposition SDTMv dataset with variables USUBJID, DSDECOD,
#'   DSSCAT and DSSTDTC
#'
#' @return Boolean value for whether the check passed or failed, with 'msg'
#'   attribute if the test failed
#'
#' @importFrom dplyr %>% filter select
#'
#' @export
#'
#' @author Kimberly Fernandes
#'
#' @examples
#'
#' DS_error1 <- data.frame(STUDYID = rep(1, 6),
#'   USUBJID = c(1, 1, 1, 2, 1,1),
#'   DSDECOD = c("DEATH", "DEATH", rep("", 2),"DEATH","DEATH"),
#'   DSSCAT = LETTERS[1:6],
#'   DSSTDTC = c("", "2016-01-01", "", "", "2016-01-02","2016-01-01"),
#'   stringsAsFactors = FALSE)
#'
#' DS_error2 <- data.frame(STUDYID = rep(1, 6),
#'   USUBJID = c(1, 1, 1, 2, 1,1),
#'   DSDECOD = c("DEATH", "DEATH", rep("", 2),"DEATH","DEATH"),
#'   DSSCAT = LETTERS[1:6],
#'   DSSTDTC = c("", "2016-01", "", "", "2016-01-01","2016-01-01"),
#'   stringsAsFactors = FALSE)
#'   
#'  DS_noerror <- data.frame(STUDYID = rep(1, 6),
#'                         USUBJID = c(1, 1, 1, 2, 1,1),
#'                         DSDECOD = c("DEATH", "DEATH", rep("", 2),"DEATH","DEATH"),
#'                         DSSCAT = LETTERS[1:6],
#'                         DSSTDTC = c("", "2016-01-01", "", "", "2016-01-01","2016-01-01"),
#'                         stringsAsFactors = FALSE)
#'
#' check_ds_multdeath_dsstdtc(DS_error1)
#' check_ds_multdeath_dsstdtc(DS_error2)
#' check_ds_multdeath_dsstdtc(DS_noerror)


check_ds_multdeath_dsstdtc <- function(DS) {
  
  if (DS %lacks_any% c("USUBJID", "DSDECOD", "DSSCAT", "DSSTDTC")) {
    
    fail(lacks_msg(DS, c("USUBJID", "DSDECOD", "DSSCAT", "DSSTDTC")))
    
  } else {
    
    #Get all records with a death date
    death_dates <- DS %>%
      filter(DSDECOD == "DEATH" & !is_sas_na(DSSTDTC)) %>%
      select("USUBJID","DSDECOD","DSSTDTC") 
    
    #Get all patients where death dates don't match
    df <- death_dates %>%
      group_by(USUBJID) %>%
      filter(n() >=2, length(unique(DSSTDTC))>1)
    
      if (nrow(df)==0) {
      
      pass()
      
    } else {
      
      fail("DS has multiple non-missing death dates in DSSTDTC that do not match. ", df)
      
    }
  }
}
