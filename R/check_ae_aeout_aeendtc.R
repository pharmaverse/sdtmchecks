#' @title Check for inconsistency between AE outcome (AEOUT) and AE end date (AEENDTC)
#'
#' @description This check looks for AEs with a present End date(AEENDTC) but outcome
#' (AEOUT) is not one of the following: "Fatal", "Recovered/Resolved", "Recovered/Resolved with 'xxyyzz'" 
#' If AE End date is missing AEENDTC missing, AEOUT should be one of "Unknown", "Not Recovered/Not Resolved", "Recovering/Resolving"
#' 
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AEENDTC, AEOUT
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#'
#' @export
#'
#' @author Jennifer Lomax
#'
#' @examples
#'
#' AE <- data.frame(
#'USUBJID = 1:9,
#'AEENDTC = c(NA, "NA", "2015-03-12", "2017-01-22",   "1999-11-07",        "",     NA,                   "2017-09-01",          "2015-01-01"),
#'AEOUT   = c("", "",   "",           "NOT RECOVERED","RECOVERED/RESOLVED","FATAL","RECOVERED/RESOLVED", "RECOVERING/RESOLVING","UNKNOWN"),
#'AESPID  = "FORMNAME-R:13/L:13XXXX",
#'stringsAsFactors = FALSE
#')
#'
#'
#' check_ae_aeout_aeendtc(AE)
#' check_ae_aeout_aeendtc(AE,preproc=roche_derive_rave_row)
#'
#' AE$AEENDTC <- NULL
#' check_ae_aeout_aeendtc(AE)
#'
#' AE$AEOUT <- NULL
#' check_ae_aeout_aeendtc(AE)
#'

check_ae_aeout_aeendtc <- function(AE,preproc=identity,...){
  
  if (AE %lacks_any% c("USUBJID", "AEENDTC", "AEOUT")) {
    
    fail(lacks_msg(AE, c("USUBJID", "AEENDTC", "AEOUT" )))
    
  } else {
    
    #Apply company specific preprocessing function
    AE = preproc(AE,...)
    
    df <- AE %>%
      filter((is_sas_na(AEENDTC)  & AEOUT %in% c("FATAL", "RECOVERED/RESOLVED", "RECOVERED/RESOLVED WITH SEQUELAE")) |  
             (!is_sas_na(AEENDTC) & AEOUT %in% c("UNKNOWN", "NOT RECOVERED/NOT RESOLVED", "RECOVERING/RESOLVING"))) %>%
      select(any_of(c("USUBJID","AEENDTC","AEOUT","RAVE")))
    
    if( nrow(df) > 0 ){
      fail("AEs with inconsistent AEENDTC and AEOUT found.", df)
    } else {
      pass()
    }
  }
}
