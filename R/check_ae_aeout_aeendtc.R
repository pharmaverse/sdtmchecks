#' @title Check for inconsistency between AE outcome (AEOUT) and AE end date (AEENDTC) for non-fatal AEs
#'
#' @description Check for non-fatal AEs with inconsistent AEOUT and AEENDTC
#' 
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AETERM, AESTDTC, AEENDTC, AEOUT
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
#'AEENDTC = c(NA, "NA", "2015-03-12", "2010-10", "2017-01-22",   "1999-11-07",        "",     NA,                   "2017-09-01",          "2015-01-01"),
#'AEOUT   = c("", "",   "",           "",        "NOT RECOVERED","RECOVERED/RESOLVED","FATAL","RECOVERED/RESOLVED", "RECOVERING/RESOLVING","UNKNOWN"),
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
  
  if (AE %lacks_any% c("USUBJID", "AESTDTC", "AETERM", "AEENDTC", "AEOUT")) {
    
    fail(lacks_msg(AE, c("USUBJID", "AESTDTC", "AETERM", "AEENDTC", "AEOUT" )))
    
  } else {
    
    #Apply company specific preprocessing function
    AE = preproc(AE,...)
    
    df <- AE %>%
      filter((is_sas_na(AEENDTC)  & AEOUT %in% c("RECOVERED/RESOLVED", "RECOVERED/RESOLVED WITH SEQUELAE")) |  
             (!is_sas_na(AEENDTC) & AEOUT %in% c("UNKNOWN", "NOT RECOVERED/NOT RESOLVED", "RECOVERING/RESOLVING"))) %>%
      select(any_of(c("USUBJID","AETERM", "AESTDTC", "AEENDTC","AEOUT","RAVE")))
    
    if( nrow(df) > 0 ){
      fail("AE(s) with inconsistent AEENDTC and AEOUT found. ", df)
    } else {
      pass()
    }
  }
}
