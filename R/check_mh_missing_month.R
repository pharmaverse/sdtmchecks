#' @title Check for MH dates with year and day known but month unknown
#'
#' @description This check looks for partial missing dates in medical history
#'   start and end dates. That is, with only the month missing while the year
#'   and day are known
#'
#' @param MH Medical History SDTM dataset with variables USUBJID, MHTERM and MHSTDTC
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return Boolean value for whether the check passed or failed, with 'msg'
#'   attribute if the check failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#'
#' @author Chandra Mannem
#'
#' @examples
#'
#' MH <- data.frame(USUBJID = LETTERS[1:5],
#'                  MHTERM = LETTERS[5:1],
#'                  MHSTDTC = c("2014", NA, "2014-01", "", "2014---02"),
#'                  stringsAsFactors = FALSE)
#'
#' check_mh_missing_month(MH)
#'
#' MH$MHSPID= "FORMNAME-R:2/L:2XXXX"
#' 
#' check_mh_missing_month(MH,preproc=roche_derive_rave_row)
#'

check_mh_missing_month <- function(MH, preproc=identity,...) {
    
    if (MH %lacks_any% c("USUBJID", "MHTERM", "MHSTDTC")) {
        
        fail(lacks_msg(MH, c("USUBJID", "MHTERM", "MHSTDTC")))
        
    } else {
        
        #Apply company specific preprocessing function
        MH = preproc(MH,...)
        
        if (MH %lacks_any% "MHENDTC") {
            
            df <- MH %>%
                select(any_of(c("USUBJID", "MHTERM", "MHSTDTC","RAVE"))) %>%
                filter(missing_month(MHSTDTC))
            
        } else {
            
            df <- MH %>%
                select(any_of(c("USUBJID", "MHTERM", "MHSTDTC", "MHENDTC","RAVE"))) %>%
                filter(missing_month(MHSTDTC) | missing_month(MHENDTC))
            
        }
        
        if (nrow(df) == 0) {
            
            pass()
            
        } else {
            
            fail("MH has date(s) with known year and day but missing month. ", df)
            
        }
    }
}

