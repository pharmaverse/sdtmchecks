#' @title Check for clinical events dates with year and day known but month unknown
#'
#' @description Check for missing month when clinical events dates 
#' (CESTDTC, CEENDTC, CEDTC) have a known year and day
#'
#' @param CE Clinical Events SDTM dataset with variables USUBJID, CETERM, and at 
#' least one of the following date variables: CESTDTC, CEENDTC, CEDTC
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% select bind_rows
#' @importFrom tidyselect any_of
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author Ryan Marinelli
#'
#' @examples
#'
#' CE <- data.frame(
#'  USUBJID = c(1, 2, 3, 4),
#'  CETERM = c("Headache", "Nausea", "Dizziness", "Fever"),
#'  CESTDTC = c("2023---01", "2023-01-15", "2023-02-01", "2023-02-10"),
#'  CEENDTC = c("2023-01-02", "2023---01", "2023-02-02", "2023-02-12"),
#'  CEDTC = c("2023--01", "", "", ""),
#'  CESEV = c("Mild", "Moderate", "Mild", "Severe"),
#'  CESPID = "FORMNAME-R:13/L:13XXXX",
#'  stringsAsFactors=FALSE
#'  )
#'
#' check_ce_missing_month(CE)
#' check_ce_missing_month(CE,preproc=roche_derive_rave_row)
#' 
#' CE <- data.frame(
#'  USUBJID = c(1, 2, 3, 4),
#'  CETERM = c("Headache", "Nausea", "Dizziness", "Fever"),
#'  CESTDTC = c("2023-01-01", "2023-01-15", "2023-02-01", "2023-02-10"),
#'  CEENDTC = c("2023-01-02", "2023-01-16", "2023-02-02", "2023-02-12"),
#'  CEENDTC = "",
#'  CESEV = c("Mild", "Moderate", "Mild", "Severe"),
#'  CESPID = "FORMNAME-R:13/L:13XXXX",
#'  stringsAsFactors=FALSE
#'  )
#'
#' check_ce_missing_month(CE)
#'
#' CE$CETERM = NULL
#'
#' check_ce_missing_month(CE)
#'

check_ce_missing_month <- function(CE, preproc = identity, ...) {
  
  ###First check that required variables exist and return a message if they don't
  if (CE %lacks_any% c("USUBJID", "CETERM")) {
    
    fail(lacks_msg(CE, c("USUBJID", "CETERM")))
    
  } else if(CE %lacks_all% c("CESTDTC", "CEENDTC", "CEDTC")){
    
    fail(lacks_msg(CE, c("CESTDTC", "CEENDTC", "CEDTC")))
    
  }else{
    
    #Apply company specific preprocessing function
    CE = preproc(CE, ...)
    
    outlist=list()
    # check if CESTDTC has missing month and is in format 'yyyy---dd'
    if (CE %has_all% c("CESTDTC")){
      outlist[["cestdtc"]] <- subset(CE,(missing_month(CESTDTC)), ) %>%
        select(any_of(c("USUBJID", "CETERM", "CESTDTC","CEENDTC","CEDTC","RAVE")))
    }
    
    # check if CEENDTC has missing month and is in format 'yyyy---dd'
    if (CE %has_all% c("CEENDTC")){
      outlist[["ceendtc"]]<- subset(CE,(missing_month(CEENDTC)), ) %>%
        select(any_of(c("USUBJID", "CETERM", "CESTDTC","CEENDTC","CEDTC","RAVE")))
    }
    
    # check if CEDTC has missing month and is in format 'yyyy---dd'
    if (CE %has_all% c("CEDTC")){
      outlist[["cedtc"]]<- subset(CE,(missing_month(CEDTC)), ) %>%
        select(any_of(c("USUBJID", "CETERM", "CESTDTC","CEENDTC","CEDTC","RAVE")))
    }
    
    ### stack
    mydf=do.call(bind_rows,outlist) %>% 
      unique()
    
    rownames(mydf) = NULL
    
    ###Print to report
    
    ### Return message if there are clinical events start and end dates with only missing month
    if (nrow(mydf) == 0) {
      pass()
      
      ### Return subset dataframe if there are records with inconsistency
    } else if (nrow(mydf) > 0) {
      fail(
        paste(
          length(unique(mydf$USUBJID)),
          "patient(s) with a clinical events date that has year and day present but missing month. "
        ),
        mydf
      )
      
    }
  }
}
