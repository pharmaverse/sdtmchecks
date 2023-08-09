#' @title Check for clinical events dates with year and day known but month unknown
#'
#' @description Check for missing month when clinical events start (CESTDTC) or end dates
#' (CEENDTC) have known year and day
#'
#' @param CE Clinical Events SDTM dataset with variables USUBJID, CETERM,
#' CESTDTC, CEENDTC
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% select
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
#'  CESTDTC = c("2023-01-01", "2023-01-15", "2023-02-01", "2023-02-10"),
#'  CEENDTC = c("2023-01-02", "2023-01-16", "2023-02-02", "2023-02-12"),
#'  CESEV = c("Mild", "Moderate", "Mild", "Severe"),
#'  stringsAsFactors=FALSE
#'  )
#'
#' check_ce_missing_month(CE)
#' check_ce_missing_month(CE,preproc=roche_derive_rave_row)
#'
#' CE$CESTDTC = NULL
#'
#' check_ce_missing_month(CE)
#'


check_ce_missing_month <- function(CE, preproc = identity, ...) {
  ###First check that required variables exist and return a message if they don't
  if (CE %lacks_any% c("USUBJID", "CETERM", "CEENDTC")) {
    fail(lacks_msg(CE, c("USUBJID", "CESTDTC", "CEENDTC")))
    
  } else if (CE %has_any% c("CEDTC", "CESTDTC")){

    #Apply company specific preprocessing function
    CE = preproc(CE, ...)
    
    # check if CESTDTC CEENDTC has missing month and is in format 'yyyy---dd'
    if (CE %has_any% c("CESTDTC"))
    {
    mydf <- subset(CE,
                   (missing_month(CESTDTC) |
                      missing_month(CEENDTC)), ) %>%
      select(any_of(c(
        "USUBJID", "CETERM", "CESTDTC", "CEENDTC"
      )))
    rownames(mydf) = NULL
    }
    
    # check if CESTDTC CEDTC has missing month and is in format 'yyyy---dd'
    if (CE %has_any% c("CEDTC"))
    {
      mydf <- subset(CE,
                     (missing_month(CEDTC) |
                        missing_month(CEENDTC)), ) %>%
        select(any_of(c(
          "USUBJID", "CETERM", "CEDTC", "CEENDTC"
        )))
      rownames(mydf) = NULL
    }
  
    ###Print to report
    
    ### Return message if there are clinical events start and end dates with only missing month
    if (nrow(mydf) == 0) {
      pass()
      
      ### Return subset dataframe if there are records with inconsistency
    } else if (nrow(mydf) > 0) {
      fail(
        paste(
          "There are ",
          length(unique(mydf$USUBJID)),
          " patients with a clinical events date that has year and day present but missing month. ",
          sep = ""
        ),
        mydf
      )
      
    }
  }else{
    fail(lacks_msg(CE, c("CESTDTC")))
  }
}