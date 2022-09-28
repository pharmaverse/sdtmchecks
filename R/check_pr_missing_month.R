#' @title Check for procedure dates with year and day known but month unknown
#'
#' @description This check looks for partial missing dates in PR Procedures
#'   start date and end date, if end date exists. If the day of the month is
#'   known, the month should be known.
#'
#' @param PR Procedures SDTM dataset with variables USUBJID, PRTRT, PRSTDTC,
#' PRENDTC (optional), PRSPID (optional)
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @examples
#'
#' PR <- data.frame(
#'  USUBJID = 1:3,
#'  PRTRT = c("Surgery Name","Procedure Name","Procedure"),
#'  PRSTDTC = c("2017-01-01","2017---01","2017-01-02"),
#'  PRENDTC = c("2017-02-01","2017-03-01","2017---01"),
#'  PRSPID = "/F:SURG-D:12345-R:1",
#'  PRCAT = "Form 1",
#'  stringsAsFactors=FALSE
#' )
#'
#' check_pr_missing_month(PR)
#'
#' check_pr_missing_month(PR,preproc=roche_derive_rave_row)
#'
#' PR$PRENDTC = NULL
#' check_pr_missing_month(PR)
#'


check_pr_missing_month <- function(PR,preproc=identity,...){

###First check that required variables exist and return a message if they don't
if(PR %lacks_any% c("USUBJID", "PRTRT", "PRSTDTC")){

    fail(lacks_msg(PR, c("USUBJID", "PRTRT", "PRSTDTC")))

    } else{

      #Apply company specific preprocessing function
      PR = preproc(PR,...)

      # PRENDTC is an optional variable

      if (PR %lacks_any% "PRENDTC") {

        mydf <- PR %>%
          filter(missing_month(PRSTDTC)) %>%
          select(any_of(c("USUBJID", "PRTRT", "PRSTDTC", "PRENDTC", "RAVE")))

      } else {

        mydf <- PR %>%
          filter(missing_month(PRSTDTC) | missing_month(PRENDTC)) %>%
          select(any_of(c("USUBJID", "PRTRT", "PRSTDTC", "PRENDTC", "RAVE")))

      }

      rownames(mydf)=NULL

      ###Print to report

      if(nrow(mydf)==0){
        pass()

        ### Return subset dataframe if there are records with inconsistency
      }else if(nrow(mydf)>0){

          fail(paste(length(unique(mydf$USUBJID)),
                     " patient(s) with a PR procedure date with known year and day but unknown month. ",sep=""),
               mydf)

        }
      }
}
