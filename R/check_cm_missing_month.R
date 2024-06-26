#' @title Check for conmed dates with year and day known but month unknown
#'
#' @description Check for missing month when conmed start (CMSTDTC) or end dates
#' (CMENDTC) have known year and day
#'
#' @param CM Concomitant Medications SDTM dataset with variables USUBJID, CMTRT,
#' CMSTDTC, CMENDTC
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
#' @author Chandra Mannem
#'
#' @examples
#'
#' CM <- data.frame(
#'  USUBJID = 1:3,
#'  CMTRT = c("CM1","CM2","CM3"),
#'  CMSTDTC = c("2017-01-01","2017---01","2017-01-02"),
#'  CMENDTC = c("2017-02-01","2017-03-01","2017---01"),
#'  CMSPID = "/F:XXX-D:12345-R:123",
#'  stringsAsFactors=FALSE
#' )
#'
#' check_cm_missing_month(CM)
#' check_cm_missing_month(CM,preproc=roche_derive_rave_row)
#'
#' CM$CMSTDTC = NULL
#'
#' check_cm_missing_month(CM)
#'


check_cm_missing_month <- function(CM,preproc=identity,...){

###First check that required variables exist and return a message if they don't
if(CM %lacks_any% c("USUBJID", "CMTRT", "CMSTDTC","CMENDTC")){

    fail(lacks_msg(CM, c("USUBJID", "CMTRT", "CMSTDTC","CMENDTC")))

    } else{

      #Apply company specific preprocessing function
      CM = preproc(CM,...)

        # check if CMSTDTC CMENDTC has missing month and is in format 'yyyy---dd'
        mydf <- subset(CM,
                       (missing_month(CMSTDTC) | missing_month(CMENDTC) ),
                       ) %>%
          select(any_of(c("USUBJID", "CMTRT", "CMSTDTC","CMENDTC","RAVE")))
        rownames(mydf)=NULL

      ###Print to report

      ### Return message if there are conmed start and end dates with only missing month
      if(nrow(mydf)==0){
        pass()

        ### Return subset dataframe if there are records with inconsistency
      }else if(nrow(mydf)>0){

          fail(paste("There are ",length(unique(mydf$USUBJID)),
                     " patient(s) with a conmed date that has year and day present but missing month. ",sep=""),
               mydf)

        }
      }
}





