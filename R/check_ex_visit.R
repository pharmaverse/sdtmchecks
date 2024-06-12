#' @title Check for missing EX.VISIT
#'
#' @description This check looks missing EX.VISIT values when EX.EXOCCUR=Y (or EX.EXOCCUR doesn't exist)
#'
#' @param EX Exposure SDTM dataset with variables USUBJID,EXTRT,EXSTDTC,VISIT, and optional variable EXOCCUR
#'
#' @author Jen Chen
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @examples
#'
#'  EX <- data.frame(
#'  USUBJID = 1:3,
#'  EXTRT = 1:3,
#'  EXSTDTC = 1:3,
#'  EXOCCUR = "Y",
#'  VISIT = NA
#' )
#'
#' check_ex_visit(EX)
#'
#' EX$EXOCCUR=NULL
#'
#' check_ex_visit(EX)
#'
#' EX$VISIT=NULL
#'
#' check_ex_visit(EX)#
#'

check_ex_visit <- function(EX){

  ###First check that required variables exist and return a message if they don't
  if(EX %lacks_any% c("USUBJID","EXTRT","EXSTDTC","VISIT")){

    fail(lacks_msg(EX, c("USUBJID","EXTRT","EXSTDTC","VISIT")))

  }else{

    if(EX %has_all% c("EXOCCUR")){

      ### Subset EX to only records with missing VISIT
      mydf = subset(EX,EX$EXOCCUR=='Y' & (is_sas_na(EX$VISIT)),c("USUBJID","EXTRT","EXSTDTC","EXOCCUR","VISIT"))
      rownames(mydf)=NULL
    }else{
      mydf = subset(EX,is_sas_na(EX$VISIT),c("USUBJID","EXTRT","EXSTDTC","VISIT"))
      rownames(mydf)=NULL
    }

    ###Print to report

    ### Return message if no records with missing VISIT
    if(nrow(mydf)==0){
      pass()

      ### Return subset dataframe if there are records with missing VISIT
    }else if(nrow(mydf)>0){

      fail(
        paste0("Total number of records is ",nrow(mydf),". "),
        mydf
      )
    }
  }
}

