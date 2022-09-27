#' @title Check for missing ARM or ARMCD values in DM
#'
#' @description This check looks for missing ARM or ARMCD values
#'
#' @param DM Demographics SDTM dataset with variables USUBJID, ARM, ARMCD
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author Rena Wang
#'
#' @examples
#'
#' DM <- data.frame(
#'  USUBJID = 1:3,
#'  ARM = 1:3,
#'  ARMCD = 1:3
#' )
#'
#' check_dm_armcd(DM)
#'
#' DM$ARMCD[1] <- NA
#' check_dm_armcd(DM)
#'
#' DM$ARM[2] <- NA
#' check_dm_armcd(DM)
#'
#' DM$ARMCD <- NULL
#' check_dm_armcd(DM)
#'
#'

check_dm_armcd <- function(DM){

  ###First check that required variables exist and return a message if they don't
if(DM %lacks_any% c("USUBJID","ARM", "ARMCD")){

    fail(lacks_msg(DM, c("USUBJID", "ARM", "ARMCD")))

    }else{

  ### Subset DM to only records with missing ARMCD
      mydf = subset(DM,is_sas_na(DM$ARM)|is_sas_na(DM$ARMCD),c("USUBJID","ARM","ARMCD"))
      rownames(mydf)=NULL

  ###Print to report

  ### Return message if no records with missing ARMCD
  if(nrow(mydf)==0){
        pass()

  ### Return subset dataframe if there are records with missing ARMCD
    }else if(nrow(mydf)>0){
        fail(
            paste("Total number of patients with missing ARM/ARMCD values is ",nrow(mydf),". ",sep=""),
            mydf
        )
    }
  }
 }
