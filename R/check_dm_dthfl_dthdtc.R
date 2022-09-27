#' @title Check that when DM.DTHFL is Y, DM.DTHDTC does not have a missing value, and vice versa
#'
#' @description This check is bi-directional for consistency of DM.DTHFL and DM.DTHDTC and returns a data
#'   frame. Note there is a possible valid scenario for this issue if death date is truly unknown
#'
#' @param DM Demographics SDTM dataset with variables USUBJID,DTHFL,DTHDTC
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#'
#' @author Ross Farrugia
#'
#' @examples
#'
#' DM <- data.frame(
#'  USUBJID = 1:7,
#'  DTHFL = 1:7,
#'  DTHDTC = 1:7
#' )
#'
#' DM$DTHFL[1] = ""
#' DM$DTHDTC[1] = "2020-01-01"
#' DM$DTHFL[2] = "N"
#' DM$DTHDTC[2] = "2020-01-01"
#' DM$DTHFL[3] = "Y"
#' DM$DTHDTC[3] = "2020-01-01"
#' DM$DTHFL[4] = "Y"
#' DM$DTHDTC[4] = ""
#' DM$DTHFL[5] = "N"
#' DM$DTHDTC[5] = ""
#' DM$DTHFL[6] = "Y"
#' DM$DTHDTC[6] = "2020"
#' DM$DTHFL[7] = ""
#' DM$DTHDTC[7] = ""
#' check_dm_dthfl_dthdtc(DM)
#'
#' DM$DTHFL <- NULL
#' DM$DTHDTC <- NULL
#' check_dm_dthfl_dthdtc(DM)
#'

check_dm_dthfl_dthdtc <- function(DM){

  ###First check that required variables exist and return a message if they don't
  if(DM %lacks_any% c("USUBJID","DTHFL","DTHDTC")){

    fail(lacks_msg(DM, c("USUBJID","DTHFL","DTHDTC")))

  }else{

    ### Subset DM to only records with inconsistency between DTHFL and DTHDTC
    mydf <- DM %>%
      select("USUBJID","DTHFL","DTHDTC") %>%
      filter((DM$DTHFL == "Y" & is_sas_na(DM$DTHDTC)) | (DM$DTHFL != "Y" & !is_sas_na(DM$DTHDTC)))

    ###Print to report

    ### Return message if no records with potential data issue
    if(nrow(mydf)==0){
      pass()
      ### Return subset dataframe if there are records with potential data issue
    }else if(nrow(mydf)>0){
      fail(paste("DM has ",nrow(mydf)," records with inconsistent values of DTHFL and DTHDTC. ",sep=""), mydf)
    }
  }
}

