#' @title Check that all AE start dates are on or before AE end dates
#'
#' @description This check identifies AESTDTC values that are after AEENDTC values
#'
#' @param AE Adverse Event SDTM dataset with variables USUBJID,AETERM,AEDECOD,AESTDTC,AEENDTC
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
#' @author Sara Bodach
#'
#' @examples
#'
#' AE <- data.frame(
#'  USUBJID = 1:12,
#'  AETERM = "SOME AE TERM",
#'  AEDECOD = "SOME AE PT",
#'  AESTDTC = c("2017-01-01","2017-01-03","2017-01-01T14:26","2017","2017-02","2017"      ,""    ,
#'              "2017"   ,"2017-01-01T14:26","2017-01-01T14:26","2017-01-01T14","2017-01-01T14:26:02")
#'              ,
#'  AEENDTC = c("2017-01-01","2017-01-02","2017-01-01T14:25","2015","2017-01","2016-01-01",
#'              "2000","2017-02","2017-01-01"      ,"2017-01","2017-01-01T13","2017-01-01T14:26:01")
#'              ,
#'  AESPID = "FORMNAME-R:19/L:19XXXX",
#'  stringsAsFactors=FALSE
#' )
#'
#' check_ae_aestdtc_after_aeendtc(AE)
#' check_ae_aestdtc_after_aeendtc(AE,preproc=roche_derive_rave_row)
#'
#' AE$AETERM <- NULL
#' check_ae_aestdtc_after_aeendtc(AE)
#'


check_ae_aestdtc_after_aeendtc <- function(AE,preproc=identity,...){

  ###First check that required variables exist and return a message if they don't
  if(AE %lacks_any% c("USUBJID", "AETERM", "AEDECOD", "AESTDTC", "AEENDTC")){

      fail(lacks_msg(AE, c("USUBJID", "AETERM", "AEDECOD", "AESTDTC", "AEENDTC")))

  } else{

    #Apply company specific preprocessing function
    AE = preproc(AE,...)

    # among records with an end date
      ## Get minumimum length for when AEENDTC and AEENDTC are different lengths
      AE$startdate=substr(AE$AESTDTC,1, pmin(nchar(AE$AESTDTC),nchar(AE$AEENDTC),na.rm = TRUE))
      AE$enddate=substr(AE$AEENDTC,1, pmin(nchar(AE$AESTDTC),nchar(AE$AEENDTC),na.rm = TRUE))

      #We're not accounting for any time resolution smaller than minutes
      AE$startdate[nchar(AE$startdate)>16] = substr(AE$startdate[nchar(AE$startdate)>16],1,16)
      AE$enddate[nchar(AE$enddate)>16] = substr(AE$enddate[nchar(AE$enddate)>16],1,16)

      # convert string to date/time
      AE$DT1 = NA
      AE$DT1[nchar(AE$startdate)==16] <- as.POSIXct(AE$startdate[nchar(AE$startdate)==16],format="%Y-%m-%dT%H:%M")
      AE$DT1[nchar(AE$startdate)==13] <- as.POSIXct(AE$startdate[nchar(AE$startdate)==13],format="%Y-%m-%dT%H")
      AE$DT1[nchar(AE$startdate)==10] <- as.POSIXct(AE$startdate[nchar(AE$startdate)==10],format="%Y-%m-%d")
      AE$DT1[nchar(AE$startdate)==7] <- as.POSIXct(AE$startdate[nchar(AE$startdate)==7],format="%Y-%M")
      AE$DT1[nchar(AE$startdate)==4] <- as.POSIXct(AE$startdate[nchar(AE$startdate)==4],format="%Y")

      AE$DT2 = NA
      AE$DT2[nchar(AE$enddate)==16] <- as.POSIXct(AE$enddate[nchar(AE$enddate)==16],format="%Y-%m-%dT%H:%M")
      AE$DT2[nchar(AE$enddate)==13] <- as.POSIXct(AE$enddate[nchar(AE$enddate)==13],format="%Y-%m-%dT%H")
      AE$DT2[nchar(AE$enddate)==10] <- as.POSIXct(AE$enddate[nchar(AE$enddate)==10],format="%Y-%m-%d")
      AE$DT2[nchar(AE$enddate)==7] <- as.POSIXct(AE$enddate[nchar(AE$enddate)==7],format="%Y-%M")
      AE$DT2[nchar(AE$enddate)==4] <- as.POSIXct(AE$enddate[nchar(AE$enddate)==4],format="%Y")

    # records with different dates
    mydf <- AE %>%
      filter(!is_sas_na(AESTDTC) & !is_sas_na(AEENDTC) & (DT1 > DT2 )) %>%
      select(any_of(c("USUBJID", "AETERM", "AEDECOD", "AESTDTC", "AEENDTC","RAVE")))

    rownames(mydf)=NULL

    ### Return message if no records with issue
    if(nrow(mydf)==0){
     pass()

    ### Return subset dataframe if there are issues with start date after end date
    }else if(nrow(mydf)>0){
        fail(paste("AE has ",nrow(mydf)," records with AESTDTC after AEENDTC. ",sep=""), mydf)
    }
  }
}
