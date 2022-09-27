#' @title Check for partial death dates in AE and DS
#'
#' @description This checks looks for partial death dates in AE and DS
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID,AEDTHDTC,AEDECOD
#' @param DS Dispostion SDTM dataset with variables USUBJID,DSSCAT,DSSTDTC,DSDECOD
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @importFrom dplyr %>% select
#' @importFrom tidyselect any_of
#'
#' @author Will Harris
#'
#' @examples
#'
#' # test with sample data
#'
#' AE <- data.frame(
#'  USUBJID = 1:3,
#'  AEDECOD = c("AE1","AE2","AE3"),
#'  AEDTHDTC = c("2017-01-01","2017",NA),
#'  AESPID = "FORMNAME-R:2/L:2XXXX",
#'  stringsAsFactors=FALSE
#' )
#'
#' DS <- data.frame(
#'  USUBJID = 1:4,
#'  DSSCAT = "STUDY DISCON",
#'  DSDECOD = "DEATH",
#'  DSSTDTC = c("2017-01-01","2017","2017-01-02","2016-10"),
#'  stringsAsFactors=FALSE
#' )
#'
#' check_ae_ds_partial_death_dates(AE,DS)
#' check_ae_ds_partial_death_dates(AE,DS,preproc=roche_derive_rave_row)
#'
#' DS$DSSTDTC = NULL
#'
#' check_ae_ds_partial_death_dates(AE,DS)
#'


check_ae_ds_partial_death_dates <- function(AE,DS,preproc=identity,...){

  ###First check that required variables exist and return a message if they don't
if(DS %lacks_any% c("USUBJID","DSSCAT","DSSTDTC","DSDECOD")){

    fail(lacks_msg(DS, c("USUBJID","DSSCAT","DSSTDTC","DSDECOD")))

}else if(AE %lacks_any% c("USUBJID","AEDTHDTC","AEDECOD")){

    fail(lacks_msg(AE, c("USUBJID","AEDTHDTC","AEDECOD")))

    }else{

      #Apply company specific preprocessing function
      AE = preproc(AE,...)

  ### Find records with partial death dates (length <10) in AE and DS

      mydf1 = subset(DS,DS$DSDECOD=="DEATH" & !is_sas_na(DS$DSSTDTC) & nchar(DS$DSSTDTC)<10,c("USUBJID","DSSCAT","DSDECOD","DSSTDTC"))
      mydf2 = subset(AE,!is_sas_na(AE$AEDTHDTC) & nchar(AE$AEDTHDTC)<10,) %>%
        select(any_of(c("USUBJID","AEDECOD","AEDTHDTC","RAVE")))
      mydf = merge(mydf1,mydf2,by="USUBJID",all=TRUE)

  ###Print to report

  ### Return message if no records
  if(nrow(mydf)==0 ){
      pass()

  ### Return subset dataframe if there are records with partial dates
    }else if(nrow(mydf)>0){
        fail(paste("There are ",length(unique(mydf$USUBJID)),
                   " patients with partial death dates. ",sep=""),
             mydf)
    }
  }
 }
