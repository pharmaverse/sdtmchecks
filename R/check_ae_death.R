#' @title Check for Grade 5 AE death variable consistency
#'
#' @description Checks for grade 5 AEs not marked fatal (AEOUT), death not indicated (AESDTH), or no death date (AESDTHDTC)
#'
#' @param AE Adverse Event dataframe with variables USUBJID,AETOXGR,AEOUT,AEDTHDTC,AESDTH
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @author Iris Zhao
#'
#' @importFrom dplyr %>% select
#' @importFrom tidyselect any_of
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @examples
#'
#'  AE <- data.frame(
#'  USUBJID = 1:10,
#'  AETOXGR = c(1:5,5,5,5,5,5),
#'  AEDTHDTC = c(rep(NA,4),rep("2020-01-01",6)),
#'  AESDTH = c(rep(NA,4),rep("Y",6)),
#'  AEOUT = c(rep(NA,4),rep("FATAL",6)),
#'   AESPID = "FORMNAME-R:13/L:13XXXX"
#' )
#'
#' check_ae_death(AE)
#' check_ae_death(AE,preproc=roche_derive_rave_row)
#'
#' AE$AEDTHDTC[5]="NA"
#' AE$AEDTHDTC[6]=NA
#' AE$AEDTHDTC[7]=""
#' AE$AESDTH[8]=NA
#' AE$AEOUT[9]=NA
#'
#' check_ae_death(AE)
#' check_ae_death(AE,preproc=roche_derive_rave_row)
#'

check_ae_death <- function(AE,preproc=identity,...){

  ###Check that required variables exist and return a message if they don't.

  if(AE %lacks_any% c("USUBJID","AETOXGR","AEOUT","AEDTHDTC","AESDTH")){

      fail(lacks_msg(AE, c("USUBJID","AETOXGR","AEOUT","AEDTHDTC","AESDTH")))

  }else{

    #Apply company specific preprocessing function
    AE = preproc(AE,...)

    ### Subset AE to records with Grade 5 AE but have missing death date, or not marked fatal, or death not indicated
    ae5 = subset(AE, AE$AETOXGR == '5' & (AE$AEOUT !='FATAL' | is_sas_na(AE$AEOUT) | is_sas_na(AE$AEDTHDTC) | AE$AESDTH !='Y' |is_sas_na(AE$AESDTH)) , ) %>%
      select(any_of(c('USUBJID','AETOXGR','AEOUT','AEDTHDTC','AESDTH','RAVE')))
    rownames(ae5)=NULL

    ###Print to report

    ### Return message if no such Grade 5 records
    if(nrow(ae5)==0){
        pass()

      ### Return subset dataframe if there are records with Grade 5 AE has missing death date, or not marked fatal, or death not indicated
    }else if(nrow(ae5)>0){
        fail(
            paste("Total number of records with grade 5 AEs and inconsistencies among AE death variables is ",nrow(ae5),". ",sep=""),
            ae5
        )
        }
  }
}
