#' @title Check for death variable consistency when AEOUT=="FATAL"
#'
#' @description This check looks for consistency in AESDTH, AEDTHDTC, and
#' AETOXGR (if applicable) when AEOUT is 'FATAL'
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AEDECOD,
#' AESTDTC, AEDTHDTC, AEOUT, AESDTH
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @importFrom dplyr %>% select
#' @importFrom tidyselect any_of
#'
#' @export
#'
#' @author Aldrich Salva
#'
#' @examples
#'
#' AE <- data.frame(
#'  STUDYID = "Study1",
#'  USUBJID = 1:5,
#'  AESTDTC = "01JAN2017",
#'  AEDECOD = c("AE1","AE2","AE3","AE4","AE5"),
#'  AEOUT = "FATAL",
#'  AEDTHDTC = c("01FEB2017",NA,"02FEB2017","03FEB2017",NA),
#'  AESDTH = c("Y","Y","N","Y",NA),
#'  AETOXGR = c("5","5","5",NA,NA),
#'  AESPID = rep("FORMNAME-R:13/L:13XXXX",5),
#'  OTHERVAR = NA,
#'  stringsAsFactors = FALSE
#' )
#'
#' check_ae_fatal(AE)
#' check_ae_fatal(AE,preproc=roche_derive_rave_row)
#'
#' AE$AETOXGR <- NULL
#' check_ae_fatal(AE)
#'
#' AE$AEDECOD <- NULL
#' check_ae_fatal(AE)
#'


## Check for missing death dates when AE outcomes are fatal.

check_ae_fatal <- function(AE,preproc=identity,...){

  ###First check that required variables exist and return a message if they don't
  if(AE %lacks_any% c("USUBJID", "AEDECOD", "AESTDTC","AEDTHDTC", "AEOUT", "AESDTH")){

    fail(lacks_msg(AE, c("USUBJID", "AEDECOD", "AESTDTC","AEDTHDTC", "AEOUT", "AESDTH")))

  } else{

    #Apply company specific preprocessing function
    AE = preproc(AE,...)

    if(AE %has_any% "AETOXGR"){
      # leave only variables on which we want to check for fatalities and their corresponding death dates
      ae0 <- AE %>%
        select(any_of(c( "USUBJID", "AEDECOD", "AESTDTC", "AEDTHDTC", "AEOUT", "AETOXGR", "AESDTH","RAVE")))

      # check if AEOUT=='FATAL' that there is a corresponding AEDTHDTC, death date
      mydf <- subset(ae0, AEOUT=='FATAL' & (is_sas_na(AEDTHDTC) |
                                              AETOXGR !=5 |
                                              is_sas_na(AETOXGR) |
                                              AESDTH != "Y" |
                                              is_sas_na(AESDTH)),
      )

    }else{
      ae0 <- AE %>%
        select(any_of(c( "USUBJID", "AEDECOD", "AESTDTC", "AEDTHDTC", "AEOUT", "AESDTH","RAVE")))

      # check if AEOUT=='FATAL' that there is a corresponding AEDTHDTC, death date
      mydf <- subset(ae0, AEOUT=='FATAL' & (is_sas_na(AEDTHDTC) |
                                              AESDTH != "Y" |
                                              is_sas_na(AESDTH)),
      )

    }

    ### Return message if no inconsistency between AEOUT and AEDTHDTC
    rownames(mydf)=NULL

    if(nrow(mydf)==0){
      pass()

      ### Return subset dataframe if there are records with inconsistency
    }else if(nrow(mydf)>0){

      return(fail(paste("AE has ",length(unique(mydf$USUBJID))," patient(s) with AE death variable inconsistencies when outcome marked FATAL. ",sep=""), mydf))

    }
  }
}
