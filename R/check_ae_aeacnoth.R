#' @title Check for null AEACNOT1/2 when AEACNOTH = 'MULTIPLE'
#'
#' @description Flag if patient has a record with null values of AEACNOT1 and AEACNOT2
#'              but AEACNOTH = 'MULTIPLE', so a likely mapping issue
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AETERM, AESTDTC,
#' AEACNOTH, AEACNOT1/2, AESPID (optional)
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#'
#' @author Ross Farrugia
#'
#' @examples
#'
#' AE <- data.frame(
#'  USUBJID = 1:7,
#'  AETERM = 1:7,
#'  AESTDTC = 1:7,
#'  AEACNOTH = 1:7,
#'  AEACNOT1 = 1:7,
#'  AEACNOT2 = 1:7,
#'  AESPID = "FORMNAME-R:13/L:13XXXX"
#' )
#'
#' # pass
#' check_ae_aeacnoth(AE)
#'
#' AE$AEACNOTH[1] = ""
#' AE$AEACNOT1[1] = ""
#' AE$AEACNOT2[1] = ""
#' AE$AEACNOTH[2] = "MULTIPLE"
#' AE$AEACNOT1[2] = "DOSE REDUCED"
#' AE$AEACNOT2[2] = "DRUG WITHDRAWN"
#' AE$AEACNOTH[3] = "MULTIPLE"
#' AE$AEACNOT1[3] = "DOSE REDUCED"
#' AE$AEACNOT2[3] = ""
#' AE$AEACNOTH[4] = "MULTIPLE"
#' AE$AEACNOT1[4] = ""
#' AE$AEACNOT2[4] = "DRUG WITHDRAWN"
#' AE$AEACNOTH[5] = "MULTIPLE"
#' AE$AEACNOT1[5] = ""
#' AE$AEACNOT2[5] = ""
#' 
#' # fail
#' check_ae_aeacnoth(AE)
#' check_ae_aeacnoth(AE,preproc=roche_derive_rave_row)
#' 
#' AE$AEACNOTH[1] = NA
#' AE$AEACNOT1[1] = NA
#' AE$AEACNOT2[1] = NA
#' AE$AEACNOT2[3] = NA 
#' AE$AEACNOT1[4] = NA 
#' AE$AEACNOT1[5] = NA
#' AE$AEACNOT2[5] = NA
#' 
#' # fail
#' check_ae_aeacnoth(AE)
#' check_ae_aeacnoth(AE,preproc=roche_derive_rave_row)
#' 
#'
#' AE$AEACNOTH <- NULL
#' AE$AEACNOT1 <- NULL
#' AE$AEACNOT2 <- NULL
#' AE$AESPID <- NULL
#' check_ae_aeacnoth(AE)
#'

check_ae_aeacnoth <- function(AE,preproc=identity,...){

  ###First check that required variables exist and return a message if they don't
  if( AE %lacks_any% c("USUBJID","AETERM","AESTDTC","AEACNOTH","AEACNOT1","AEACNOT2")){

    fail(lacks_msg(AE, c("USUBJID","AETERM","AESTDTC","AEACNOTH","AEACNOT1","AEACNOT2")))

  } else{

    #Apply company specific preprocessing function
    AE = preproc(AE,...)

    ### Subset AE to only records with null AEACNOT[1/2] when AEACNOTH = 'MULTIPLE'
    mydf <- AE %>%
      select(any_of(c("USUBJID", "AETERM","AESTDTC","AEACNOTH","AEACNOT1","AEACNOT2","RAVE"))) %>%
      filter(AE$AEACNOTH == "MULTIPLE" & (is_sas_na(AEACNOT1) | is_sas_na(AEACNOT2)))

    rownames(mydf)=NULL

    ###Print to report

    ### Return message if no records with null AEACNOT[1/2] when AEACNOTH = 'MULTIPLE'
    if(nrow(mydf)==0){
      pass()
      ### Return subset dataframe if there are records with null AEACNOT[1/2] when AEACNOTH = 'MULTIPLE'
    }else if(nrow(mydf)>0){
      fail(paste("AE has ",nrow(mydf)," record(s) with null AEACNOT[1/2] when AEACNOTH = 'MULTIPLE'. ",sep=""), mydf)
    }
  }
}

