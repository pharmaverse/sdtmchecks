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
#' @importFrom dplyr filter select %>% bind_rows
#'
#' @export
#'
#' @author Aldrich Salva
#'
#' @examples
#'
#' # AETOXGR, no AESEV
#'
#' AE <- data.frame(
#'  USUBJID = 1:5,
#'  AESTDTC = "01JAN2017",
#'  AEDECOD = c("AE1","AE2","AE3","AE4","AE5"),
#'  AEOUT = "FATAL",
#'  AEDTHDTC = c("01FEB2017",NA,"02FEB2017","03FEB2017",NA),
#'  AESDTH = c("Y","Y","N","Y",NA),
#'  AETOXGR = c("5","5","5",NA,NA),
#'  AESPID = "FORMNAME-R:12/L:2XXXX",
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
#'
#' # AESEV, no AETOXGR
#'
#'  AE <- data.frame(
#'  USUBJID = 1:5,
#'  AESTDTC = "01JAN2017",
#'  AEDECOD = c("AE1","AE2","AE3","AE4","AE5"),
#'  AEOUT = "FATAL",
#'  AEDTHDTC = c("01FEB2017","02FEB2017","03FEB2017","04FEB2017",NA),
#'  AESDTH = c("Y","Y","N","Y",NA),
#'  AESEV = c("SEVERE","MILD","SEVERE",NA,NA),
#'  AESPID = "FORMNAME-R:12/L:2XXXX",
#'  stringsAsFactors = FALSE
#' )
#'
#' check_ae_fatal(AE)
#' check_ae_fatal(AE,preproc=roche_derive_rave_row)
#'
#' AE$AESEV <- NULL
#' check_ae_fatal(AE)
#'
#' # Both AESEV and AETOXGR have non-missing values
#'
#' AE <- data.frame(
#'  USUBJID = 1:7,
#'  AESTDTC = "01JAN2017",
#'  AEDECOD = c("AE1","AE2","AE3","AE4","AE5","AE6","AE7"),
#'  AEOUT = "FATAL",
#'  AEDTHDTC = c("01FEB2017",NA,"02FEB2017","03FEB2017",NA,"04FEB2017","05FEB2017"),
#'  AESDTH = c("Y","Y","N","Y",NA,"Y","Y"),
#'  AESEV = c("SEVERE","MILD","SEVERE",NA,NA,"MILD","SEVERE"),
#'  AETOXGR = c("5","5","5",NA,NA,"1","5"),
#'  AESPID = "FORMNAME-R:12/L:2XXXX",
#'  stringsAsFactors = FALSE
#' )
#'
#' check_ae_fatal(AE)
#' check_ae_fatal(AE,preproc=roche_derive_rave_row)
#'
#'
#' # Neither AESEV or AETOXGR
#'
#' AE <- data.frame(
#'  USUBJID = 1:5,
#'  AESTDTC = "01JAN2017",
#'  AEDECOD = c("AE1","AE2","AE3","AE4","AE5"),
#'  AEOUT = "FATAL",
#'  AEDTHDTC = c("01FEB2017",NA,"02FEB2017","03FEB2017",NA),
#'  AESDTH = c("Y","Y","N","Y",NA),
#'  AESPID = "FORMNAME-R:12/L:2XXXX",
#'  stringsAsFactors = FALSE
#' )
#'
#' check_ae_fatal(AE)
#'
#' # AETOXGR, existing but unmapped AESEV
#'
#'  AE <- data.frame(
#'  USUBJID = 1:5,
#'  AESTDTC = "01JAN2017",
#'  AEDECOD = c("AE1","AE2","AE3","AE4","AE5"),
#'  AEOUT = "FATAL",
#'  AEDTHDTC = c("01FEB2017",NA,"02FEB2017","03FEB2017",NA),
#'  AESDTH = c("Y","Y","N","Y",NA),
#'  AESEV = rep(NA,5),
#'  AETOXGR = c("5","5","5",NA,NA),
#'  AESPID = "FORMNAME-R:12/L:2XXXX",
#'  stringsAsFactors = FALSE
#' )
#'
#' check_ae_fatal(AE)
#' check_ae_fatal(AE,preproc=roche_derive_rave_row)
#'


## Check for missing death dates when ae outcomes are fatal.
## Note, in a case where both AETOXGR and AESEV exist and some records are supposed to have AETOXGR populated while others have AESEV (ie mutaually exlusive)
## Then this check will likely return false positives as it expects a variable to be populated for all records

check_ae_fatal <- function(AE,preproc=identity,...){

  ###First check that required variables exist and return a message if they don't
  if(AE %lacks_any% c("USUBJID", "AEDECOD", "AESTDTC","AEDTHDTC", "AEOUT", "AESDTH")){

      fail(lacks_msg(AE, c("USUBJID", "AEDECOD", "AESTDTC", "AEDTHDTC", "AEOUT", "AESDTH")))

  } else{

    #Apply company specific preprocessing function
    AE = preproc(AE,...)

    outlist=list() #empty list for results

    # check if AEOUT=='FATAL' that there is a corresponding AEDTHDTC, death date

    if(AE %has_any% "AETOXGR" & !all(is_sas_na(AE$AETOXGR))){ #Non-empty AETOXGR exists

    outlist[[1]] = AE %>%
      filter(AEOUT=='FATAL' & (is_sas_na(AEDTHDTC) | AETOXGR !=5 | is_sas_na(AETOXGR) | AESDTH != "Y" | is_sas_na(AESDTH)))
    }

    if(AE %has_any% "AESEV" & !all(is_sas_na(AE$AESEV))){ #Non-empty AESEV exists

    outlist[[2]] <- AE %>%
          filter(AEOUT=='FATAL' & (is_sas_na(AEDTHDTC) | AESEV != "SEVERE" | AESDTH != "Y" | is_sas_na(AESDTH)))

    }

    #Neither of Non-empty AESEV/AETOXGR exists
    #ie Both AESEV/AETOXGR don't exists or one or more exists but is not mapped

    if(!(AE %has_any% "AESEV" & !all(is_sas_na(AE$AESEV))) & !(AE %has_any% "AETOXGR" & !all(is_sas_na(AE$AETOXGR)))){

    outlist[[3]] <- AE %>%
          filter(AEOUT=='FATAL' & (is_sas_na(AEDTHDTC) | AESDTH != "Y" | is_sas_na(AESDTH)))
    }

    mydf = bind_rows(outlist)
    # leave only variables on which we want to check for fatalities and their corresponding death dates
    mydf = unique(mydf[,intersect(names(AE),c("STUDYID","USUBJID", "AEDECOD", "AESTDTC","AEDTHDTC", "AEOUT","AESEV","AETOXGR","AESDTH","RAVE"))])
    rownames(mydf)=NULL

    ### Return message if no inconsistency between AEOUT and AEDTHDTC
    if(nrow(mydf)==0){
      pass()

      ### Return subset dataframe if there are records with inconsistency
    }else if(nrow(mydf)>0){

      fail(paste("AE has ",length(unique(mydf$USUBJID))," patient(s) with AE death variable inconsistencies when outcome marked FATAL. ",sep=""), mydf)

    }
  }
}
