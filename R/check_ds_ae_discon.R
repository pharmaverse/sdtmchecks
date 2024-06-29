#' @title Check for treatment discontinuation consistency between DS and AE
#'
#' @description This check looks for consistency when DS.DSSPID=DISCTX*
#'              then there should be AE.AEACN*=DRUG WITHDRAWN
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AEDECOD,
#' AESTDTC, AEACN*
#'
#' @param DS Disposition SDTM dataset with variables USUBJID, DSSPID, DSCAT,
#' DSDECOD, DSSTDTC
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#' test failed
#'
#' @importFrom dplyr distinct
#'
#' @export
#'
#' @author Sarwan Singh
#'
#' @examples
#'
#' AE <- data.frame(
#'  USUBJID = 1:5,
#'  AESTDTC = "01JAN2017",
#'  AETERM  = c("AE1","AE2","AE3","AE4","AE5"),
#'  AEDECOD = c("AE1","AE2","AE3","AE4","AE5"),
#'  AEACN = c("DOSE REDUCED", "DOSE REDUCED", "DOSE NOT CHANGED",
#'  "DOSE NOT CHANGED", "NOT APPLICABLE"),
#'  stringsAsFactors = FALSE
#' )
#'
#'
#' DS <- data.frame(
#'  USUBJID = 1:5,
#'  DSSPID  = c('XXXDISCTXXXXX'),
#'  DSSTDTC = '01JAN2017',
#'  DSCAT   = rep("DISPOSITION EVENT", 5),
#'  DSSCAT   = rep("TX FORM", 5),
#'  DSDECOD = c("PHYSICIAN DECISION", "OTHER", "PHYSICIAN DECISION", "OTHER", "DEATH"),
#'  stringsAsFactors = FALSE
#' )
#'
#' # no case
#' check_ds_ae_discon(DS, AE)
#'
#' # 1 case
#' DS[3, "DSDECOD"] <- 'ADVERSE EVENT'
#' check_ds_ae_discon(DS, AE)
#'
#' # mutliple AEACNx
#' AE <- data.frame(
#'  USUBJID = 1:5,
#'  AESTDTC = c("01JAN2017"),
#'  AETERM  = c("AE1","AE2","AE3","AE4","AE5"),
#'  AEDECOD = c("AE1","AE2","AE3","AE4","AE5"),
#'  AEACN   = rep("MULTIPLE", 5),
#'  AEACN1  = c("DOSE REDUCED", "DOSE NOT CHANGED", "DOSE NOT CHANGED",
#'  "DOSE NOT CHANGED", "NOT APPLICABLE"),
#'  stringsAsFactors = FALSE
#' )
#'
#' check_ds_ae_discon(DS, AE)
#'


## Check for missing death dates when ae outcomes are fatal.

check_ds_ae_discon <- function(DS, AE){

  ###First check that required variables exist and return a message if they don't
  if(AE %lacks_any% c("USUBJID", "AETERM", "AEDECOD", "AESTDTC","AEACN")){

    fail(lacks_msg(AE, c("USUBJID", "AETERM", "AEDECOD", "AESTDTC","AEACN")))

  } else if(DS %lacks_any% c("USUBJID", "DSSPID", "DSCAT","DSSCAT", "DSDECOD", "DSSTDTC")){

    fail(lacks_msg(DS, c("USUBJID", "DSSPID", "DSCAT","DSSCAT", "DSDECOD", "DSSTDTC")))

  } else{

    # keep variables on which we want to check action taken with treatment
    ae0 <- subset(AE,
                  select = c("USUBJID", "AETERM", "AEDECOD", "AESTDTC", "AEACN",
                             grep("AEACN[0-9]", names(AE), value=TRUE)))


    # select if AE dataset has AEACNx variables
    aeacnvars <- grep("AEACN[0-9]", names(ae0), value=TRUE)

    #create a where condition for AE
    whrcond <- paste("AEACN=='DRUG WITHDRAWN'")
    for (i in aeacnvars) {
      whrcond <- paste(whrcond, " | ", i, "=='DRUG WITHDRAWN'", sep="")
    }

    # loop through each AEACNx and keep DRUG WITHDRAWN AEs
    ae1 <- subset(ae0,
                  eval(parse(text = whrcond)) )

    # keep unique usubjid
    ae2 <- distinct(ae1, USUBJID, .keep_all = TRUE)


    # keep records with discontinuation treatment due to AE
    ds0 <- subset(DS,
                  DSCAT == 'DISPOSITION EVENT' & grepl("DISCTX", DSSPID) &
                    DSDECOD == 'ADVERSE EVENT',
                  select = c("USUBJID", "DSSPID", "DSSCAT", "DSDECOD", "DSSTDTC" ))


    finout <- merge(ae2, ds0,
                    by= "USUBJID",
                    all = TRUE)


    # check if DS record with AE has corresponding record in AE
    mydf <- subset(finout,
                   is_sas_na(AETERM),
                   select = c("USUBJID", "DSSCAT", "DSDECOD", "DSSTDTC")
    )

    rownames(mydf)=NULL

    ### Return message if no inconsistency between AEOUT and AEDTHDTC
    if(nrow(mydf)==0){
      pass()

      ### Return subset dataframe if there are records with inconsistency
    }else if(nrow(mydf)>0){

      return(fail(paste(length(unique(mydf$USUBJID)),
                        " patient(s) with Treatment Discon due to AE but no AE record indicating drug withdrawn. ",sep=""), mydf))

    } #end else if mydf has records
  }  #end else if required variable exist
}  #end check_ds_ae_discon()
