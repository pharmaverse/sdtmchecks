#' @title Check for missing AEDTHDTC where DS indicates death due to AE
#'
#' @description This check looks for missing AEDTHDTC values if a patients has a
#'   DS record where DSDECOD=DEATH and DSTERM contains ADVERSE EVENT
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AEDTHDTC
#' @param DS Disposition SDTM dataset with variables USUBJID, DSDECOD, DSTERM, DSSTDTC
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author Aldrich Salva
#'
#' @examples
#'
#' AE <- data.frame(
#'  USUBJID = 1:3,
#'  AEDTHDTC = c(NA,NA,1)
#' )
#'
#' DS <- data.frame(
#'  USUBJID = 1:4,
#'  DSTERM = c("DEATH DUE TO ADVERSE EVENT","DEATH DUE TO PROGRESSIVE DISEASE",
#'             "DEATH DUE TO ADVERSE EVENT","DEATH DUE TO ADVERSE EVENT")
#'             ,
#'  DSDECOD = rep("DEATH",4),
#'  DSSTDTC = "2020-01-01"
#' )
#'
#' check_ae_aedthdtc_ds_death(AE,DS)
#'
#' DS$DSSTDTC = NULL
#'
#' check_ae_aedthdtc_ds_death(AE,DS)
#'
#'


check_ae_aedthdtc_ds_death <- function(AE,DS){

  ###First check that required variables exist and return a message if they don't
    if (AE %lacks_any% c("USUBJID", "AEDTHDTC")) {

        fail(lacks_msg(AE, c("USUBJID", "AEDTHDTC")))

    } else if (DS %lacks_any% c("USUBJID", "DSDECOD", "DSTERM","DSSTDTC")) {

        fail(lacks_msg(DS, c("USUBJID", "DSDECOD", "DSTERM", "DSSTDTC")))

    } else{

    ######### Only consider DS records from patients who have a record with DEATH and ADVERSE EVENT
    ds0a  <- subset(DS,(regexpr("DEATH", DS$DSDECOD, ignore.case = TRUE) != -1) &
                      (regexpr("ADVERSE EVENT", DS$DSTERM, ignore.case = TRUE) != -1),)

    ######### If there are no DS records that qualify for the check ###########
    if(nrow(ds0a)==0){

        pass()

    ######### If there are DS records that qualify ###########
    }else if(nrow(ds0a)>0){

      # Look for all records in AE where the death date is populated
      ae0a <- subset(AE,!is_sas_na(AE$AEDTHDTC),)

      # Check for subjects in ds0a that do not have a matching record in ae0a
      ds11 <- subset(ds0a,!(ds0a$USUBJID %in% ae0a$USUBJID),c('USUBJID','DSTERM','DSSTDTC'))
      rownames(ds11)=NULL

      # If all all subjects in ds0a have a matching record in ae0a
      if(nrow(ds11)==0){
          pass()

      ### Return subset dataframe if there are records with inconsistency
      }else if(nrow(ds11)>0){

          fail(
              msg = paste("There are",
                          length(unique(ds11$USUBJID)),
                          "patients where DS.DSDECOD contains 'DEATH' AND DS.DSTERM contains 'ADVERSE EVENT'",
                          "but with no death date in AE.AEDTHDTC. "
                          ),
              data = ds11
              )
      }
    }
  }
}

