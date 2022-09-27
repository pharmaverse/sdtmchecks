#' @title Check missing cause of death information in DS
#'
#' @description This check looks for DS.DSTERM values with missing death reason
#'   and returns a data frame (e.g. records where DSTERM = 'DEATH DUE TO')
#'
#' @param DS Disposition SDTMv dataset with variables USUBJID, DSTERM, DSDECOD,
#'   DSDTC, DSSTDTC
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
#' DS <- data.frame(
#'  STUDYID = 1,
#'  USUBJID = 1:4,
#'  DSTERM = c("DEATH DUE TO",
#'    "DEATH DUE TO ",
#'    "DEATH DUE TO ADVERSE EVENT",
#'    "DEATH DUE TO UNKNOWN"),
#'  DSDECOD = "DEATH",
#'  DSDTC = "2017-01-01",
#'  DSSTDTC = "2017-01-01",
#'  stringsAsFactors=FALSE
#' )
#'
#' check_ds_dsterm_death_due_to(DS)
#'
#' DS$DSDECOD <- NULL
#' check_ds_dsterm_death_due_to(DS)
#'

check_ds_dsterm_death_due_to <- function(DS){

  ###First check that required variables exist and return a message if they don't
  if(DS %lacks_any% c("USUBJID", "DSTERM", "DSDECOD", "DSDTC", "DSSTDTC" )){

      fail(lacks_msg(DS, c("USUBJID", "DSTERM", "DSDECOD", "DSDTC", "DSSTDTC")))

  } else{

    # DS records with cause of death
    mydf <- subset(DS, (DS$DSTERM=="DEATH DUE TO" | DS$DSTERM=="DEATH DUE TO "),
                    select=c( "USUBJID", "DSTERM", "DSDECOD", "DSDTC", "DSSTDTC" ))

    ### Return message if no records with issue
    if(nrow(mydf)==0){
     pass()

    ### Return subset dataframe if issues with cause of death in DS domain
    }else if(nrow(mydf)>0){
        fail(paste("DS has ",
                   nrow(mydf)," records with missing death reason. ",sep=""), mydf)
    }
  }
}

