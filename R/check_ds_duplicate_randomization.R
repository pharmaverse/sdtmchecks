#' @title Check for duplicate randomization records for a patient
#'
#' @description Checks for duplicate subject IDs (USUBJID) in the DS domain when
#' randomization is indicated
#'
#' @param DS Disposition SDTM dataset with variables USUBJID, DSDECOD
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author Madeleine Ma
#'
#' @examples
#'
#' DS <- data.frame(
#'  USUBJID = c("ID1","ID1","ID2","ID2","ID3","ID3"),
#'  DSDECOD = c("RANDOMIZATION","OTHER THING","RANDOMIZATION",
#'              "OTHER THING","RANDOMIZATION","RANDOMIZATION")
#'              , stringsAsFactors = FALSE
#' )
#'
#' check_ds_duplicate_randomization(DS)
#'
#' DS$DSDECOD <- NULL
#' check_ds_duplicate_randomization(DS)
#'


check_ds_duplicate_randomization <- function(DS){

    if(DS %lacks_any% c("USUBJID", "DSDECOD")){

        fail(lacks_msg(DS, c("USUBJID", "DSDECOD")))

    } else{

        myds = subset(DS,grepl("RANDOMIZ",toupper(DS$DSDECOD)),)

        if(nrow(myds)==0){

            pass()

            }else{

  n_uniqueID <- length(unique(myds$USUBJID))

  #gives you a data frame with a list of ID and the frequency they occurred
  n_occur <-data.frame(table(myds$USUBJID))
  IDlist <- subset(n_occur,n_occur$Freq>1)
  colnames(IDlist) <- c("Duplicate USUBJID", "Number of Records")

  if (n_uniqueID== nrow(myds)){

    pass()

  }
  else if (n_uniqueID != nrow(myds)) {
      fail(paste("DS has ",nrow(IDlist),
                 " patient(s) with duplicate randomization records. ",sep=""),
           IDlist)
            }
        }
    }
}
