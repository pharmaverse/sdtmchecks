#' @title Check for missing TULOC values
#'
#' @description This check looks for target lesions with missing TULOC values and
#' returns a data frame. Only applies to assessments by investigator.
#'
#' @param TU Tumor Identification SDTM dataset with variables USUBJID, TUDTC,
#' VISIT, TUORRES, TULOC, TUSPID (optional)
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
#' @author Will Harris
#'
#' @examples
#' TU <- data.frame(
#' USUBJID = 1:10,
#' TUDTC = 1:10,
#' VISIT = "C1D1",
#' TUORRES = "TARGET",
#' TULOC = "LIVER",
#' TUSPID = "FORMNAME-R:19/L:19XXXX",
#' stringsAsFactors=FALSE
#' )
#'
#' check_tu_tuloc_missing(TU)
#'
#' TU$TULOC[1] = "NA"
#' TU$TULOC[2] = ""
#' TU$TULOC[3] = NA
#'
#' check_tu_tuloc_missing(TU,preproc=roche_derive_rave_row)
#'
#' TU$TUSPID <- NULL
#' check_tu_tuloc_missing(TU)
#'
#'


check_tu_tuloc_missing <- function(TU,preproc=identity,...) {

    ###First check that required variables exist and return a message if they don't
    if(TU %lacks_any% c("USUBJID","TUDTC","VISIT","TUORRES","TULOC")){

        fail(lacks_msg(TU, c("USUBJID","TUDTC","VISIT","TUORRES","TULOC")))

    } else{

        #Apply company specific preprocessing function
        TU = preproc(TU,...)

        if(TU %lacks_any% "TUEVAL"){
            ### Subset TU to only target lesions with missing TULOC
            mydf = TU %>%
                filter(is_sas_na(TULOC),toupper(TUORRES)=="TARGET") %>%
                select(any_of(c("USUBJID", "TUDTC","VISIT","TUORRES","TULOC", "RAVE")))

        }else{
            mydf = TU %>%
                filter(is_sas_na(TULOC),toupper(TUORRES)=="TARGET",toupper(TUEVAL) == "INVESTIGATOR" | is_sas_na(TUEVAL)) %>%
                select(any_of(c("USUBJID", "TUDTC","VISIT","TUORRES","TULOC", "RAVE")))
        }

        rownames(mydf)=NULL

        if (nrow(mydf)==0) {
            pass()
        } else{

            ### Return subset dataframe if there are records with missing TUDTC
            fail(paste("There are",nrow(mydf),"target lesions with missing TULOC. "),mydf)
        }
    }
}
