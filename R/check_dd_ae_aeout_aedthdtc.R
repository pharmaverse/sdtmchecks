#' @title Check if there is a death date and AEOUT='FATAL' agreement
#'
#' @description This check looks for AE death dates if AEOUT='FATAL' and for the
#'    reverse, i.e if there is an AE death date, then AEOUT should have the value
#'    "FATAL".
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AEDTHDTC,
#'    AEDECOD, AESTDTC and AEOUT
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% select
#' @importFrom tidyselect any_of
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author Joel Laxamana
#'
#' @examples
#'
#' AE <- data.frame(
#'  USUBJID = 1:3,
#'  AEDTHDTC = c("2020-01-01","2020-01-02","2020-01-03"),
#'  AEDECOD = 1:3,
#'  AESTDTC = 1:3,
#'  AEOUT = rep("FATAL", 3),
#'  AESPID = "FORMNAME-R:19/L:19XXXX",
#'  stringsAsFactors = FALSE
#' )
#'
#' # pass
#' check_dd_ae_aeout_aedthdtc(AE)
#'
#' # fail - 1 case (AEDTHDTC not populated but AEOUT == FATAL)
#' AE1 <- AE
#' AE1[3, "AEDTHDTC"] <- NA
#' check_dd_ae_aeout_aedthdtc(AE1)
#' check_dd_ae_aeout_aedthdtc(AE1,preproc=roche_derive_rave_row)
#'
#' # pass -- even though AEDTHDTC populated 
#' AE2 <- AE
#' AE2[1, "AEOUT"] <- NA
#' check_dd_ae_aeout_aedthdtc(AE2)
#' check_dd_ae_aeout_aedthdtc(AE2,preproc=roche_derive_rave_row)
#' 
#' # 2 cases 
#' AE[3, "AEDTHDTC"] <- NA
#' AE[1, "AEOUT"] <- NA
#' check_dd_ae_aeout_aedthdtc(AE)
#' check_dd_ae_aeout_aedthdtc(AE,preproc=roche_derive_rave_row)
#'
#' # 2 cases 
#' AE[1, "AEOUT"] <- 'NOT RECOVERED/NOT RESOLVED'
#' check_dd_ae_aeout_aedthdtc(AE)
#' check_dd_ae_aeout_aedthdtc(AE,preproc=roche_derive_rave_row)
#'
#' # non-critical variable missing
#' AE$AESPID <- NULL
#' check_dd_ae_aeout_aedthdtc(AE)
#' check_dd_ae_aeout_aedthdtc(AE,preproc=roche_derive_rave_row)
#' 
#' # critical variables are missing
#' AE$AEDTHDTC <- NULL
#' AE$USUBJID <- NULL
#' check_dd_ae_aeout_aedthdtc(AE)
#' check_dd_ae_aeout_aedthdtc(AE,preproc=roche_derive_rave_row)
#'

check_dd_ae_aeout_aedthdtc <- function(AE, preproc=identity,...) {

    if (AE %lacks_any% c("USUBJID", "AEDTHDTC", "AEOUT", "AEDECOD", "AESTDTC")) {

        fail(lacks_msg(AE, c("USUBJID", "AEDTHDTC", "AEOUT", "AEDECOD", "AESTDTC")))

    } else{

        #Apply company specific preprocessing function
        AE = preproc(AE,...)

        # check if AEOUT=='FATAL' that there is a corresponding AEDTHDTC, death date
        # check non-missing AEDTHDTC with AEOUT=='FATAL'
        mydf <- unique(subset(AE
                              , (AE$AEOUT == 'FATAL' & is_sas_na(AE$AEDTHDTC))
                               | (!is_sas_na(AE$AEDTHDTC) & AE$AEOUT != 'FATAL' | is_sas_na(AE$AEOUT))
                              , )) %>%
            select(any_of(c("USUBJID", "AEDECOD", "AESTDTC","AEDTHDTC", "AEOUT","RAVE")))
        

        if (nrow(mydf) > 0) {
            fail(paste0(nrow(mydf), ' record(s) with a discrepant AE outcome and AE death date. '), mydf)
        } else if (nrow(mydf) == 0) {
            pass()
        }
    }
}

