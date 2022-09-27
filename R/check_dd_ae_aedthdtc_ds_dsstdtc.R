#' @title Check if death date is the same in AE and DS domains
#'
#' @description This check compares death date in AE AEDTHDT with death date in
#'    DS DSSTDTC. It is expected that they are the same.
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID and AEDTHDTC
#' @param DS Disposition SDTM dataset with variables USUBJID, DSDECOD, DSSTDTC
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
#' @author Hiral Raval
#'
#' @examples
#'
#' AE <- data.frame(
#'  STUDYID = rep(1, 3),
#'  USUBJID = 1:3,
#'  AEDTHDTC = c("2020-01-01","2020-01-02","2020-01-03"),
#'  AESPID = "FORMNAME-R:19/L:19XXXX"
#' )
#'
#' DS <- data.frame(
#'  STUDYID = rep(1, 3),
#'  USUBJID = 1:3,
#'  DSDECOD = rep("DEATH", 3),
#'  DSSTDTC = c("2020-01-01","2020-01-02","2020-01-03"),
#'  DSSPID = "XXX-R:0",
#'  stringsAsFactors = FALSE
#' )
#'
#' # no case
#' check_dd_ae_aedthdtc_ds_dsstdtc(AE, DS)
#'
#' # 1 case
#' DS[3, "DSSTDTC"] <- "2000-01-01"
#' check_dd_ae_aedthdtc_ds_dsstdtc(AE, DS, preproc=roche_derive_rave_row)
#'
#' # check for non existence of vars
#' DS$DSDECOD <- NULL
#' DS$DSSTDTC <- NULL
#' check_dd_ae_aedthdtc_ds_dsstdtc(AE, DS)
#'



check_dd_ae_aedthdtc_ds_dsstdtc <- function(AE, DS, preproc=identity,...) {

    if (AE %lacks_any% c("USUBJID", "AEDTHDTC")) {

        fail(lacks_msg(AE, c("USUBJID", "AEDTHDTC")))

    } else if (DS %lacks_any% c("USUBJID", "DSDECOD", "DSSTDTC")) {

        fail(lacks_msg(DS, c("USUBJID", "DSDECOD", "DSSTDTC")))

    } else{

        #Apply company specific preprocessing function
        AE = preproc(AE,...)
        DS = preproc(DS,...)

        # From AE keep rows where the death date is populated
        ae0 <- subset(AE, !is_sas_na(AE$AEDTHDTC), ) %>%
            select(any_of(c("USUBJID", "AEDTHDTC","RAVE")))

        # From DS take DEATH records where DEATH date is populated
        ds0 <- subset(DS, !is_sas_na(DS$DSSTDTC)
                      & (regexpr("DEATH", DS$DSDECOD, ignore.case = TRUE) != -1)
                      ,) %>%
            select(any_of(c("USUBJID", "DSSTDTC","RAVE")))

        #Merge DS and AE and if death dates in both are different then output in mydf
        mydf0 = full_join(ds0, ae0, by = "USUBJID",suffix = c(".DS",".AE"))
        mydf <- unique(subset(mydf0, !(mydf0$AEDTHDTC == mydf0$DSSTDTC),))  %>%
            select(USUBJID,DSSTDTC,AEDTHDTC,everything())


        ds11 <- as.data.frame(unique(mydf$USUBJID))
        names(ds11) <- "USUBJID"
        rownames(ds11) = NULL

        n3 <- '';

        # declare number of patients
        n3 <- paste('There are ', nrow(ds11), ' patients with a death date different in DS and AE. ', sep = '')
        if (nrow(ds11) > 0) {
            fail(n3, mydf)
        }
        else if (nrow(ds11) == 0) {
            pass()
        }
    }
}

