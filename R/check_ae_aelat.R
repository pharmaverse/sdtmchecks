#' @title Check if AESOC has Eye, and Affected Eye is missing
#'
#' @description This check looks if AESOC has Eye, and AELAT is missing.
#'
#' @param AE Adverse Event Dataset for Ophtho Study with variables USUBJID, AELAT, AESOC,
#'              AEDECOD, AETERM, AESTDTC (if present), AESPID (if present)
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% filter select mutate
#'
#' @family OPHTH
#'
#' @keywords OPHTH
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @author Monarch Shah (HackR 2021 Team Eye)
#'
#' @examples
#'
#' AE <- data.frame(
#'    USUBJID = 1:5,
#'    AESTDTC = 1:5,
#'    AELOC   = c("", "EYE", "eye", "", "EYE"),
#'    AELAT   = c("Left", "","left", "RIGHT", ""),
#'    AETERM  = c("A", "B", "A", "B", "A"),
#'    AEDECOD = c("A", "B", "A", "B", "A"),
#'    AESOC   = c("Eye", "Eye","Eye Disorder","Eye Disorder", "Eye"),
#'    AESPID  = "FORMNAME-R:19/L:19XXXX",
#'    stringsAsFactors = FALSE)
#'
#' check_ae_aelat(AE)
#' check_ae_aelat(AE,preproc=roche_derive_rave_row)
#'
#' AE <- data.frame(
#'    USUBJID = 1:5,
#'    AESTDTC = 1:5,
#'    AELAT   = c("Left", "","Bilateral", "", ""),
#'    AETERM  = c("A", "B", "A", "B", "A"),
#'    AEDECOD = c("A", "B", "A", "B", "A"),
#'    AESOC   = c("Eye", "Eye","Eye Disorder","Eye Disorder", "Eye"),
#'    stringsAsFactors = FALSE)
#'
#' check_ae_aelat(AE)
#' check_ae_aelat(AE,preproc=roche_derive_rave_row)
#'
#'


check_ae_aelat <- function(AE,preproc=identity,...) {

    if (AE %lacks_any% c("USUBJID", "AELAT", "AESOC", "AEDECOD", "AETERM")) {

        fail(lacks_msg(AE, c("USUBJID", "AELAT", "AESOC", "AEDECOD", "AETERM")))
    }

    else {

        #Apply company specific preprocessing function
        AE = preproc(AE,...)

        perm_var <- c("AESTDTC", "RAVE")
        int_var <- intersect(names(AE), perm_var)

        mydf = AE %>%
               mutate(MISFLAG =  ifelse(((grepl("Eye", AESOC, ignore.case = TRUE))) &
                                         (!(toupper(AELAT) %in% c("LEFT", "RIGHT", "BILATERAL"))), 1, 0))

        my_select_var <- c("USUBJID", int_var,  "AELAT", "AESOC", "AEDECOD", "AETERM", "MISFLAG")

        mydf <- mydf[,my_select_var] %>%
            filter(MISFLAG == 1) %>%
            select(-MISFLAG)

        rownames(mydf)=NULL

        if ((nrow(mydf) > 0 ) == FALSE) {
            pass()
        } else {
            fail(paste0(nrow(mydf), " record(s) with AELAT Missing, when AE is Eye related. "), mydf )
        }
    }

}
