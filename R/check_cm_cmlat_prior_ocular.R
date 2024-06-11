#' @title Check if ocular concomitant medication has laterality missing for specific
#' "PRIOR OCULAR THERAPIES AND TREATMENTS" (or similar names) CRF page.
#'
#' @description This check assesses ocular CMCAT records and flags records with
#' missing/inconsistent laterality
#'
#' @param CM Concomitant Medications Dataset for Ophtha Study with variables USUBJID, CMCAT, CMLAT, CMTRT, CMSPID
#'           (if Present), CMSTDTC (if Present), CMLOC (if Present), CMINDC (if Present), CMDOSFRM (if Present)
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% filter mutate select
#' @importFrom tidyselect any_of
#'
#' @family OPHTH
#'
#' @keywords OPHTH
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @author Tim Barnett (HackR 2021 Team Eye) (copied from check_cm_cmlat)
#'
#'
#' @examples
#'
#' CM <- data.frame(
#'    USUBJID = 1:5,
#'    CMCAT = "PRIOR OCULAR THERAPIES AND TREATMENTS",
#'    CMSTDTC = 1:5,
#'    CMLAT   = c("Left", "","Bilateral", "", ""),
#'    CMTRT   = c("A", "B", "A", "B", "A"),
#'    CMDECOD = c("A", "B", "A", "B", "A"),
#'    CMROUTE = c("","OPHTHALMIC","INTRAVITREAL","INTRAVITREAL", "opHTHALMIC"),
#'    CMSPID  = "FORMNAME-R:13/L:13XXXX",
#'    stringsAsFactors = FALSE)
#' check_cm_cmlat_prior_ocular(CM,preproc=roche_derive_rave_row)
#'
#' CM <- data.frame(
#'    USUBJID = 1:5,
#'    CMCAT = "Prior Ocular Therapies/Treatments",
#'    CMSTDTC = 1:5,
#'    CMLAT   = c("", "LEFT","Bilateral", "", "RIgHT"),
#'    CMTRT  = c("A", "B", "A", "B", "A"),
#'    CMDECOD = c("A", "B", "A", "B", "A"),
#'    #CMROUTE = c("","OPHTHALMIC","INTRAVITREAL","INTRAVITREAL", "opHTHALMIC"),
#'    stringsAsFactors = FALSE)
#' check_cm_cmlat_prior_ocular(CM)
#'
#' CM <- data.frame(
#'    USUBJID = 1:5,
#'    CMCAT = "CONCOMITANT MEDICATIONS",
#'    CMSTDTC = 1:5,
#'    CMLAT   = c("Left", "LEFT","Bilateral", "RIGHT", "RIgHT"),
#'    CMTRT  = c("A", "B", "A", "B", "A"),
#'    CMDECOD = c("A", "B", "A", "B", "A"),
#'    CMROUTE = c("","OPHTHALMIC","INTRAVITREAL","INTRAVITREAL", "opHTHALMIC"),
#'    stringsAsFactors = FALSE)
#' check_cm_cmlat_prior_ocular(CM)
#'
#' CM <- data.frame(
#'    USUBJID = 1:5,
#'    CMCAT = "CONCOMITANT MEDICATIONS",
#'    CMSTDTC = 1:5,
#'    CMLAT   = c("Left", "LEFT","Bilateral", "RIGHT", "RIgHT"),
#'    CMTRT  = c("A", "B", "A", "B", "A"),
#'    CMDECOD = c("A", "B", "A", "B", "A"),
#'    #CMROUTE = c("","OPHTHALMIC","INTRAVITREAL","INTRAVITREAL", "opHTHALMIC"),
#'    stringsAsFactors = FALSE)
#' check_cm_cmlat_prior_ocular(CM)
#'
#' CM <- data.frame(
#'    USUBJID = 1:5,
#'    CMCAT = c(rep("Prior Ocular Therapies/Treatments",3), rep("Non-Ocular Therapies/Treatments",2)),
#'    CMSTDTC = 1:5,
#'    CMLAT   = c("", "LEFT","Bilateral", "", ""),
#'    CMTRT  = c("A", "B", "A", "B", "A"),
#'    CMDECOD = c("A", "B", "A", "B", "A"),
#'    #CMROUTE = c("","OPHTHALMIC","INTRAVITREAL","ORAL", "ORAL"),
#'    stringsAsFactors = FALSE)
#' check_cm_cmlat_prior_ocular(CM)
#'
#'
#'


check_cm_cmlat_prior_ocular <- function(CM,preproc = identity,...) {

    if (CM %lacks_any% c("USUBJID", "CMCAT", "CMLAT", "CMTRT")) {

        fail(lacks_msg(CM, c("USUBJID", "CMCAT", "CMLAT", "CMTRT")))
    }

    else {
        #Apply company specific preprocessing function
        CM = preproc(CM,...)

        mydf = CM %>%
            filter(grepl("OCULAR", toupper(CMCAT), fixed = T) &
                   !grepl("NON-OCULAR", toupper(CMCAT), fixed = T)) %>%
            select(any_of(c("CMSTDTC", "RAVE", "CMLOC", "CMINDC", "CMDOSFRM","CMLAT", "CMTRT"))) %>%
            mutate(MISFLAG =  ifelse(!(toupper(CMLAT) %in% c("LEFT", "RIGHT", "BILATERAL")), 1, 0))

        mydf <- mydf %>% filter(MISFLAG == 1) %>% select(-MISFLAG)

        rownames(mydf)=NULL

        if ((nrow(mydf) > 0 ) == FALSE) {
            pass()
        } else {
            fail(paste0(nrow(mydf), " record(s) with CMLAT missing when expected to be populated. "), mydf )

        }
    }
}
