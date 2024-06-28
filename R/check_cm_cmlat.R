#' @title Check if ocular concomitant medication has laterality missing or laterality field is
#'        populated but route is not eye-related.
#'
#' @description This check assesses CMCAT = "CONCOMITANT MEDICATIONS" and flags potential
#'              ocular records with missing/inconsistent route and laterality: for eye-related
#'              CMROUTE ('INTRAVITREAL', 'OPHTHALMIC', etc.), CMLAT is not populated -or- CMROUTE is not eye-related
#'              (i.e., not INTRAVITREAL, OPHTHALMIC, TOPICAL, etc.) but CMLAT is LEFT/RIGHT/BILATERAL.
#'
#' @param CM Concomitant Medications Dataset for Ophtho Study with variables
#'           USUBJID, CMCAT, CMLAT, CMDECOD, CMTRT, CMROUTE, CMSPID (if Present), CMSTDTC (if Present)
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% filter mutate select
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
#'
#' @examples
#'
#' CM <- data.frame(
#'    USUBJID = 1:7,
#'    CMCAT = "CONCOMITANT MEDICATIONS",
#'    CMSTDTC = 1:7,
#'    CMLAT   = c("Left", "","Bilateral", "", "", "LEFT", ""),
#'    CMTRT  = c("A", "B", "A", "B", "A", "A", "B"),
#'    CMDECOD = c("A", "B", "A", "B", "A", "A", "B"),
#'    CMROUTE = c("","OPHTHALMIC","INTRAVITREAL","INTRAVITREAL",
#'                "opHTHALMIC", "INTRaOCULAr", "INTRaOCULAr"),
#'    CMSPID  = "FORMNAME-R:13/L:13XXXX",
#'    stringsAsFactors = FALSE)
#' check_cm_cmlat(CM,preproc=roche_derive_rave_row)
#'
#' CM <- data.frame(
#'    USUBJID = 1:5,
#'    CMCAT = rep("CONCOMITANT MEDICATIONS",5),
#'    CMSTDTC = 1:5,
#'    CMLAT   = c("Left", "LEFT","Bilateral",
#'                "RIGHT", "RIgHT"),
#'    CMTRT  = c("A", "B", "A", "B", "A"),
#'    CMDECOD = c("A", "B", "A", "B", "A"),
#'    CMROUTE = c("","OPHTHALMIC","INTRAVITREAL",
#'                "INTRaOCULAr", "opHTHALMIC"),
#'    stringsAsFactors = FALSE)
#' check_cm_cmlat(CM)
#'
#' CM <- data.frame(
#'    USUBJID = 1:5,
#'    CMCAT = "CONCOMITANT MEDICATIONS",
#'    CMSTDTC = 1:5,
#'    CMLAT   = c("Left", "LEFT","Bilateral", "RIGHT", "RIgHT"),
#'    CMTRT  = c("A", "B", "A", "B", "A"),
#'    CMDECOD = c("A", "B", "A", "B", "A"),
#'    #CMROUTE = c("","OPHTHALMIC","INTRAVITREAL","INTRaOCULAr", "opHTHALMIC"),
#'    stringsAsFactors = FALSE)
#' check_cm_cmlat(CM)
#'


check_cm_cmlat <- function(CM,preproc = identity,...) {

    if (CM %lacks_any% c("USUBJID", "CMCAT", "CMLAT", "CMTRT", "CMDECOD", "CMROUTE")) {

        fail(lacks_msg(CM, c("USUBJID", "CMCAT", "CMLAT", "CMTRT", "CMDECOD", "CMROUTE")))
    }

    else {

        #Apply company specific preprocessing function
        CM = preproc(CM,...)

        perm_var <- c("CMSTDTC", "RAVE")
        int_var <- intersect(names(CM), perm_var)

        mydf = CM %>%
               filter(CMCAT == "CONCOMITANT MEDICATIONS") %>%
               mutate(MISFLAG =  ifelse((toupper(CMROUTE) %in% c("OPHTHALMIC", "INTRAVITREAL", "INTRAOCULAR",
                                                                 "CONJUNCTIVAL", "INTRACAMERAL", "INTRACORNEAL",
                                                                 "RETROBULBAR", "SUBTENON", "SUBRETINAL",
                                                                 "SUBCONJUNCTIVAL") &
                                        !(toupper(CMLAT) %in% c("LEFT", "RIGHT", "BILATERAL"))) |
                                        (!(toupper(CMROUTE) %in% c("OPHTHALMIC", "INTRAVITREAL", "INTRAOCULAR",
                                                                   "CONJUNCTIVAL", "INTRACAMERAL", "INTRACORNEAL",
                                                                   "RETROBULBAR", "SUBTENON", "SUBRETINAL",
                                                                   "TOPICAL")) &
                                        (toupper(CMLAT) %in% c("LEFT", "RIGHT", "BILATERAL"))), 1, 0))


        my_select_var <- c("USUBJID", int_var,  "CMLAT", "CMTRT", "CMDECOD", "CMROUTE", "MISFLAG")
        mydf <- mydf[,my_select_var]

        rownames(mydf)=NULL

        mydf <- mydf %>% filter(MISFLAG == 1) %>% select(-MISFLAG)

        if ((nrow(mydf) > 0 ) == FALSE) {
            pass()
        } else {
            fail(paste(nrow(mydf),
            "record(s) with CMLAT Missing when CM is Eye related, or CMLAT is LEFT/RIGHT/BILATERAL and CMROUTE is not Eye related."
            ), mydf )
        }
    }
}
