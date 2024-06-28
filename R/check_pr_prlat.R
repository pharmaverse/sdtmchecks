#' @title Check if ocular procedures/surgeries has laterality missing for CRF pages which contain the word "OCULAR" (and not "NON-OCULAR").
#'
#' @description This check assesses observations where PRCAT contains the word OCULAR and flags records with missing/inconsistent laterality
#'
#' @param PR Procedure/Surgery Dataset for Ophtho Study with variables USUBJID,
#' PRCAT, PRLAT, PRTRT, PROCCUR, PRPRESP, PRSPID (if Present),
#' PRSTDTC (if Present), PRINDC (if Present)
#'
#' @importFrom dplyr %>% filter mutate select intersect
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% filter select
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
#' @author Tim Barnett (HackR 2021 Team Eye) Monarch Shah (Added Concurrent Ocular Procedure in this check) (copied from check_cm_cmlat)
#'
#'
#' @examples
#'
#' PR <- data.frame(
#'    USUBJID = 1:5,
#'    PRCAT = "PRIOR OCULAR SURGERIES AND PROCEDURES",
#'    PRSTDTC = 1:5,
#'    PRLAT   = c("Left", "","Bilateral", "", ""),
#'    PRTRT   = c("A", "B", "A", "B", "A"),
#'    PROCCUR = c("Y", "N", "N", "Y", "Y"),
#'    PRPRESP = "Y",
#'    PRSPID  = "FORMNAME-R:2/L:2XXXX",
#'    stringsAsFactors = FALSE)
#' check_pr_prlat(PR,preproc=roche_derive_rave_row)
#'
#' PR <- data.frame(
#'    USUBJID = 1:5,
#'    PRCAT = "CONCURRENT OCULAR PROCEDURE",
#'    PRSTDTC = 1:5,
#'    PRLAT   = c("Left", "LEFT","Bilateral", "RIGHT", "RIgHT"),
#'    PRTRT  = c("A", "B", "A", "B", "A"),
#'    PROCCUR = NA,
#'    PRPRESP = NA,
#'    stringsAsFactors = FALSE)
#' check_pr_prlat(PR)
#'
#' PR <- data.frame(
#'    USUBJID = 1:5,
#'    PRCAT = "CONCURRENT OCULAR PROCEDURE",
#'    PRSTDTC = 1:5,
#'    PRLAT   = c("Left", "LEFT","Bilateral", "RIGHT", ""),
#'    PRTRT  = c("A", "B", "A", "B", "A"),
#'    PROCCUR = NA,
#'    PRPRESP = NA,
#'    stringsAsFactors = FALSE)
#' check_pr_prlat(PR)
#'
#' PR <- data.frame(
#'    USUBJID = 1:5,
#'    PRCAT = "CONCURRENT OCULAR PROCEDURE",
#'    PRSTDTC = 1:5,
#'    PRLAT   = c("Left", "","Bilateral", "RIGHT", ""),
#'    PRTRT  = c("A", "B", "A", "B", "A"),
#'    PROCCUR = c("Y", "N", "N", "Y", "Y"),
#'    PRPRESP = "Y",
#'    stringsAsFactors = FALSE)
#' check_pr_prlat(PR)
#'
#' PR <- data.frame(
#'    USUBJID = 1:5,
#'    PRCAT = c(rep("CONCURRENT NON-OCULAR PROCEDURE",3),rep("CONCURRENT OCULAR PROCEDURE",2)),
#'    PRSTDTC = 1:5,
#'    PRLAT   = c("", "","", "RIGHT", ""),
#'    PRTRT  = c("A", "B", "A", "B", "A"),
#'    PROCCUR = c("Y", "N", "N", "Y", "Y"),
#'    PRPRESP = "Y",
#'    stringsAsFactors = FALSE)
#' check_pr_prlat(PR)
#'
#'
#'


check_pr_prlat <- function(PR,preproc=identity,...) {

    if (PR %lacks_any% c("USUBJID", "PRCAT", "PRLAT", "PRTRT", "PROCCUR", "PRPRESP")) {

        fail(lacks_msg(PR, c("USUBJID", "PRCAT", "PRLAT", "PRTRT", "PROCCUR", "PRPRESP")))

        ### Dont run if relevant PRCAT not present
    } else if (PR %>% filter(grepl("OCULAR", toupper(PRCAT), fixed = T) & ! grepl("NON-OCULAR", toupper(PRCAT), fixed = T)) %>%
               select(PRCAT) %>% unique() %>% nrow()<1) {

        fail(msg="No data with PRCAT containing the word OCULAR")

    } else {

        #Apply company specific preprocessing function
        PR = preproc(PR,...)

        perm_var <- c("PRSTDTC", "RAVE", "PRINDC")
        int_var <- intersect(names(PR), perm_var)

        my_select_var <- c("USUBJID", "PRCAT", int_var, "PRLAT", "PRTRT")

        mydf = PR %>%
            filter(grepl("OCULAR", toupper(PRCAT), fixed = T) & ! grepl("NON-OCULAR", toupper(PRCAT), fixed = T) &
                       ((PRPRESP == "Y" & PROCCUR == "Y") | (is_sas_na(PRPRESP) & is_sas_na(PROCCUR)))) %>%
            select(any_of(my_select_var)) %>%
            mutate(MISFLAG = ifelse(!(toupper(PRLAT) %in% c("LEFT", "RIGHT", "BILATERAL")), 1, 0))

        rownames(mydf)=NULL

        mydf <- mydf %>% filter(MISFLAG == 1) %>% select(-MISFLAG)

        if ((nrow(mydf) > 0 ) == FALSE) {
            pass()
        } else {
            fail(paste0(nrow(mydf),
                        " record(s) with PRLAT Missing from records with PRCAT containing the word OCULAR when expected to be populated. "),
                 mydf )

        }
    }
}

