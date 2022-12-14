#' @title Check for consistency between new lesions and overall PD response
#'
#' @description This checks for patients with new lesions in TU (TUSTRESC='NEW')
#'   but no overall response of PD in RS. Only applies to assessments by
#'   investigator.
#'
#' @param TU Tumor Identification SDTM dataset with variables USUBJID, TUSTRESC,
#'   TUDTC
#'
#' @param RS Response SDTM dataset with variables USUBJID, RSSTRESC, RSTESTCD
#'
#' @return TRUE if check is passed and FALSE if check failed + 'msg' and 'data'
#'   attributes
#'
#' @export
#'
#' @author Will Harris
#'
#' @examples
#'
#' TU <- data.frame(
#'  USUBJID = 1:3,
#'  TUSTRESC = c("INV001","NEW","NEW"),
#'  TUDTC = "2017-01-01"
#' )
#'
#' RS <- data.frame(
#'  USUBJID = 1:2,
#'  RSTESTCD = "OVRLRESP",
#'  RSSTRESC = c("SD","NE")
#' )
#'
#' check_tu_rs_new_lesions(RS,TU)
#'
#'



check_tu_rs_new_lesions <- function(RS, TU) {

    ###First check that required variables exist and return a message if they don't
    if (TU %lacks_any% c("USUBJID", "TUSTRESC", "TUDTC")) {
        return(fail(lacks_msg(TU, c("USUBJID", "TUSTRESC", "TUDTC"))))
    }

    if (RS %lacks_any% c("USUBJID", "RSSTRESC", "RSTESTCD")) {
        return(fail(lacks_msg(RS, c("USUBJID", "RSSTRESC", "RSTESTCD"))))
    }


    ### Find new lesions in TU and overall PD responses in RS

    if (TU %lacks_any% "TUEVAL") {
        mytu = unique(subset(TU, TU$TUSTRESC == "NEW", c("USUBJID", "TUDTC")))
    } else {
        mytu = unique(subset(TU, TU$TUSTRESC == "NEW" & (toupper(TU$TUEVAL) == "INVESTIGATOR" | is_sas_na(TU$TUEVAL)), c("USUBJID", "TUDTC")))
    }

    if (RS %lacks_any% "RSEVAL") {
        myrs = unique(subset(RS, RS$RSTESTCD == "OVRLRESP" & RS$RSSTRESC == "PD", "USUBJID"))
    } else {
        myrs = unique(subset(RS, RS$RSTESTCD == "OVRLRESP" & RS$RSSTRESC == "PD" & (toupper(RS$RSEVAL) == "INVESTIGATOR" | is_sas_na(RS$RSEVAL)), "USUBJID"))
    }

    mydf = subset(mytu, !(mytu$USUBJID %in% myrs$USUBJID))
    rownames(mydf) = NULL

    ###Print to report

    ### Return message if no inconsistency
    if (nrow(mydf) == 0) {
        pass()

        ### Return subset dataframe if there are records with inconsistency
    } else if (nrow(mydf) > 0) {

        fail(paste("There are ",length(unique(mydf$USUBJID)),
                   " patients with new lesions but no overall response of PD. ",sep = ""),
             mydf)
    }
}
