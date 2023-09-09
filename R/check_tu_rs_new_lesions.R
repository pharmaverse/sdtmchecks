#' @title Check for consistency between new lesions and overall PD response
#'
#' @description This checks for patients with new lesions in TU (TUSTRESC=='NEW')
#'   but no Overall Response assessment of PD (Disease Progression) or 
#'   PMD (Progressive Metabolic Disease) in RS (i.e., (RSTESTCD=='OVRLRESP' and 
#'   RSSTRESC %in% c('PD','PMD'))). Only applies to assessments by investigator, 
#'   if TUEVAL and RSEVAL variables available. 
#'
#' @param TU Tumor Identification SDTM dataset with variables USUBJID, TUSTRESC,
#'   TUDTC
#'
#' @param RS Response SDTM dataset with variables USUBJID, RSSTRESC, RSTESTCD
#'
#' @return TRUE if check passed and FALSE if check failed + 'msg' and 'data'
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
#'  RSSTRESC = c("SD","NE")
#' )
#'
#' # required variable is missing 
#' check_tu_rs_new_lesions(RS,TU)
#'
#' RS$RSTESTCD = 'OVRLRESP'
#' 
#' # flag USUBJIDs with NEW 
#' check_tu_rs_new_lesions(RS,TU)
#'
#'
#' RS$RSSTRESC[2] = "PD"
#' 
#' # flag USUBJID with NEW and without PD
#' check_tu_rs_new_lesions(RS,TU)
#'    
#' # Metabolic response in heme trials
#' RS$RSSTRESC[2] = "PMD"
#' check_tu_rs_new_lesions(RS,TU)
#' 
#' 
#' # pass when USUBJIDs with new have PD
#' RS <- data.frame(
#'  USUBJID = 1:3,
#'  RSSTRESC = c("SD","PD", "PD"), 
#'  RSTESTCD = "OVRLRESP"
#' )
#' 
#' check_tu_rs_new_lesions(RS,TU)
#' 
#' TU$TUEVAL = "INDEPENDENT ASSESSOR"
#' 
#' RS$RSEVAL = "INDEPENDENT ASSESSOR"
#' 
#' ## pass if by IRF, even if NEW in TU
#' check_tu_rs_new_lesions(RS,TU)
#'
#' RS <- NULL
#' 
#' # required dataset missing 
#' check_tu_rs_new_lesions(RS,TU)
#' 


check_tu_rs_new_lesions <- function(RS, TU) {

    ###First check that required variables exist and return a message if they don't
    if (TU %lacks_any% c("USUBJID", "TUSTRESC", "TUDTC")) {
        return(fail(lacks_msg(TU, c("USUBJID", "TUSTRESC", "TUDTC"))))
    }

    if (RS %lacks_any% c("USUBJID", "RSSTRESC", "RSTESTCD")) {
        return(fail(lacks_msg(RS, c("USUBJID", "RSSTRESC", "RSTESTCD"))))
    }


    ### Find new lesions in TU and overall PD or PMD responses in RS 

    if (TU %lacks_any% "TUEVAL") {
        mytu = subset(TU, TU$TUSTRESC == "NEW")
    } else {
        mytu = subset(TU, TU$TUSTRESC == "NEW" & (toupper(TU$TUEVAL) == "INVESTIGATOR" | is_sas_na(TU$TUEVAL)))
    }

    if (RS %lacks_any% "RSEVAL") {
        myrs = unique(subset(RS, RS$RSTESTCD == "OVRLRESP" & RS$RSSTRESC %in% c("PD","PMD"), "USUBJID"))
    } else {
        myrs = unique(subset(RS, RS$RSTESTCD == "OVRLRESP" & RS$RSSTRESC %in% c("PD","PMD") & (toupper(RS$RSEVAL) == "INVESTIGATOR" | is_sas_na(RS$RSEVAL)), c("USUBJID")))
    }

    keeper_vars = intersect(names(TU),c("USUBJID","TUSTRESC","TUDTC","VISIT"))
    mydf = unique(subset(mytu, !(mytu$USUBJID %in% myrs$USUBJID), keeper_vars))
    rownames(mydf) = NULL
    

    ###Print to report

    ### Return message if no inconsistency
    if (nrow(mydf) == 0) {
        pass()

        ### Return subset dataframe if there are records with inconsistency
    } else if (nrow(mydf) > 0) {

        fail(paste("TU has ", length(unique(mydf$USUBJID)),
                   " patient(s) with a new lesion but no Overall Response indicating progression. ",sep = ""),
             mydf)
    }
}
