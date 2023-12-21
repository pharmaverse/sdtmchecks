#' @title Check for TR records with missing TRSTRESN for Longest Diameter (LDIAM)
#'
#' @description This checks looks for TR records with missing values in numeric
#'   result/finding for the Longest Diameter (TRTESTCD is LDIAM) tumor measurement. 
#'   Only applies to assessments by investigator, selected based on uppercased 
#'   TREVAL = "INVESTIGATOR" or missing or TREVAL variable does not exist.
#'
#' @param TR Tumor Results SDTM dataset with variables USUBJID, TRTESTCD,
#'   TRLINKID/TRLNKID, TRDTC, VISIT, TRORRES, TRSTRESN, TREVAL (optional),
#'   TRSTAT (optional), TRSPID (optional)
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#'
#' @return Boolean value for whether the check passed or failed, with 'msg'
#'   attribute if the check failed
#'
#' @export
#'
#' @author Will Harris
#'
#' @examples
#'
#' TR <- data.frame(USUBJID = 1:5,
#'                  TRTESTCD = c("OTHER", rep("LDIAM", 4)),
#'                  TRLINKID = 1:5,
#'                  TRDTC = 1:5,
#'                  VISIT = LETTERS[1:5],
#'                  TRORRES = LETTERS[1:5],
#'                  TRSTRESN = 1:5,
#'                  TRSTAT = "",
#'                  TREVAL = "INVESTIGATOR",
#'                  TRSPID = "FORMNAME-R:19/L:19XXXX",
#'                  stringsAsFactors = FALSE)
#'
#' check_tr_trstresn_ldiam(TR)
#'
#' TR1 <- TR
#' TR1$TRSTAT <- NULL
#' TR1$TREVAL <- NULL
#' TR1$TRSPID <- NULL
#'
#' check_tr_trstresn_ldiam(TR1)
#'
#' TR2 <- TR
#' TR2$TRSTRESN <- c("", "NA", NA, 1, 1)
#'
#' check_tr_trstresn_ldiam(TR2)
#' check_tr_trstresn_ldiam(TR2,preproc=roche_derive_rave_row)
#'
#'

check_tr_trstresn_ldiam <- function(TR,preproc=identity,...) {
    
    if ((TR %lacks_any% c("USUBJID", "TRTESTCD", "TRDTC", "VISIT", "TRORRES", "TRSTRESN")) || (TR %lacks_all% c("TRLINKID","TRLNKID"))) {
        
        fail(lacks_msg(TR, c("USUBJID", "TRTESTCD", "TRLINKID","TRLNKID", "TRDTC", "VISIT", "TRORRES", "TRSTRESN")))
        
    } else if (!("LDIAM" %in% TR[["TRTESTCD"]])) {
        
        fail("No records with TR.TRTESTCD = 'LDIAM' found")
        
    } else {
        
        #Apply company specific preprocessing function
        TR = preproc(TR,...)
        
        myvars <- c("USUBJID","TRTESTCD","TRDTC", "VISIT", "TRORRES", "TRSTRESN",names(TR)[names(TR) %in% c("RAVE", "TRLINKID","TRLNKID","TRSTAT")])
        if(TR %lacks_any% "TREVAL"){
            df <- subset(TR,TR$TRTESTCD=="LDIAM" & is_sas_na(TR$TRSTRESN), myvars)
        }else{
            df <- subset(TR,TR$TRTESTCD=="LDIAM" & is_sas_na(TR$TRSTRESN) & (toupper(TR$TREVAL) == "INVESTIGATOR" | is_sas_na(TR$TREVAL)), myvars)
        }
        
        if (nrow(df)==0) {
            
            pass()
            
        } else {
            
            #x not done
            x = nrow(df[df$TRORRES %in% c("NOT DONE","ND"),])
            if("TRSTAT" %in% names(df)){
                x = x + nrow(df[df$TRSTAT=="NOT DONE",])
            }
            #y not evaluable
            y = nrow(df[df$TRORRES %in% c("NOT EVALUABLE","NE"),])
            #z done but missing 
            z = nrow(df)-x-y
            
            fail(paste0("TR has ", 
                        nrow(df), 
                        " record(s) with missing TRSTRESN values for LDIAM assessment. ",
                        x," record(s) indicate 'NOT DONE'. ",
                        y," record(s) indicate 'NOT EVALUABLE'. ",
                        z," record(s) indicate done and evaluable but otherwise missing."
            ), df)
        }
    }
}