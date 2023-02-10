#' @title Check for missing lab units (LBSTRESU)
#'
#' @description This check identifies records where original lab values (LBORRES)
#' exist but standard lab units (LBSTRESU) are not populated, excluding
#' qualitative results (LBMETHOD) and excluding records when LBTESTCD in
#' ("PH" "SPGRAV")
#'
#' @param LB Lab SDTM dataset with variables USUBJID, LBSTRESC, LBSTRESN,
#'   LBORRES, LBSTRESU, LBTESTCD, LBDTC, LBMETHOD (optional),
#'   LBSPID (optional), and VISIT (optional)
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author Iris Zhao
#'
#' @examples
#'
#' LB <- data.frame(
#' USUBJID = 1:10,
#' LBSTRESC = "5",
#' LBSTRESN = 1:10,
#' LBORRES = "5",
#' LBSTRESU = "g/L",
#' LBTESTCD = "ALB",
#' LBDTC = 1:10,
#' stringsAsFactors=FALSE
#' )
#'
#' check_lb_lbstresu(LB)
#'
#' LB$LBSTRESU[1]=""
#' check_lb_lbstresu(LB)
#'
#' LB$LBSTRESU[2]="NA"
#' check_lb_lbstresu(LB)
#'
#' LB$LBSTRESU[3]=NA
#' check_lb_lbstresu(LB)
#'
#' LB$LBSPID= "FORMNAME-R:2/L:2XXXX"
#' check_lb_lbstresu(LB,preproc=roche_derive_rave_row)
#'
#' LB$VISIT= "SCREENING"
#' check_lb_lbstresu(LB)
#'
#' LB$LBSTRESU=NULL
#' check_lb_lbstresu(LB)
#'

check_lb_lbstresu <- function(LB,preproc=identity,...){
    
    ###Check that required variables exist and return a message if they don't.
    if(LB %lacks_any% c("USUBJID", "LBSTRESC", "LBSTRESN", "LBSTRESU", "LBORRES",
                        "LBTESTCD", "LBDTC")) {
        fail(lacks_msg(LB, c("USUBJID", "LBSTRESC", "LBSTRESN", "LBSTRESU",
                             "LBTESTCD", "LBDTC")))
    } else{
        
        #Apply company specific preprocessing function
        LB = preproc(LB,...)
        
        # Exclude records with qualitative results
        if ("LBMETHOD" %in% names(LB)) {
            LB <- LB %>%
                filter(!grepl("QUALITATIVE", LBMETHOD))
        }
        
        # Exclude records marked as not done
        if ("LBSTAT" %in% names(LB)) {
            LB <- LB %>%
                filter(!grepl("NOT DONE", LBSTAT,ignore.case=TRUE))
        }
        
        
        # Subset LB to fewer variables
        df <- LB %>%
            select(any_of(c('USUBJID','LBTESTCD','LBORRES','LBSTRESU','LBSTRESC','LBDTC','RAVE','VISIT')))
        
        ### Exclude particular labs known to be unitless
        df <- df %>%
            filter(LBTESTCD != "PH" & LBTESTCD != "SPGRAV" & !grepl("^NEGATIVE$|^POSITIVE$|^NOT DONE$",LBORRES,ignore.case=TRUE))
        
        ### Subset LB to records with missing lab units and non-missing lab test results
        missingunit <- df %>%
            filter(is_sas_na(LBSTRESU) & !is_sas_na(LBORRES))
        
        rownames(missingunit) = NULL
        
        if (nrow(missingunit)==0){
            pass()
        }
        else if (nrow(missingunit)>0) {
            fail(paste(length(unique(missingunit$USUBJID)),
                       "unique patient(s) with", nrow(missingunit),
                       "record(s) with missing lab units and non-missing test results. "),
                 missingunit)
        }
        
    }
    
}