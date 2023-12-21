#' @title Check TU Records where the same date occurs across multiple visits
#'
#' @description This check identifies records where the same date TUDTC occurs
#'  across multiple visits. Only applies to assessments by investigator, 
#'  selected based on uppercased TUEVAL = "INVESTIGATOR" or missing or 
#'  TUEVAL variable does not exist.
#'
#' @param TU Tumor Identification SDTM dataset with variables USUBJID, TUDTC, VISIT,
#' TUEVAL (optional), TUTESTCD (optional)
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed.
#'
#' @export
#'
#' @importFrom dplyr %>% filter select left_join
#' @importFrom stats aggregate
#' @importFrom tidyselect any_of
#'
#' @author Will Harris
#'
#' @examples
#'
#' # records flagged 
#' TU <- data.frame(USUBJID = 1,
#'                  TUDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
#'                  VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
#'                  TUSPID = "FORMNAME-R:13/L:13XXXX",
#'                  stringsAsFactors=FALSE)
#'
#' check_tu_tudtc_across_visit(TU)
#' check_tu_tudtc_across_visit(TU, preproc=roche_derive_rave_row)
#'
#' # no records flagged because non-Investigator results
#' TU2 <- TU
#' TU2$TUEVAL <- "INDEPENDENT ASSESSOR"
#'
#' check_tu_tudtc_across_visit(TU2)
#' check_tu_tudtc_across_visit(TU2, preproc=roche_derive_rave_row)
#' 
#' # example with TUTESTCD and with records flagged
#' TU3 <- TU
#' TU3$TUTESTCD = c(rep("TUMIDENT", 2), rep("OTHER", 2), 
#'    rep("TUMIDENT", 2), rep("OTHER", 2), rep("TUMIDENT", 2))
#' check_tu_tudtc_across_visit(TU3)
#' check_tu_tudtc_across_visit(TU3, preproc=roche_derive_rave_row)
#' 
#' 
#' # example without TUSPID and with records flagged
#' TU4 <- TU
#' TU4$TUSPID <- NULL
#' 
#' check_tu_tudtc_across_visit(TU4)
#' check_tu_tudtc_across_visit(TU4, preproc=roche_derive_rave_row)
#' 
#' # example with required variable missing
#' TU5 <- TU
#' TU5$VISIT <- NULL
#' 
#' check_tu_tudtc_across_visit(TU5)
#' check_tu_tudtc_across_visit(TU5, preproc=roche_derive_rave_row)
#' 

check_tu_tudtc_across_visit <- function(TU, preproc=identity,...) {
    
    ###First check that required variables exist and return a message if they don't
    if((TU %lacks_any% c("USUBJID", "TUDTC", "VISIT"))){
        
        fail(lacks_msg(TU, c("USUBJID", "TUDTC", "VISIT")))
        
    } else{
        
        #Apply company specific preprocessing function
        TU = preproc(TU,...)
        
        ### Find unique pairs of TUDTC and VISIT per USUBJID
        
        if(TU %lacks_any% "TUEVAL"){
            tusub = TU %>%
                select(USUBJID, TUDTC, VISIT, any_of(c("TUTESTCD","RAVE")))
        }else{
            tusub = TU %>%
                filter(toupper(TUEVAL) == "INVESTIGATOR" | is_sas_na(TUEVAL)) %>%
                select(USUBJID, TUDTC, VISIT, any_of(c("TUTESTCD","RAVE")))
        }
        
        tu_orig=tusub #Save RAVE for merging in later
        tusub = tusub %>% 
            select(-any_of(c("TUTESTCD","RAVE"))) #dont want to unique on RAVE var
        
        if(nrow(tusub)>0){
            
            #get unique visit/date pairs per patients
            mypairs = unique(tusub) 
            mypairs$x = 1
            
            ### Get counts of visit values per date for each subject
            mydf0 = aggregate(mypairs$x,by=list(USUBJID=mypairs$USUBJID, TUDTC=mypairs$TUDTC),FUN=sum)
            
            ### Subset where count is >1
            mydf0 = mydf0 %>%
                filter(x>1) %>%
                select(USUBJID, TUDTC) 
            
            mypairs0 = mypairs %>%
                select(USUBJID, TUDTC,VISIT)
            
            # subset unique pairs to only instances where visit has >1 date
            mydf = merge(mydf0,mypairs0,by=c('USUBJID','TUDTC'),all.x = TRUE) %>% 
                left_join(tu_orig,by=c("USUBJID", "TUDTC", "VISIT")) %>% #merge in RAVE var if it exists
                unique()
            
            rownames(mydf)=NULL
        }else{
            mydf=data.frame()
        }
        
        
        ### if no consistency
        if (nrow(mydf)==0) {
            pass()
        } else if (nrow(mydf) > 0) {
            ### Return subset dataframe if there are records with inconsistency
            fail(paste("There are",nrow(mydf),"TU records where the same date occurs accross multiple visits. "),
                 mydf)
        }
    }
}
