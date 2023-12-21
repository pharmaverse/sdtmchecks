#' @title Check TR Longest Diameter Records where the same date occurs across multiple visits
#'
#' @description This check identifies records where the same date TRDTC occurs
#'  across multiple visits for Longest Diameter measurements (TRTESTCD is "LDIAM").
#'  Only applies to assessments by investigator, selected based on uppercased 
#'  TREVAL = "INVESTIGATOR" or missing or TREVAL variable does not exist.
#'
#' @param TR Tumor Result SDTM dataset with variables USUBJID, TRDTC, TRTESTCD, VISIT, 
#' TREVAL (optional)
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
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
#' TR <- data.frame(
#' USUBJID = 1,
#' TRDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
#' VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
#' TRTESTCD = c(rep("LDIAM",7),rep("SAXIS",3)),
#' TRSPID = "FORMNAME-R:13/L:13XXXX",
#' stringsAsFactors=FALSE)
#'
#' check_tr_trdtc_across_visit(TR)
#' check_tr_trdtc_across_visit(TR, preproc=roche_derive_rave_row)
#' 
#' TR2 <- TR
#' TR2$TRSPID[4:5] <- c("FORMNAME2-R:5/L:13XXXX", "FORMNAME3-R:0/L:13XXXX")
#' 
#' check_tr_trdtc_across_visit(TR2)
#' check_tr_trdtc_across_visit(TR2, preproc=roche_derive_rave_row)
#'
#' # missing optional variable
#' TR3 <- TR
#' TR3$TRSPID <- NULL
#' 
#' check_tr_trdtc_across_visit(TR3)
#' check_tr_trdtc_across_visit(TR3, preproc=roche_derive_rave_row)
#' 
#' # missing required variable
#' TR4 <- TR
#' TR4$TRTESTCD <- NULL
#' 
#' check_tr_trdtc_across_visit(TR4)
#' check_tr_trdtc_across_visit(TR4, preproc=roche_derive_rave_row)
#'

check_tr_trdtc_across_visit <- function(TR, preproc=identity,...) {
    
    ###First check that required variables exist and return a message if they don't
    if((TR %lacks_any% c("USUBJID", "TRDTC", "VISIT", "TRTESTCD"))){
        
        fail(lacks_msg(TR, c("USUBJID", "TRDTC", "VISIT", "TRTESTCD")))
        
    } else{
        
        #Apply company specific preprocessing function
        TR = preproc(TR,...)
        
        ### Find unique pairs of TRDTC and VISIT per USUBJID
        
        if(TR %lacks_any% "TREVAL"){
            trsub = TR %>%
                filter(TRTESTCD == "LDIAM") %>%
                select(USUBJID, TRDTC, VISIT, TRTESTCD,any_of("RAVE"))
        }else{
            trsub = TR %>%
                filter(TRTESTCD == "LDIAM" & (toupper(TREVAL) == "INVESTIGATOR" | is_sas_na(TREVAL))) %>%
                select(USUBJID, TRDTC, VISIT, TRTESTCD,any_of("RAVE"))
        }
        
        tr_orig = trsub #Save RAVE for merging in later
        trsub = trsub %>% select(-any_of("RAVE")) #dont want to unique on RAVE var
        
        if(nrow(trsub)>0){
            
            mypairs = unique(trsub)
            mypairs$x = 1
            
            ### Get counts of visit values per date for each subject
            mydf0 = aggregate(mypairs$x, by=list(USUBJID=mypairs$USUBJID, TRDTC=mypairs$TRDTC),FUN=sum)
            
            ### Subset where count is >1 and output
            mydf0 = mydf0 %>%
                select("USUBJID", "TRDTC") %>%
                filter(mydf0$x>1)
            
            mypairs0 = mypairs %>%
                select("USUBJID", "TRDTC", "VISIT", "TRTESTCD")
            
            mydf = merge(mydf0, mypairs0, by=c("USUBJID", "TRDTC"), all.x = TRUE) %>% 
                left_join(tr_orig, by=c("USUBJID", "TRDTC", "VISIT", "TRTESTCD")) %>% #merge in RAVE var if it exists
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
            fail(paste("There are",nrow(mydf),"TR Longest Diameter records where the same date occurs accross multiple visits. "),
                 mydf)
        }
    }
}
