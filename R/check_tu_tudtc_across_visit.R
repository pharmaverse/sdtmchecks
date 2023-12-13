#' @title Check TU Records where the same date occurs across multiple visits
#'
#' @description This check identifies records where the same date TUDTC occurs
#'  across multiple visits. Only applies to assessments by investigator.
#'
#' @param TU Tumor Identification SDTM dataset with variables USUBJID, TUDTC, VISIT
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed.
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#'
#' @author Will Harris
#'
#' @examples
#'
#' TU <- data.frame(USUBJID = 1,
#'                  TUDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
#'                  VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
#'                  stringsAsFactors=FALSE)
#'
#' check_tu_tudtc_across_visit(TU)
#'

check_tu_tudtc_across_visit <- function(TU) {

    ###First check that required variables exist and return a message if they don't
    if((TU %lacks_any% c("USUBJID", "TUDTC", "VISIT"))){

        fail(lacks_msg(TU, c("USUBJID", "TUDTC", "VISIT")))

    } else{

        ### Find unique pairs of TUDTC and VISIT per USUBJID

        if(TU %lacks_any% "TUEVAL"){
        tusub = TU %>%
            select(USUBJID, TUDTC, VISIT)
        }else{
        tusub = TU %>%
            filter(toupper(TUEVAL) == "INVESTIGATOR" | is_sas_na(TUEVAL)) %>%
            select(USUBJID, TUDTC, VISIT)
        }


        mypairs = unique(tusub)
        mypairs$x = 1

        ### Get counts of visit values per date for each subject
        mydf0 = aggregate(mypairs$x,by=list(USUBJID=mypairs$USUBJID, TUDTC=mypairs$TUDTC),FUN=sum)

        ### Subset where count is >1 and output
        mydf0 = mydf0 %>%
            filter(x>1) %>%
            select(USUBJID, TUDTC) 

        mypairs0 = mypairs %>%
            select(USUBJID, TUDTC,VISIT)

        mydf = merge(mydf0,mypairs0,by=c('USUBJID','TUDTC'),all.x = TRUE)
        rownames(mydf)=NULL

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
