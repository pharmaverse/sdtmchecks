#' @title Check RS Records where the same date occurs across multiple visits
#'
#' @description This check identifies records where the same date RSDTC occurs
#'  across multiple visits. Only applies to assessments by investigator.
#'
#' @param RS Disease Response SDTM dataset with variables USUBJID, RSDTC, VISIT
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#' @importFrom stats aggregate
#'
#' @author Will Harris
#'
#' @examples
#'
#' RS <- data.frame(
#' USUBJID = 1,
#' RSDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
#' VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
#' stringsAsFactors=FALSE)
#'
#' check_rs_rsdtc_across_visit(RS)
#'
#'
#'

check_rs_rsdtc_across_visit <- function(RS) {

    ###First check that required variables exist and return a message if they don't
    if((RS %lacks_any% c("USUBJID", "RSDTC", "VISIT"))){

        fail(lacks_msg(RS, c("USUBJID", "RSDTC", "VISIT")))

    } else{

        ### Find unique pairs of RSDTC and VISIT per USUBJID
        if(RS %lacks_any% "RSEVAL"){
        rssub = RS %>%
            select(USUBJID, RSDTC,VISIT)
        }else{
        rssub = RS %>%
                select(USUBJID, RSDTC,VISIT) %>%
                filter(toupper(RSEVAL) == "INVESTIGATOR" | is_sas_na(RSEVAL))
        }

        if(nrow(rssub)>0){
            mypairs = unique(rssub)
            mypairs$x = 1

            ### Get counts of visit values per date for each subject
            mydf0 = aggregate(mypairs$x,by=list(USUBJID=mypairs$USUBJID, RSDTC=mypairs$RSDTC),FUN=sum)

            ### Subset where count is >1 and output
            mydf0 = mydf0 %>%
                select('USUBJID', 'RSDTC') %>%
                filter(mydf0$x>1)

            mypairs0 = mypairs %>%
                select('USUBJID', 'RSDTC','VISIT')

            mydf = merge(mydf0,mypairs0,by=c('USUBJID','RSDTC'),all.x = TRUE)
            rownames(mydf)=NULL
        }else{
            mydf=data.frame()
        }

        ### if no consistency
        if (nrow(mydf)==0) {
            pass()
        } else if (nrow(mydf) > 0) {
            ### Return subset dataframe if there are records with inconsistency
            fail(paste(nrow(mydf),"records with same date at >1 visit. "),
                 mydf)
        }
    }
}
