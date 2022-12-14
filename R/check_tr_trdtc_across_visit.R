#' @title Check TR Longest Diameter Records where the same date occurs accross multiple visits
#'
#' @description This check identifies records where the same date TRDTC occurs
#'  across multiple visits when TRTESTCD is "LDIAM". Only applies to assessments by investigator.
#'
#' @param TR Tumor Result SDTM dataset with variables USUBJID, TRDTC, VISIT, TRTESTCD
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
#' TR <- data.frame(
#' USUBJID = 1,
#' TRDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
#' VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
#' TRTESTCD = c(rep("LDIAM",7),rep("SAXIS",3)),
#' stringsAsFactors=FALSE)
#'
#' check_tr_trdtc_across_visit(TR)
#'
#'

check_tr_trdtc_across_visit <- function(TR) {

    ###First check that required variables exist and return a message if they don't
    if((TR %lacks_any% c("USUBJID", "TRDTC", "VISIT", "TRTESTCD"))){

        fail(lacks_msg(TR, c("USUBJID", "TRDTC", "VISIT", "TRTESTCD")))

    } else{

        ### Find unique pairs of TRDTC and VISIT per USUBJID

        if(TR %lacks_any% "TREVAL"){
        trsub = TR %>%
            filter(TRTESTCD == "LDIAM") %>%
            select(USUBJID, TRDTC, VISIT, TRTESTCD)
        }else{
        trsub = TR %>%
            filter(TRTESTCD == "LDIAM" & (toupper(TREVAL) == "INVESTIGATOR" | is_sas_na(TREVAL))) %>%
            select(USUBJID, TRDTC, VISIT, TRTESTCD)
        }

        if(nrow(trsub)>0){
            mypairs = unique(trsub)
            mypairs$x = 1

            ### Get counts of visit values per date for each subject
            mydf0 = aggregate(mypairs$x,by=list(USUBJID=mypairs$USUBJID, TRDTC=mypairs$TRDTC),FUN=sum)

            ### Subset where count is >1 and output
            mydf0 = mydf0 %>%
                select("USUBJID", "TRDTC") %>%
                filter(mydf0$x>1)

            mypairs0 = mypairs %>%
                select("USUBJID", "TRDTC", "VISIT", "TRTESTCD")

            mydf = merge(mydf0,mypairs0,by=c("USUBJID", "TRDTC"),all.x = TRUE)
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
