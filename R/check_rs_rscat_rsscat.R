#' @title Check for patients with populated RSSCAT but missing RSCAT.
#'
#' @description Check for patients with populated RSSCAT but missing RSCAT in RS domain to
#' help flag a potential mapping issue for SPA; this does not warrant a query in Rave.
#'
#' @param RS Response SDTM dataset with variables USUBJID, RSCAT and RSSCAT.
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author Saibah Chohan, Ashley Mao, Tina Cho (HackR 2021 Team STA-R)
#'
#' @examples
#'
#' RS <- data.frame(
#'  USUBJID = c("id1", "id1", "id2", "id2", "id3"),
#'  RSCAT = c("A", "A", "B", NA, NA),
#'  RSSCAT = c("AA", "AA", "BB", "BB","AA"))
#' check_rs_rscat_rsscat(RS)
#'
#' # Test with missing RSCAT
#' RS$RSCAT = NULL
#' check_rs_rscat_rsscat(RS)
#'


check_rs_rscat_rsscat <- function(RS){

    ###First check that required variables exist and return a message if they don't
    if(RS %lacks_any% c("USUBJID", "RSCAT", "RSSCAT")){

        fail(lacks_msg(RS, c("USUBJID", "RSCAT", "RSSCAT")))

    }else{

        ### Subset RS to only patient(s) with non-missing RSCAT and missing RSCAT
        mydf = subset(RS, is_sas_na(RS$RSCAT) & !is_sas_na(RS$RSSCAT), c("USUBJID", "RSCAT", "RSSCAT"))

        ### Return message if no patient(s) with missing RSCAT
        if(nrow(mydf)==0){
            pass()

            ### Return subset dataframe if there are patient(s) with missing AGE, AGE<18 or AGE>90
        }else if(nrow(mydf)>0){

            fail(paste("There are ",length(unique(mydf$USUBJID)),
                       " patients with unpopulated RSCAT values. ",sep=""),
                 mydf)
        }
    }
}
