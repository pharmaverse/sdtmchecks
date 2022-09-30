#' @title Check for missing lab reference ranges (LBSTNRLO, LBSTNRHI)
#'
#' @description This check looks for missing lab reference ranges (LBSTNRLO,
#' LBSTNRHI) in standard units when numeric result in standard unit (LBSTRESN)
#' is not missing and returns a data frame
#'
#' @param LB Lab SDTM dataset with variables USUBJID, LBTEST, LBSTRESN, LBSTNRLO, LBSTNRHI
#' @param DM DM SDTM dataset with variable USUBJID, SITEID
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter rename select
#'
#' @author Lei Zhao
#'
#' @examples
#'
#' LB <- data.frame(
#' USUBJID = "1",
#' LBTEST = "Albumin",
#' LBSTRESN = 1:10,
#' LBSTNRLO = 1:10,
#' LBSTNRHI = 1:10,
#' stringsAsFactors=FALSE
#' )
#'
#' LB$LBSTNRLO[1]=""
#' LB$LBSTNRLO[2]="NA"
#' LB$LBSTNRLO[3]=NA
#' LB$LBSTNRHI[3]=""
#' LB$LBSTNRHI[4]="NA"
#' LB$LBSTNRHI[5]=NA
#'
#' DM <- data.frame(
#' USUBJID = "1",
#' SITEID = "123456",
#' stringsAsFactors=FALSE
#' )
#'
#' check_lb_lbstnrlo_lbstnrhi(DM, LB)
#'

check_lb_lbstnrlo_lbstnrhi <- function(DM, LB){

    ###First check that required variables exist and return a message if they don't
    if(LB %lacks_any% c("USUBJID", "LBTEST", "LBSTRESN", "LBSTNRLO", "LBSTNRHI")) {

        fail(lacks_msg(LB, c("USUBJID", "LBTEST", "LBSTRESN", "LBSTNRLO", "LBSTNRHI")))

    } else if(DM %lacks_any% c("USUBJID", "SITEID")) {

        fail(lacks_msg(DM, c("USUBJID", "SITEID")))

    } else {

        ### Subset LB to only records with missing reference range (missing LBSTNRLO
        ### or LBSTNRHI) when numeric result in std unit (LBSTRESN) is not missing
        mydf0 <- LB %>%
            select(USUBJID, LBTEST, LBSTRESN, LBSTNRLO, LBSTNRHI)

        mydf1 <- mydf0 %>%
                filter(!is_sas_na(LBSTRESN) &
                           (is_sas_na(LBSTNRLO) |
                                is_sas_na(LBSTNRHI)))

        if (nrow(mydf1)==0) {
            pass()
        } else {

            #merge to get SITEID
            site <- DM %>%
                select(USUBJID, SITEID)

            mydf2 = merge(mydf1, site,
                          by = c("USUBJID"),
                          all.x = T)

            mydf <- as.data.frame(table(mydf2$SITEID, mydf2$LBTEST)) %>%
                rename(SITEID=Var1, LBTEST=Var2) %>%
                filter(Freq != 0)

            mydf <- with(mydf, mydf[order(SITEID, LBTEST, Freq),])
            rownames(mydf)=NULL

            fail(paste0("Lab tests with missing reference range in standard units when standard numeric result is not missing: ",
                            length(unique(mydf$LBTEST)), " LBTEST(s) across ",
                        length(unique(mydf$SITEID)), " unique SITEID(s). "),
                 mydf)
        }
    }
}
