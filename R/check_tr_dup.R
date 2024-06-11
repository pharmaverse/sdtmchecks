#' @title Check for duplicate TR records
#'
#' @description This check looks for duplicate TR records and returns a data frame.
#'   Only applies to assessments by Investigator, selected based on uppercased
#'   TREVAL = "INVESTIGATOR" or missing or TREVAL variable does not exist.
#'
#' @param TR dataframe with variables USUBJID, TRCAT, TRLINKID/TRLNKID, TRTESTCD, TRSTRESC,
#'           TRDTC, TRSPID (if it exists)
#'
#' @author Joel Laxamana
#'
#' @importFrom dplyr %>% select filter group_by_all n
#' @importFrom tidyselect any_of
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @examples
#'
#' # example with an error
#' TR <- data.frame(
#'  USUBJID  = c(1,1,2,2),
#'  TRCAT    = c(1,1,2,2),
#'  TRTESTCD = c(1,1,2,2),
#'  TRLINKID = c(1,1,2,2),
#'  TRDTC = c(rep("2016-01-01",2), rep("2016-06-01",2)),
#'  TRSTRESC = c(1,1,2,2),
#'  TRSPID = "FORMNAME-R:19/L:19XXXX",
#'  TREVAL = "INVESTIGATOR",
#'  stringsAsFactors = FALSE
#' )
#'
#' check_tr_dup(TR)
#'
#' TR1 <- TR
#' TR1$TRSPID <- NULL
#'
#' check_tr_dup(TR1)
#'
#' TR2 <- TR
#' TR2$TREVAL <- NULL
#'
#' check_tr_dup(TR2)
#'
#' # example with no records flagged because issues only among IRF records
#' TR3 <- TR
#' TR3$TREVAL <- "INDEPENDENT ASSESSOR"
#' check_tr_dup(TR3)
#'
#' # example with required variable missing
#' TR4 <- TR
#' TR4$TRLINKID <- NULL
#' check_tr_dup(TR4)
#'
#'


check_tr_dup <- function(TR){

    if (TR %lacks_any% c("USUBJID","TRCAT","TRTESTCD","TRDTC","TRSTRESC")){

        fail (lacks_msg(TR, c("USUBJID","TRCAT","TRTESTCD","TRDTC","TRSTRESC")))

    } else if (TR %lacks_all% c("TRLINKID", "TRLNKID")) {

        fail("TR is missing both the TRLINKID and TRLNKID variables. ")

    } else{

        myvars <- c("USUBJID","TRCAT","TRTESTCD",names(TR)[names(TR) %in% c("TRLINKID","TRLNKID")],names(TR)[names(TR) %in% "TRSPID"],"TRDTC","TRSTRESC")

        if(TR %lacks_any% "TREVAL"){

            # for TR domains without TREVAL

            # leave only variables on which we want to check duplicate TR records
            # Subsets to duplicated entries only
            tr1 <- TR %>%
                select(any_of(myvars)) %>%
                group_by_all() %>%
                filter(n()>1)

        }else{

            # for TR domains with TREVAL
            # Subsets to duplicated entries only
            tr1 <- TR %>%
                filter(toupper(TREVAL) == "INVESTIGATOR" | is_sas_na(TREVAL)) %>%
                select(any_of(myvars)) %>%
                group_by_all() %>%
                filter(n()>1)
        }

        # duplicate TR records
        dups <- subset(tr1, duplicated(tr1), myvars)
        rownames(dups)=NULL

        # declare number of duplicated TR records and print them
        n0 <- paste('There are ', nrow(dups), ' duplicated TR records. ', sep ='')

        if (nrow(dups) == 0){
            pass()
        } else{
            fail(paste(n0), data=dups)
        }
    }
}
