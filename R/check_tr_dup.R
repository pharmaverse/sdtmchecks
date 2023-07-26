#' @title Check for duplicate TR records
#'
#' @description This check looks for duplicate TR records and returns a data frame.
#'   Only applies to assessments by investigator.
#'
#' @param TR TR dataframe with variables USUBJID, TRLINKID, TRTESTCD, TRCAT, TRSTRESC,
#'           TRSTRESU, VISITDY, TRDTC
#'
#' @author Joel Laxamana
#'
#' @importFrom dplyr %>% select filter arrange_at
#' @importFrom tidyselect any_of
#'
#' @export
#'
#' @examples
#'
#'  TR <- data.frame(
#'  USUBJID  = c(1,1,2,2),
#'  TRCAT    = c(1,1,2,2),
#'  TRTESTCD = c(1,1,2,2),
#'  TRLINKID = c(1,1,2,2),
#'  TRSPID   = c(1,1,2,2),
#'  TRDTC    = c(1,1,2,2),
#'  TRSTRESC = c(1,1,2,2)
#' )
#'
#' check_tr_dup(TR)
#'
#'

check_tr_dup <- function(TR){
    if (TR %lacks_any% c("USUBJID","TRCAT","TRTESTCD","TRDTC","TRSTRESC")){

        fail (lacks_msg(TR, c("USUBJID","TRCAT","TRTESTCD","TRDTC","TRSTRESC")))

    } else if (TR %lacks_all% c("TRLINKID","TRLNKID")) {

        fail("TR is missing both the TRLINKID and TRLNKID variable. ")

    } else{

        myvars <- c("USUBJID","TRCAT","TRTESTCD",names(TR)[names(TR) %in% c("TRLINKID","TRLNKID")],names(TR)[names(TR) %in% "TRSPID"],"TRDTC","TRSTRESC")


        if(TR %lacks_any% "TREVAL"){

        # leave only variabls on which we want to check duplicate TR records
        tr1 <- TR %>%
            arrange_at(myvars) %>%
            select(any_of(myvars))
        }else{
            tr1 <- TR %>%
                arrange_at(myvars) %>%
                filter(toupper(TREVAL) == "INVESTIGATOR" | is_sas_na(TREVAL)) %>%
                select(any_of(myvars))
        }

        # check if there are duplicate TR records
        dups <- subset(tr1,duplicated(tr1),myvars)
        rownames(dups)=NULL

        # declare number of duplicated AEs and print them
        n0 <- paste('There are ', nrow(dups), ' duplicated TR records. ', sep ='')

        if (nrow(dups) == 0){
            pass()
        } else{
            fail(
                paste(n0), data=dups
            )
        }
    }
}
