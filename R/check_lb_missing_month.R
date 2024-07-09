#' @title Check for lab dates with year and day known but month unknown
#'
#' @description Check for missing month when lab specimen collection date (LBDTC)
#' has known year and day
#'
#' @param LB Laboratory data SDTM dataset with variables USUBJID,LBTEST,LBDTC,VISIT
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#' 
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#'
#' @author Sara Bodach
#'
#' @examples
#'
#' LB <- data.frame(
#'  USUBJID = 1:4,
#'  LBTEST = c("TEST1","TEST2","TEST3","TEST3"),
#'  LBDTC = c("2017-01-01","2017-02-01","2017---01", "2017----01"),
#'  VISIT = c("VISIT1","VISIT2","VISIT3","VISIT3"),
#'  stringsAsFactors=FALSE
#' )
#'
#' check_lb_missing_month(LB)
#'
#' LB$LBSPID= "FORMNAME-R:2/L:2XXXX"
#' 
#' check_lb_missing_month(LB,preproc=roche_derive_rave_row)
#' 
#' LB$LBDTC = NULL
#'
#' check_lb_missing_month(LB)

check_lb_missing_month <- function(LB, preproc=identity,...){

    ###First check that required variables exist and return a message if they don't
    if(LB %lacks_any% c("USUBJID", "LBTEST", "LBDTC","VISIT")){

        fail(lacks_msg(LB, c("USUBJID", "LBTEST", "LBDTC","VISIT")))

    } else{

        #Apply company specific preprocessing function
        LB = preproc(LB,...)
        
        # check if LBDTC has missing month and is in format 'yyyy---dd'
        mydf <- LB %>%
                   select(any_of(c("USUBJID", "LBTEST", "LBDTC","VISIT","RAVE")))%>%
                   filter(missing_month(LBDTC))
        rownames(mydf)=NULL

        ### Return message if there are lab dates with only missing month
        if(nrow(mydf)==0){
            pass()

        }else if(nrow(mydf)>0){

            fail(paste0("There are ",length(unique(mydf$USUBJID)),
                       " patients with a lab date that has year and day present but missing month. "), mydf)

        }
    }
}
