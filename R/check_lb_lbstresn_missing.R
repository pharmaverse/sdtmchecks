#' @title Check missing standard lab values (LBSTRESN/LBSTRESC)
#'
#' @description This check looks for missing standardized finding (LBSTRESN/LBSTRESC)
#'              when original finding (LBORRES) is not missing
#'
#' @param LB Lab SDTM dataset with variables USUBJID, LBTESTCD, LBDTC, LBORRES,
#' LBORRESU, LBSTRESN, LBSTRESC, VISIT (optional), LBSPID (optional)
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom utils head
#'
#' @author Madeleine Ma
#'
#' @examples
#'
#' LB <- data.frame(
#'  USUBJID = c("Patient 1","Patient 2","Patient 3"),
#'  LBTEST   = "Test A",
#'  LBTESTCD   = "TA",
#'  LBDTC   = "2017-01-01",
#'  LBORRES = c("5","6","7"),
#'  LBSTRESC = c("5","6","7"),
#'  LBORRESU = rep("mg",3),
#'  LBSTRESN  = c(5,6,NA),
#'  stringsAsFactors=FALSE
#'  )
#'
#' check_lb_lbstresn_missing(LB)
#'
#' LB$LBSTRESC[3] = ""
#' check_lb_lbstresn_missing(LB)
#'
#' LB$LBSTRESC[1] = ""
#' check_lb_lbstresn_missing(LB)
#'
#' LB$VISIT = "SCREENING"
#' check_lb_lbstresn_missing(LB)
#'
#' LB$LBSPID= "FORMNAME-R:2/L:2XXXX"
#' check_lb_lbstresn_missing(LB,preproc=roche_derive_rave_row)
#'
#' LB$LBSTRESN = NULL
#' check_lb_lbstresn_missing(LB)
#'
#' LB$LBSTRESC = NULL
#' check_lb_lbstresn_missing(LB)
#'

check_lb_lbstresn_missing <- function(LB,preproc=identity,...){

  if(LB %lacks_any% c("USUBJID", "LBTESTCD", "LBDTC", "LBORRES", "LBORRESU", "LBSTRESN", "LBSTRESC")){

    fail(lacks_msg(LB, c("USUBJID", "LBTESTCD", "LBDTC", "LBORRES", "LBORRESU", "LBSTRESN", "LBSTRESC")))

  }else{

    #Apply company specific preprocessing function
    LB = preproc(LB,...)

    # Subset LB to fewer variables
    LB <- LB %>%
      select(any_of(c("USUBJID", "LBTESTCD", "LBDTC", "LBORRES", "LBORRESU", "LBSTRESN", "LBSTRESC","RAVE","VISIT")))

    # Subset to LBORRES populated but LBSTRESN not
    mydf <- subset(LB, !is_sas_na(LB$LBORRES) & is_sas_na(LB$LBSTRESN) & is_sas_na(LB$LBSTRESC))

    if (nrow(mydf)==0){
      pass()
    }
    else if (nrow(mydf)>0) {
        fail(paste0(length(unique(mydf$USUBJID)),
                    " unique patient(s) with ",
                    nrow(mydf),
                    " lab record(s) with result reported without standard value. "),
             mydf)
    }
  }
}
