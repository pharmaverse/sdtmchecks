#' @title Check LBORRES/LBSTRESC populated with number beginning with
#' character '>' or '<', which will yield missing AVAL in ADaM and records will be omitted in analyses such as Hy's Law
#'
#' @description This check looks for missing numeric standardized finding (LBSTRESN)
#'              when original finding (LBORRES) and character standardized finding (LBSTRESC) are not missing
#'              and LBORRES/LBSTRESC populated with number beginning with character '>' or '<'
#'
#' @param LB Lab SDTM dataset with variables USUBJID, LBTEST, LBDTC, LBORRES, LBORRESU, LBSTRESN, LBSTRESC
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author Vira Vrakina
#'
#' @examples
#'
#'  LB <- data.frame(
#'  USUBJID = c("Patient 1","Patient 2","Patient 3"),
#'  LBTEST   = "Test A",
#'  LBDTC   = "2017-01-01",
#'  LBORRES = c("5","3","7"),
#'  LBORRESU = rep("mg",3),
#'  LBSTRESC  = c("5","3","7"),
#'  LBSTRESN  = c(5,3,7),
#'  stringsAsFactors = FALSE
#' )
#'
#' check_lb_lbstresc_char(LB)
#'
#'  LB <- data.frame(
#'  USUBJID = c("Patient 1","Patient 2","Patient 3"),
#'  LBTEST   = rep("Test A", 3),
#'  LBDTC   = "2017-01-01",
#'  LBORRES = c("5","3","<7"),
#'  LBORRESU = rep("mg",3),
#'  LBSTRESC  = c("5","3","<7"),
#'  LBSTRESN  = c(5,3,NA),
#'  stringsAsFactors = FALSE
#' )
#'
#' check_lb_lbstresc_char(LB)
#'
#' LB <- data.frame(
#'  USUBJID = c("Patient 1","Patient 2","Patient 3"),
#'  LBTEST   = rep("Test A", 3),
#'  LBDTC   = rep("2017-01-01", 3),
#'  LBORRES = c("5","BLQ","<7"),
#'  LBORRESU = rep("mg",3),
#'  LBSTRESC  = c("5","BLQ","<7"),
#'  LBSTRESN  = c(5,NA,NA),
#'  stringsAsFactors = FALSE
#' )
#'
#' check_lb_lbstresc_char(LB)
#'
#'

check_lb_lbstresc_char <- function(LB){

    if(LB %lacks_any% c("USUBJID", "LBTEST", "LBDTC", "LBORRES", "LBORRESU", "LBSTRESN", "LBSTRESC")){

        fail(lacks_msg(LB, c("USUBJID", "LBTEST", "LBDTC", "LBORRES", "LBORRESU", "LBSTRESN", "LBSTRESC")))

    }else{

        #subset data to contain relevant column variables
        vars <- c("USUBJID", "LBTEST", "LBDTC", "LBORRES", "LBORRESU", "LBSTRESN", "LBSTRESC")

        # Subset to LBORRES populated but LBSTRESN not
        mydf <- subset(LB, !is_sas_na(LB$LBORRES) &
                           !is_sas_na(LB$LBSTRESC) &
                           is_sas_na(LB$LBSTRESN) &
                           grepl("[><]{1}[0-9]", LB$LBSTRESC),
                       vars)

        if (nrow(mydf)==0){
            pass()
        }
        else if (nrow(mydf)>0) {
            fail(paste("LBSTRESN missing but LBORRES/LBSTRESC populated with number beginning with character > or < for ", nrow(mydf)," record(s). ", sep=""), mydf)
        }
    }
}
