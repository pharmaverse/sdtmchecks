#' @title Check that DBP is not higher than SBP in VS
#'
#' @description This check looks for non-missing diastolic BP is not higher than non-missing systolic BP
#' @param VS Vital Signs SDTM dataset with variables USUBJID,VISIT,VSDTC,VSTESTCD,VSSTRESN,VSSPID
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#' @export
#'
#' @importFrom dplyr %>% filter select rename
#'
#' @examples
#'
#' vs <- data.frame(
#'  STUDYID = 1,
#'  USUBJID = 1,
#'  VSSPID = c("1","2","1","2"),
#'  VISIT = 1,
#'  VSDTC = c("2010-01-01","2010-01-01","2010-01-01","2010-01-01"),
#'  VSTESTCD = c("SYSBP","SYSBP",
#'             "DIABP","DIABP")
#'             ,
#'  VSSTRESN = c(80,120,100,80)
#'  )
#'
#'  vs0 <- subset(vs, select = c(USUBJID, VSSPID, VSSTRESN))
#'
#'  check_vs_sbp_lt_dbp(VS=vs)
#'  check_vs_sbp_lt_dbp(VS=vs0)
#'
#'


check_vs_sbp_lt_dbp <- function(VS){

  ###First check that required variables exist and return a message if they don't
  if (VS %lacks_any% c("USUBJID", "VISIT", "VSDTC", "VSTESTCD", "VSSTRESN", "VSSPID" )) {

    fail(lacks_msg(VS, c("USUBJID", "VISIT", "VSDTC", "VSTESTCD", "VSSTRESN", "VSSPID" )))

  } else{

    ### select records for blood pressure
    ### ignore VSPOS for now but revisit
    ### VSTPT consideration

    vs0 <- VS %>%
            select(USUBJID, VISIT, VSDTC, VSTESTCD, VSSTRESN, VSSPID)

    sbp <- vs0 %>%
      filter(VSTESTCD == "SYSBP") %>%
      rename(VSDTC.SYSBP = VSDTC, SYSBP = VSSTRESN)

    dbp <- vs0 %>%
      filter(VSTESTCD == "DIABP") %>%
      rename(VSDTC.DIABP = VSDTC, DIABP = VSSTRESN)

    mydf0 = merge(sbp, dbp, by = c("USUBJID", "VISIT", "VSSPID"))

    mydf <- mydf0 %>%
      filter(mydf0$DIABP > SYSBP & SYSBP > 0 & DIABP > 0) %>%
      select(USUBJID, VISIT, VSDTC.SYSBP, SYSBP, DIABP) %>%
      rename(VSDTC = VSDTC.SYSBP)

    ######### If there are no VS records that qualify for the check ###########
    if(nrow(mydf)==0){

      pass()

      ######### If there are VS records that qualify ###########
    }else if(nrow(mydf)>0){
      fail(paste("VS has ", nrow(mydf), " records with Systolic BP < Diastolic BP. ", sep=""), mydf)
    }
  }
}

