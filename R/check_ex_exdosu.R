#' @title Check for missing EXDOSU records
#'
#' @description This check looks for missing EXODOSU values for valid doses
#'
#' @param EX Exposure SDTM dataset with variables USUBJID,EXTRT,EXSTDTC,EXDOSU
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @importFrom dplyr %>% select filter matches
#'
#' @export
#'
#' @author Jen Chen
#'
#' @examples
#'
#' EX <- data.frame(
#'  USUBJID = 1:10,
#'  EXTRT = 1:10,
#'  EXSTDTC = 1:10,
#'  EXDOSE = 1:10,
#'  EXOCCUR = as.character(c(rep("Y",5),rep("N",5))),
#'  EXDOSU = as.character(rep("mg",10))
#' )
#'
#' EX$EXDOSU[1] = ""
#' EX$EXDOSU[2] = "NA"
#' EX$EXDOSU[3] = NA
#'
#' check_ex_exdosu(EX)
#'
#' EX$EXSTDTC = NULL
#'
#' check_ex_exdosu(EX)
#'


check_ex_exdosu <- function(EX){

  ###First bifurcate EX into a df based on occurrence of EXOCCUR
  if ("EXOCCUR" %in% names(EX)) {
    df <- EX %>% filter(EXOCCUR == "Y")
  }
  else {
    df <- EX
  }

  ###Check that required variables exist and return a message if they don't
  if(EX %lacks_any% c("USUBJID","EXTRT","EXSTDTC","EXDOSE","EXDOSU"))
  {
    fail(lacks_msg(EX, c("USUBJID", "EXTRT", "EXSTDTC","EXDOSE","EXDOSU")))
  }
  else
  {
    ### Subset EX to only records with missing EXDOSU
    df <- df %>%
      # select all variables matching regular expressions.
      select(matches(match = "USUBJID$|EXTRT$|EXSTDTC$|EXSTDTC$|EXDOSE$|EXDOSU$")) %>%
      # order
      select(USUBJID, EXTRT, EXDOSE, EXSTDTC, EXDOSE, EXDOSU) %>%
      # filter missing EXDOSU obs
      filter(is_sas_na(EXDOSU))

    rownames(df)=NULL

    ###Print to report

    ### Return message if no records in df
    if(nrow(df)==0)
    {
      pass()

      ### Return subset dataframe if there are records with missing EXDOSU
    }
    else if(nrow(df)>0)
    {
      fail(
        msg = paste0("There are ",nrow(df)," records with missing dose units. "), data = df)
    }
  }
}
