#' @title Check for Missing EXDOSE.
#'
#' @description This checks looks for missing EXDOSE values when EXOCCUR="Y"
#'   or when EXOCCUR does not exist.
#'   It could be for a specified drug/treatment, or for all drugs/treatments
#'   in the dataset
#'
#' @param EX Exposure SDTM dataset with variables USUBJID, EXTRT, EXSTDTC,
#'    EXDOSE, and optional variable EXOCCUR and optional variable VISIT
#' @param drug Drug name for EXTRT; used to subset the dataset.
#'   Default value is NULL (i.e. no filtering by drug)
#'
#' @return Boolean value for whether the check passed or failed, with 'msg'
#'   attribute if the test failed
#'
#' @export
#'
#' @importFrom  dplyr %>% filter select
#'
#' @author Will Harris, Pasha Foroudi
#'
#' @examples
#'
#' EX <- data.frame(
#'  USUBJID = 1:3,
#'  EXSEQ   = 1:3,
#'  EXSTDTC = 1:3,
#'  EXTRT   = c(1,2,NA),
#'  EXOCCUR = "Y",
#'  EXDOSE  = 1:3,
#'  VISIT = c("CYCLE 1 DAY 1", "CYCLE 2 DAY 1", "CYCLE 3 DAY 1")
#' )
#'
#' check_ex_exdose_exoccur(EX)
#'
#' EX$EXDOSE[3]=NA
#' check_ex_exdose_exoccur(EX)
#'
#' EX$EXVISIT = NULL
#' check_ex_exdose_exoccur(EX)
#'
#' EX$EXDOSE = NULL
#' check_ex_exdose_exoccur(EX)
#'
#'

check_ex_exdose_exoccur <- function(EX, drug=NULL) {

    if (EX %lacks_any% c("USUBJID","EXTRT","EXSTDTC","EXDOSE")) {
      fail(lacks_msg(EX, c("USUBJID","EXTRT","EXSTDTC","EXDOSE")))
      # Checks validity of drug name argument
    }else{

    if (!is.null(drug) && !(drug %in% EX[["EXTRT"]])) {
      fail(msg = "Drug name not found in dataset")
    }else if (EX %lacks_all% c("EXOCCUR", "VISIT")){
      #For DFs *without* EXOCCUR and VISIT variables
      # Subsets EX to rows where EXDOSE is missing
      df <- EX %>%
        select("USUBJID", "EXTRT", "EXSTDTC", "EXDOSE") %>%
        filter(is_sas_na(EX[["EXDOSE"]]))
    }else if (EX %lacks_any% c("EXOCCUR")){
      #For DFs *without* EXOCCUR variable but *with* VISIT variable
      # Subsets EX to rows where EXDOSE is missing
      df <- EX %>%
        select("USUBJID", "EXTRT", "VISIT", "EXSTDTC", "EXDOSE") %>%
        filter(is_sas_na(EX[["EXDOSE"]]))
    }else if (EX %lacks_any% c("VISIT")){
      #For DFs *without* VISIT variable but *with* EXOCCUR variable
      # Subsets EX to rows where EXDOSE is missing
      df <- EX %>%
        select("USUBJID", "EXTRT", "EXSTDTC", "EXOCCUR", "EXDOSE") %>%
        filter(is_sas_na(EX[["EXDOSE"]]))
    }else {
      # For DFs *with* EXOCCUR and VISIT variables
      # Subsets EX to rows where EXOCCUR == "Y" but EXDOSE is missing
      df <- EX %>%
        select("USUBJID", "EXTRT", "VISIT", "EXSTDTC", "EXOCCUR", "EXDOSE") %>%
        filter(EXOCCUR == "Y" & is_sas_na(EX[["EXDOSE"]]))
    }

  if (!is.null(drug)) {
    df <- df %>% filter(EXTRT == drug)
  }
  # Outputs a resulting message depending on whether there are instances with
  # EXOCCUR == "Y" and EXDOSE is missing, and whether a drug name is supplied
  if (nrow(df) != 0 && !is.null(drug)) {
    fail(paste0("EX has ", nrow(df), " record(s) with missing EXDOSE when EXOCCUR = 'Y' (or EXOCCUR does not exist) for ", drug,". "), df)
  } else if (nrow(df) != 0) {
    fail(paste0("EX has ", nrow(df), " record(s) with missing EXDOSE when EXOCCUR = 'Y' (or EXOCCUR does not exist). "), df)
  } else {
    pass()
  }
    }
}
