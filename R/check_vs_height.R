#' @title Check for missing height values
#'
#' @description This check looks for both records where height is missing
#' as well as DM patients with no height records at all
#'
#' @param VS Vital Signs SDTM dataset with variables USUBJID,VSTEST,VSTESTCD,VSSTRESN,VISIT
#' @param DM Demographics SDTM dataset with variable USUBJID
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @importFrom dplyr anti_join %>% filter select
#'
#' @export
#'
#' @author Sara Bodach
#'
#' @examples
#'
#' DM <- data.frame(
#'  STUDYID = 1,
#'  USUBJID = 1:10
#' )
#'
#' VS <- data.frame(
#'  STUDYID = 1,
#'  USUBJID = 1:10,
#'  VSTEST = "HEIGHT",
#'  VSTESTCD = "HEIGHT",
#'  VSSTRESN = 1:10,
#'  VISIT = 1:10
#' )
#' 
#' check_vs_height(VS,DM)
#' 
#' DM <- data.frame(
#'  STUDYID = 1,
#'  USUBJID = 1:11
#' )
#'
#' VS$VSSTRESN[1] = NA
#' VS$VSSTRESN[2] = "NA"
#' VS$VSSTRESN[3] = ""
#' VS$VSSTRESN[4] = "."
#'
#' check_vs_height(VS,DM)
#'
#'


check_vs_height <- function(VS,DM){

  if(VS %lacks_any% c("USUBJID", "VSTEST", "VSSTRESN")){

    fail(lacks_msg(VS, c("USUBJID", "VSTEST", "VSSTRESN")))

  }else if(DM %lacks_any% c("USUBJID")){

    fail(lacks_msg(DM,c("USUBJID")))

  }else{

    ### Obtain dataset containing all patients in VS with a valid height
    vs_pats <- VS %>%
      filter(toupper(VSTEST) == "HEIGHT") %>%
      filter(!is_sas_na(VSSTRESN)) %>%
      select(USUBJID)

    ### Obtain list of patients in DM
    dm_pats <- DM %>%
      select(USUBJID)

    ### Obtain patients in dm_pats who don't appear in vs_pats
    mydf <- dm_pats %>%
      anti_join(vs_pats, by = "USUBJID")

    ### Print to report

    ### Return message if no records with missing HEIGHT
    if(nrow(mydf)==0){
      pass()

      ### Return subset dataframe if there are records with missing HEIGHT
    }else{
      fail(
        paste0(nrow(mydf)," patient(s) in DM with no recorded height at any visit. "),
        mydf)
    }
  }
}
