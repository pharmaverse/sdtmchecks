#' @title Check if, whenever a patient experiences an AE leading to study discontinuation,
#' they also have a DS record indicating this.
#'
#' @description This code checks that when a patient has an AE with AEACNOTx = "SUBJECT
#' DISCONTINUED FROM STUDY" (x = "H", "1", "2", ...) then there should also be a record in DS
#' where DS.DSSCAT = "STUDY COMPLETION/EARLY DISCONTINUATION" and DS.DSDECOD != "COMPLETED".
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AEDECOD, AEACNOTx
#' @param DS Disposition SDTM dataset with variables USUBJID, DSCAT, DSSCAT, DSDECOD
#'
#' @return boolean value if check returns 0 obs, otherwise return subset dataframe.
#'
#' @export
#'
#' @author Edoardo Mancini
#'
#' @examples
#'
#' # test with sample data
#'
#' AE <- data.frame(
#'     STUDYID  = "1001",
#'     USUBJID  = c("1","2","3","4","5","1"),
#'     AESTDTC  = rep('2020-05-05', 6),
#'     AEDECOD  = c("HEADACHE", "HEART ATTACK","CHILLS", "PNEUMONIA", "ARTHRITIS", "FATIGUE"),
#'     AEACNOTH = c("NONE", "SUBJECT DISCONTINUED FROM STUDY", "MULTIPLE", "NONE",
#'                  "SUBJECT DISCONTINUED FROM STUDY", "SUBJECT DISCONTINUED FROM STUDY"),
#'     AEACNOT1 = c("", "", "PROCEDURE/SURGERY", "", "", ""),
#'     AEACNOT2 = c("", "", "SUBJECT DISCONTINUED FROM STUDY", "", "", ""),
#'     stringsAsFactors = FALSE
#' )
# 'DS <- data.frame(
#'     USUBJID = c("1","5"),
#'     DSCAT   = c("DISPOSITION EVENT", "DISPOSITION EVENT"),
#'     DSSCAT  = c("STUDY COMPLETION/EARLY DISCONTINUATION", "STUDY COMPLETION/EARLY DISCONTINUATION"),
#'     DSDECOD = c("ADVERSE EVENT", "ADVERSE EVENT" ),
#'     stringsAsFactors = FALSE
#' )
#'
#' check_ae_aeacnoth_ds_disctx(AE, DS)
#'
#'\dontrun{
#'
#' rice::rice_session_open()
#'
#' AE1 = rice::rice_read("root/clinical_studies/RO5541077/CDPT7898/GO29365/data_analysis/armg_report4/data/sdtmv/ae.sas7bdat", prolong = T)
#' AE2 = rice::rice_read("root/clinical_studies/RO5541267/CDT30019/GO29436/data_analysis/3rd_os/data/sdtmv/ae.sas7bdat", prolong = T)
#' AE3 = rice::rice_read("root/clinical_studies/RO7200394/CDPT7921/BP40923/data_analysis/CSR/data/sdtmv/ae.sas7bdat", prolong = T)
#' AE4 = rice::rice_read("root/clinical_studies/RO5541267/CDPT3852/GO29537/data_analysis/events/data/sdtmv/ae.sas7bdat", prolong = T)
#' AE5 = rice::rice_read("root/clinical_studies/RO0220912/CDT30061/GA29960/data_analysis/final_vads/data/sdtmv/ae.sas7bdat", prolong = T)
#' AE6 = rice::rice_read("root/clinical_studies/RO7200220/CDT70193/BP40899/data_analysis/CSR_P123/data/sdtmv/ae.sas7bdat", prolong = T)
#' AE7 = rice::rice_read("root/clinical_studies/RO5490261/CDPT7437/GA29144/data_analysis/CSR_FINAL/data/sdtmv/ae.sas7bdat")
#'
#' DS1 = rice::rice_read("root/clinical_studies/RO5541077/CDPT7898/GO29365/data_analysis/armg_report4/data/sdtmv/ds.sas7bdat", prolong = T)
#' DS2 = rice::rice_read("root/clinical_studies/RO5541267/CDT30019/GO29436/data_analysis/3rd_os/data/sdtmv/ds.sas7bdat", prolong = T)
#' DS3 = rice::rice_read("root/clinical_studies/RO7200394/CDPT7921/BP40923/data_analysis/CSR/data/sdtmv/ds.sas7bdat", prolong = T)
#' DS4 = rice::rice_read("root/clinical_studies/RO5541267/CDPT3852/GO29537/data_analysis/events/data/sdtmv/ds.sas7bdat", prolong = T)
#' DS5 = rice::rice_read("root/clinical_studies/RO0220912/CDT30061/GA29960/data_analysis/final_vads/data/sdtmv/ds.sas7bdat", prolong = T)
#' DS6 = rice::rice_read("root/clinical_studies/RO7200220/CDT70193/BP40899/data_analysis/CSR_P123/data/sdtmv/ds.sas7bdat", prolong = T)
#' DS7 = rice::rice_read("root/clinical_studies/RO5490261/CDPT7437/GA29144/data_analysis/CSR_FINAL/data/sdtmv/ds.sas7bdat")
#' rice::rice_session_close()
#'
#' check_ae_aeacnoth_ds_disctx(AE1,DS1)
#' check_ae_aeacnoth_ds_disctx(AE2,DS2)
#' check_ae_aeacnoth_ds_disctx(AE3,DS3)
#' check_ae_aeacnoth_ds_disctx(AE4,DS4)
#' check_ae_aeacnoth_ds_disctx(AE5,DS5)
#' check_ae_aeacnoth_ds_disctx(AE6,DS6)
#'
#'}
#'
check_ae_aeacnoth_ds_disctx <- function(AE, DS){
  
  # First check that required variables exist and return a message if they don't
  if( AE %lacks_any% c("USUBJID", "AEDECOD", "AEACNOTH")){
    
    fail(lacks_msg(AE, c("USUBJID", "AEDECOD", "AEACNOTH")))
    
  }else if( DS %lacks_any% c("USUBJID", "DSCAT", "DSSCAT", "DSDECOD")){
    
    fail(lacks_msg(DS, c("USUBJID", "DSCAT"," DSSCAT", "DSDECOD")))
    
  } else{
    
    # Get all AEACNOTx (x = "H", "1", "2", etc) columns
    aeacnotx_cols <- c("AEACNOTH", grep("AEACNOT[0-9]", names(AE), value = TRUE))
    
    # Keep only AE columns that are needed
    ae1 <- AE %>%
      select(c("USUBJID", "AEDECOD", "AESTDTC", aeacnotx_cols))
    
    # Filter for AE records where any of AEACNOTHx (x = "", "1", "2", etc) is "SUBJECT DISCONTINUED FROM STUDY"
    ae1$subj_discont_fl <- apply(ae1[aeacnotx_cols] == "SUBJECT DISCONTINUED FROM STUDY", 1, any)
    
    ae2 <- ae1 %>%
      filter(subj_discont_fl) %>%
      select(-subj_discont_fl)
    
    # Keep only DS columns that are needed
    ds1 <- DS %>% select(c("USUBJID", "DSCAT", "DSSCAT","DSDECOD"))
    
    # Filter for DS records indicating subject didn't complete the study
    ds2 <- ds1 %>% filter(grepl("DISCON", str_to_upper(DSSCAT)) & DSDECOD != "COMPLETED") 
    
    # Merge AE and DS to cross-check records
    ae_ds <- ae2 %>% left_join(ds2, by = c("USUBJID"))
    
    # Keep only AE records where there is no corresponding DS record
    mydf <- ae_ds %>% filter(is.na(DSDECOD))
    rownames(mydf) = NULL
    
    # Return pass message if no there is no inconsistency between AE and DS
    if(nrow(mydf) == 0){
      pass()
      
      # Return subset dataframe if there are records with inconsistencies
    }else if(nrow(mydf)>0){
      
      return(fail(paste(length(unique(mydf$USUBJID)),
                        " patient(s) with AEs leading to Study Discontinutation but no corresponding record in DS. ",
                        sep = ""),
                  mydf))
      
    }
  }
}