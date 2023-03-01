#' @title Check if, whenever a patient experiences an AE leading to study discontinuation,
#' they also have a DS record indicating this.
#'
#' @description This code checks that when a patient has an AE with AEACNOTx = "SUBJECT
#' DISCONTINUED FROM STUDY" (x = "H", "1", "2", ...) then there should also be a record in DS
#' where DS.DSSCAT = "STUDY COMPLETION/EARLY DISCONTINUATION" and DS.DSDECOD != "COMPLETED".
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AEDECOD, AEACNOTx
#' @param DS Disposition SDTM dataset with variables USUBJID, DSCAT, DSSCAT, DSDECOD
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#' 
#' @importFrom tidyselect any_of
#'
#' @return boolean value if check returns 0 obs, otherwise return subset dataframe.
#'
#' @export
#'
#' @author Edoardo Mancini
#'
#' @examples
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
#'     AESPID = "FORMNAME-R:13/L:13XXXX",
#'     stringsAsFactors = FALSE
#' )
#' 
#' DS <- data.frame(
#'     USUBJID = c("1","5"),
#'     DSCAT   = c("DISPOSITION EVENT", "DISPOSITION EVENT"),
#'     DSSCAT  = c("STUDY COMPLETION/EARLY DISCONTINUATION", "STUDY COMPLETION/EARLY DISCONTINUATION"),
#'     DSDECOD = c("ADVERSE EVENT", "ADVERSE EVENT" ),
#'     stringsAsFactors = FALSE
#' )
#'
#' check_ae_aeacnoth_ds_disctx(AE, DS)
#' check_ae_aeacnoth_ds_disctx(AE, DS, preproc=roche_derive_rave_row)
#'
check_ae_aeacnoth_ds_disctx <- function(AE, DS, preproc=identity,...){
  
  # First check that required variables exist and return a message if they don't
  if( AE %lacks_any% c("USUBJID", "AEDECOD", "AEACNOTH")){
    
    fail(lacks_msg(AE, c("USUBJID", "AEDECOD", "AEACNOTH")))
    
  }else if( DS %lacks_any% c("USUBJID", "DSCAT", "DSSCAT", "DSDECOD")){
    
    fail(lacks_msg(DS, c("USUBJID", "DSCAT"," DSSCAT", "DSDECOD")))
    
  } else{
    
    #Apply company specific preprocessing function
    AE = preproc(AE,...)
    
    # Get all AEACNOTx (x = "H", "1", "2", etc) columns
    aeacnotx_cols <- c("AEACNOTH", grep("AEACNOT[0-9]", names(AE), value = TRUE))
    
    # Keep only AE columns that are needed
    ae1 <- AE %>%
      select(any_of(c("USUBJID", "AEDECOD", "AESTDTC", aeacnotx_cols, "RAVE")))
    
    # Filter for AE records where any of AEACNOTHx (x = "", "1", "2", etc) is "SUBJECT DISCONTINUED FROM STUDY"
    ae1$subj_discont_fl <- apply(ae1[aeacnotx_cols] == "SUBJECT DISCONTINUED FROM STUDY", 1, any)
    
    ae2 <- ae1 %>%
      filter(subj_discont_fl) %>%
      select(-subj_discont_fl)
    
    # Keep only DS columns that are needed
    ds1 <- DS %>% select(c("USUBJID", "DSCAT", "DSSCAT","DSDECOD"))
    
    # Filter for DS records indicating subject didn't complete the study
    ds2 <- ds1 %>% filter(grepl("DISCON", toupper(DSSCAT)) &
                          !grepl("DRUG", toupper(DSSCAT)) &
                          !grepl("TREATMENT", toupper(DSSCAT)) 
                          & DSDECOD != "COMPLETED") 
    
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
                        " patient(s) with AEs leading to Study Discontinuation but no corresponding record in DS. ",
                        sep = ""),
                  mydf))
      
    }
  }
}
