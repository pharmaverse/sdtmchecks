test_that("Function returns false when errors are present", {
    
    AE <- data.frame(
        STUDYID  = "1001",
        USUBJID  = c("1","2","3","4","5","1"),
        AESTDTC  = rep('2020-05-05', 6),
        AEDECOD  = c("HEADACHE", "HEART ATTACK","CHILLS", "PNEUMONIA", "ARTHRITIS", "FATIGUE"),
        AEACNOTH = c("NONE", "SUBJECT DISCONTINUED FROM STUDY", "MULTIPLE", "NONE",
                     "SUBJECT DISCONTINUED FROM STUDY", "SUBJECT DISCONTINUED FROM STUDY"),
        AEACNOT1 = c("", "", "PROCEDURE/SURGERY", "", "", ""),
        AEACNOT2 = c("", "", "SUBJECT DISCONTINUED FROM STUDY", "", "", ""),
        AESPID = "FORMNAME-R:13/L:13XXXX",
        stringsAsFactors = FALSE
    )
    
    DS <- data.frame(
        USUBJID = c("1","5"),
        DSCAT   = c("DISPOSITION EVENT", "DISPOSITION EVENT"),
        DSSCAT  = c("STUDY COMPLETION/EARLY DISCONTINUATION", "STUDY COMPLETION/EARLY DISCONTINUATION"),
        DSDECOD = c("ADVERSE EVENT", "ADVERSE EVENT" ),
        stringsAsFactors = FALSE
    )
    
    expect_false(check_ae_aeacnoth_ds_disctx(AE, DS))
})

test_that("Function returns false when errors are present - with preprocessing function specified", {
    
    AE <- data.frame(
        STUDYID  = "1001",
        USUBJID  = c("1","2","3","4","5","1"),
        AESTDTC  = rep('2020-05-05', 6),
        AEDECOD  = c("HEADACHE", "HEART ATTACK","CHILLS", "PNEUMONIA", "ARTHRITIS", "FATIGUE"),
        AEACNOTH = c("NONE", "SUBJECT DISCONTINUED FROM STUDY", "MULTIPLE", "NONE",
                     "SUBJECT DISCONTINUED FROM STUDY", "SUBJECT DISCONTINUED FROM STUDY"),
        AEACNOT1 = c("", "", "PROCEDURE/SURGERY", "", "", ""),
        AEACNOT2 = c("", "", "SUBJECT DISCONTINUED FROM STUDY", "", "", ""),
        AESPID = "FORMNAME-R:13/L:13XXXX",
        stringsAsFactors = FALSE
    )
    
    DS <- data.frame(
        USUBJID = c("1","5"),
        DSCAT   = c("DISPOSITION EVENT", "DISPOSITION EVENT"),
        DSSCAT  = c("STUDY COMPLETION/EARLY DISCONTINUATION", "STUDY COMPLETION/EARLY DISCONTINUATION"),
        DSDECOD = c("ADVERSE EVENT", "ADVERSE EVENT" ),
        stringsAsFactors = FALSE
    )
    
    expect_false(check_ae_aeacnoth_ds_disctx(AE, DS, preproc=roche_derive_rave_row))
})


test_that("Function returns true when no errors are present", {
    
    AE <- data.frame(
        STUDYID  = "1001",
        USUBJID  = c("1","2","3","4","5","1"),
        AESTDTC  = rep('2020-05-05', 6),
        AEDECOD  = c("HEADACHE", "HEART ATTACK","CHILLS", "PNEUMONIA", "ARTHRITIS", "FATIGUE"),
        AEACNOTH = c("NONE", "SUBJECT DISCONTINUED FROM STUDY", "MULTIPLE", "NONE",
                     "SUBJECT DISCONTINUED FROM STUDY", "SUBJECT DISCONTINUED FROM STUDY"),
        AEACNOT1 = c("", "", "PROCEDURE/SURGERY", "", "", ""),
        AEACNOT2 = c("", "", "SUBJECT DISCONTINUED FROM STUDY", "", "", ""),
        AESPID = "FORMNAME-R:13/L:13XXXX",
        stringsAsFactors = FALSE
    )
    
    DS <- data.frame(
        USUBJID = c("1", "2", "3", "5"),
        DSCAT   = rep("DISPOSITION EVENT", 4),
        DSSCAT  = rep("STUDY COMPLETION/EARLY DISCONTINUATION",4),
        DSDECOD = rep("ADVERSE EVENT", 4),
        stringsAsFactors = FALSE
    )
    
    expect_true(check_ae_aeacnoth_ds_disctx(AE, DS))
})







