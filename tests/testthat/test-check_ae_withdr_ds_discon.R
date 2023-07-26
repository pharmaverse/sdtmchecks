
test_that("Function returns false when no errors are present", {
    
    
    AE <- data.frame(
        USUBJID = 1:6,
        AEACN = c("DRUG WITHDRAWN",NA,NA,NA,NA,NA),
        AETOXGR = c(NA,NA,NA,NA,"5",NA),
        AEDECOD=c("NAUSEA","HEADACHE"),
        AESPID = "FORMNAME-R:5/L:5XXXX"
    )
    
    DS <- data.frame(
        USUBJID = 1:3,
        DSCAT="DISPOSITION EVENT",
        DSSCAT="STUDY TREATMENT",
        DSDECOD=c("COMPLETED","ADVERSE EVENT","DEATH")
    )
    
    TS <- data.frame(
        TSPARMCD="TRT",
        TSVAL="CHECK"
    )
    
    expect_false(check_ae_withdr_ds_discon(AE,DS,TS))
})


test_that("Function returns false when no errors are present - with processing", {
    
    
    AE <- data.frame(
        USUBJID = 1:6,
        AEACN = c("DRUG WITHDRAWN",NA,NA,NA,NA,NA),
        AETOXGR = c(NA,NA,NA,NA,"5",NA),
        AEDECOD=c("NAUSEA","HEADACHE"),
        AESPID = "FORMNAME-R:5/L:5XXXX"
    )
    
    DS <- data.frame(
        USUBJID = 1:3,
        DSCAT="DISPOSITION EVENT",
        DSSCAT="STUDY TREATMENT",
        DSDECOD=c("COMPLETED","ADVERSE EVENT","DEATH")
    )
    
    TS <- data.frame(
        TSPARMCD="TRT",
        TSVAL="CHECK"
    )
    
    expect_false(check_ae_withdr_ds_discon(AE,DS,TS, preproc=roche_derive_rave_row))
})


test_that("Function returns false when no errors are present", {
    
    
    AE <- data.frame(
        USUBJID = 1:6,
        AEACN = c("DRUG WITHDRAWN",NA,NA,NA,NA,NA),
        AETOXGR = c(NA,NA,NA,NA,"5",NA),
        AEDECOD=c("NAUSEA","HEADACHE"),
        AESPID = "FORMNAME-R:5/L:5XXXX"
    )
    
    DS <- data.frame(
        USUBJID = 1:3,
        DSCAT="DISPOSITION EVENT",
        DSSCAT="STUDY TREATMENT",
        DSDECOD=c("COMPLETED","ADVERSE EVENT","DEATH")
    )
    
    TS <- data.frame(
        TSPARMCD="TRT",
        TSVAL="CHECK"
    )
    
    DS$DSSCAT = NULL
    
    expect_false(check_ae_withdr_ds_discon(AE,DS,TS))
})

