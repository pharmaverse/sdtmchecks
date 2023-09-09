
test_that("Function returns true when no errors are present", {
    
    
    AE <- data.frame(
        USUBJID = 1:10,
        AETOXGR = c(1:5,5,5,5,5,5),
        AEDTHDTC = c(rep(NA,4),rep("2020-01-01",6)),
        AESDTH = c(rep(NA,4),rep("Y",6)),
        AEOUT = c(rep(NA,4),rep("FATAL",6)),
        AESPID = "FORMNAME-R:13/L:13XXXX"
    )
    
    expect_true(check_ae_death(AE,preproc=roche_derive_rave_row))
})



test_that("Function returns true when no errors are present - with processing", {
    
    
    AE <- data.frame(
        USUBJID = 1:10,
        AETOXGR = c(1:5,5,5,5,5,5),
        AEDTHDTC = c(rep(NA,4),rep("2020-01-01",6)),
        AESDTH = c(rep(NA,4),rep("Y",6)),
        AEOUT = c(rep(NA,4),rep("FATAL",6)),
        AESPID = "FORMNAME-R:13/L:13XXXX"
    )
    
    expect_true(check_ae_death(AE))
})

test_that("Function returns false when errors are present", {
    
    
    AE <- data.frame(
        USUBJID = 1:10,
        AETOXGR = c(1:5,5,5,5,5,5),
        AEDTHDTC = c(rep(NA,4),rep("2020-01-01",6)),
        AESDTH = c(rep(NA,4),rep("Y",6)),
        AEOUT = c(rep(NA,4),rep("FATAL",6)),
        AESPID = "FORMNAME-R:13/L:13XXXX"
    )
    
    AE$AEDTHDTC[5]="NA"
    AE$AEDTHDTC[6]=NA
    AE$AEDTHDTC[7]=""
    AE$AESDTH[8]=NA
    AE$AEOUT[9]=NA
    
    expect_false(check_ae_death(AE))
})

test_that("Function returns false when errors are present - with processing", {
    
    
    AE <- data.frame(
        USUBJID = 1:10,
        AETOXGR = c(1:5,5,5,5,5,5),
        AEDTHDTC = c(rep(NA,4),rep("2020-01-01",6)),
        AESDTH = c(rep(NA,4),rep("Y",6)),
        AEOUT = c(rep(NA,4),rep("FATAL",6)),
        AESPID = "FORMNAME-R:13/L:13XXXX"
    )
    
    AE$AEDTHDTC[5]="NA"
    AE$AEDTHDTC[6]=NA
    AE$AEDTHDTC[7]=""
    AE$AESDTH[8]=NA
    AE$AEOUT[9]=NA
    
    expect_false(check_ae_death(AE,preproc=roche_derive_rave_row))
})
