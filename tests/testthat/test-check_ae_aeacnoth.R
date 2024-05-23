
test_that("Function returns true when no errors are present", {
    
    AE <- data.frame(
        USUBJID = 1:7,
        AETERM = 1:7,
        AESTDTC = 1:7,
        AEACNOTH = 1:7,
        AEACNOT1 = 1:7,
        AEACNOT2 = 1:7,
        AESPID = "FORMNAME-R:13/L:13XXXX"
    )
    
    expect_true(check_ae_aeacnoth(AE))
})



test_that("Function returns false when errors are present", {
    
    AE <- data.frame(
        USUBJID = 1:7,
        AETERM = 1:7,
        AESTDTC = 1:7,
        AEACNOTH = 1:7,
        AEACNOT1 = 1:7,
        AEACNOT2 = 1:7,
        AESPID = "FORMNAME-R:13/L:13XXXX"
    )
    
    AE$AEACNOTH[1] = ""
    AE$AEACNOT1[1] = ""
    AE$AEACNOT2[1] = ""
    AE$AEACNOTH[2] = "MULTIPLE"
    AE$AEACNOT1[2] = "DOSE REDUCED"
    AE$AEACNOT2[2] = "DRUG WITHDRAWN"
    AE$AEACNOTH[3] = "MULTIPLE"
    AE$AEACNOT1[3] = "DOSE REDUCED"
    AE$AEACNOT2[3] = ""
    AE$AEACNOTH[4] = "MULTIPLE"
    AE$AEACNOT1[4] = ""
    AE$AEACNOT2[4] = "DRUG WITHDRAWN"
    AE$AEACNOTH[5] = "MULTIPLE"
    AE$AEACNOT1[5] = ""
    AE$AEACNOT2[5] = ""
    
    expect_false(check_ae_aeacnoth(AE))
})



test_that("Function returns false when errors are present", {
    
    AE <- data.frame(
        USUBJID = 1:7,
        AETERM = 1:7,
        AESTDTC = 1:7,
        AEACNOTH = 1:7,
        AEACNOT1 = 1:7,
        AEACNOT2 = 1:7,
        AESPID = "FORMNAME-R:13/L:13XXXX"
    )
    
    AE$AEACNOTH[1] = NA
    AE$AEACNOT1[1] = NA
    AE$AEACNOT2[1] = NA
    AE$AEACNOTH[2] = "MULTIPLE"
    AE$AEACNOT1[2] = "DOSE REDUCED"
    AE$AEACNOT2[2] = "DRUG WITHDRAWN"
    AE$AEACNOTH[3] = "MULTIPLE"
    AE$AEACNOT1[3] = "DOSE REDUCED"
    AE$AEACNOT2[3] = NA
    AE$AEACNOTH[4] = "MULTIPLE"
    AE$AEACNOT1[4] = NA
    AE$AEACNOT2[4] = "DRUG WITHDRAWN"
    AE$AEACNOTH[5] = "MULTIPLE"
    AE$AEACNOT1[5] = NA
    AE$AEACNOT2[5] = NA
    
    expect_false(check_ae_aeacnoth(AE))
})


test_that("Function returns false when errors are present - with preprocessing function specified", {
    
    AE <- data.frame(
        USUBJID = 1:7,
        AETERM = 1:7,
        AESTDTC = 1:7,
        AEACNOTH = 1:7,
        AEACNOT1 = 1:7,
        AEACNOT2 = 1:7,
        AESPID = "FORMNAME-R:13/L:13XXXX"
    )
    
    AE$AEACNOTH[1] = ""
    AE$AEACNOT1[1] = ""
    AE$AEACNOT2[1] = ""
    AE$AEACNOTH[2] = "MULTIPLE"
    AE$AEACNOT1[2] = "DOSE REDUCED"
    AE$AEACNOT2[2] = "DRUG WITHDRAWN"
    AE$AEACNOTH[3] = "MULTIPLE"
    AE$AEACNOT1[3] = "DOSE REDUCED"
    AE$AEACNOT2[3] = ""
    AE$AEACNOTH[4] = "MULTIPLE"
    AE$AEACNOT1[4] = ""
    AE$AEACNOT2[4] = "DRUG WITHDRAWN"
    AE$AEACNOTH[5] = "MULTIPLE"
    AE$AEACNOT1[5] = ""
    AE$AEACNOT2[5] = ""
    
    expect_false(check_ae_aeacnoth(AE,preproc=roche_derive_rave_row))
})


test_that("Function returns false when columns missing", {
    
    AE <- data.frame(
        USUBJID = 1:7,
        AETERM = 1:7,
        AESTDTC = 1:7,
        AEACNOTH = 1:7,
        AEACNOT1 = 1:7,
        AEACNOT2 = 1:7,
        AESPID = "FORMNAME-R:13/L:13XXXX"
    )
    
    AE$AEACNOTH <- NULL
    AE$AEACNOT1 <- NULL
    AE$AEACNOT2 <- NULL
    AE$AESPID <- NULL
    
    expect_false(check_ae_aeacnoth(AE))
})
    









