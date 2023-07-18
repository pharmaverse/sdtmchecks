
test_that("Function returns false when errors are present", {
    
    AE <- data.frame(
        USUBJID = 1:5,
        AESTDTC = 1:5,
        AELOC   = c("", "EYE", "eye", "", "EYE"),
        AELAT   = c("Left", "","left", "RIGHT", ""),
        AETERM  = c("A", "B", "A", "B", "A"),
        AEDECOD = c("A", "B", "A", "B", "A"),
        AESOC   = c("Eye", "Eye","Eye Disorder","Eye Disorder", "Eye"),
        AESPID  = "FORMNAME-R:19/L:19XXXX",
        stringsAsFactors = FALSE)
    
    expect_false(check_ae_aelat(AE))
})


test_that("Function returns false when errors are present - with preprocessing function specified", {
    
    AE <- data.frame(
        USUBJID = 1:5,
        AESTDTC = 1:5,
        AELOC   = c("", "EYE", "eye", "", "EYE"),
        AELAT   = c("Left", "","left", "RIGHT", ""),
        AETERM  = c("A", "B", "A", "B", "A"),
        AEDECOD = c("A", "B", "A", "B", "A"),
        AESOC   = c("Eye", "Eye","Eye Disorder","Eye Disorder", "Eye"),
        AESPID  = "FORMNAME-R:19/L:19XXXX",
        stringsAsFactors = FALSE)
    
    expect_false(check_ae_aelat(AE, preproc=roche_derive_rave_row))
})


test_that("Function returns false when errors are present", {
    
    AE <- data.frame(
        USUBJID = 1:5,
        AESTDTC = 1:5,
        AELAT   = c("Left", "","Bilateral", "", ""),
        AETERM  = c("A", "B", "A", "B", "A"),
        AEDECOD = c("A", "B", "A", "B", "A"),
        AESOC   = c("Eye", "Eye","Eye Disorder","Eye Disorder", "Eye"),
        stringsAsFactors = FALSE)
    
    expect_false(check_ae_aelat(AE))
})


test_that("Function returns false when errors are present - with preprocessing function specified", {
    
    AE <- data.frame(
        USUBJID = 1:5,
        AESTDTC = 1:5,
        AELAT   = c("Left", "","Bilateral", "", ""),
        AETERM  = c("A", "B", "A", "B", "A"),
        AEDECOD = c("A", "B", "A", "B", "A"),
        AESOC   = c("Eye", "Eye","Eye Disorder","Eye Disorder", "Eye"),
        stringsAsFactors = FALSE)
    
    expect_false(check_ae_aelat(AE, preproc=roche_derive_rave_row))
})

