test_that("Function returns false when errors are present", {
    
    AE <- data.frame(
        USUBJID = 1:10,
        DOMAIN = "AE",
        AEDTHDTC = c(NA, "NA", rep("2015-03-12",4), NA, NA, "2020-01-01", ""),
        AEENDTC = c(NA, "NA", rep("2015-03-12",4), NA, "2020-01-01", NA, ""),
        AEOUT = c("", "", "","FATAL","RECOVERED/RESOLVED", rep("FATAL",5)),
        AETERM = 1:10,
        AESPID = "FORMNAME-R:13/L:13XXXX",
        stringsAsFactors = FALSE)
    
    expect_false(check_ae_aeout_aeendtc_aedthdtc(AE))
})

test_that("Function returns false when errors are present - with preprocessing function specified", {
    
    AE <- data.frame(
        USUBJID = 1:10,
        DOMAIN = "AE",
        AEDTHDTC = c(NA, "NA", rep("2015-03-12",4), NA, NA, "2020-01-01", ""),
        AEENDTC = c(NA, "NA", rep("2015-03-12",4), NA, "2020-01-01", NA, ""),
        AEOUT = c("", "", "","FATAL","RECOVERED/RESOLVED", rep("FATAL",5)),
        AETERM = 1:10,
        AESPID = "FORMNAME-R:13/L:13XXXX",
        stringsAsFactors = FALSE)
    
    expect_false(check_ae_aeout_aeendtc_aedthdtc(AE,preproc=roche_derive_rave_row))
})


test_that("Function returns false when errors are present despite null AESPID", {
    
    AE <- data.frame(
        USUBJID = 1:10,
        DOMAIN = "AE",
        AEDTHDTC = c(NA, "NA", rep("2015-03-12",4), NA, NA, "2020-01-01", ""),
        AEENDTC = c(NA, "NA", rep("2015-03-12",4), NA, "2020-01-01", NA, ""),
        AEOUT = c("", "", "","FATAL","RECOVERED/RESOLVED", rep("FATAL",5)),
        AETERM = 1:10,
        AESPID = "FORMNAME-R:13/L:13XXXX",
        stringsAsFactors = FALSE)
    
    AE$AESPID <- NULL
    check_ae_aeout_aeendtc_aedthdtc(AE)
    
    expect_false(check_ae_aeout_aeendtc_aedthdtc(AE))
})


test_that("Function returns false when key columns set to null", {
    
    AE <- data.frame(
        USUBJID = 1:10,
        DOMAIN = "AE",
        AEDTHDTC = c(NA, "NA", rep("2015-03-12",4), NA, NA, "2020-01-01", ""),
        AEENDTC = c(NA, "NA", rep("2015-03-12",4), NA, "2020-01-01", NA, ""),
        AEOUT = c("", "", "","FATAL","RECOVERED/RESOLVED", rep("FATAL",5)),
        AETERM = 1:10,
        AESPID = "FORMNAME-R:13/L:13XXXX",
        stringsAsFactors = FALSE)
    
    AE$AEDTHDTC <- NULL
    AE$AEOUT <- NULL
    
    expect_false(check_ae_aeout_aeendtc_aedthdtc(AE))
})


    









