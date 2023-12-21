
test_that("function returns false when errors are present", {
    
    AE <- data.frame(
        USUBJID = 1:10,
        AETERM  = "AE",
        AESTDTC = c(NA, "NA", "2015-03-09", "2010-10", "2017-01-20",   "1999-11-02",        "",     NA,                   "2017-08-20",          "2014-12-01"),
        AEENDTC = c(NA, "NA", "2015-03-12", "2010-10", "2017-01-22",   "1999-11-07",        "",     NA,                   "2017-09-01",          "2015-01-01"),
        AEOUT   = c("", "",   "",           "",        "NOT RECOVERED","RECOVERED/RESOLVED","FATAL","RECOVERED/RESOLVED", "RECOVERING/RESOLVING","UNKNOWN"),
        AESPID  = "FORMNAME-R:13/L:13XXXX",
        stringsAsFactors = FALSE
    )
    
    expect_false(check_ae_aeout_aeendtc_nonfatal(AE))
    
})

test_that("function returns false when errors are present - with preprocessing", {
    
    AE <- data.frame(
        USUBJID = 1:10,
        AETERM  = "AE",
        AESTDTC = c(NA, "NA", "2015-03-09", "2010-10", "2017-01-20",   "1999-11-02",        "",     NA,                   "2017-08-20",          "2014-12-01"),
        AEENDTC = c(NA, "NA", "2015-03-12", "2010-10", "2017-01-22",   "1999-11-07",        "",     NA,                   "2017-09-01",          "2015-01-01"),
        AEOUT   = c("", "",   "",           "",        "NOT RECOVERED","RECOVERED/RESOLVED","FATAL","RECOVERED/RESOLVED", "RECOVERING/RESOLVING","UNKNOWN"),
        AESPID  = "FORMNAME-R:13/L:13XXXX",
        stringsAsFactors = FALSE
    )
    
    expect_false(check_ae_aeout_aeendtc_nonfatal(AE,preproc=roche_derive_rave_row))
    
})

test_that("function returns false when variable missing", {
    
    AE <- data.frame(
        USUBJID = 1:10,
        AETERM  = "AE",
        AESTDTC = c(NA, "NA", "2015-03-09", "2010-10", "2017-01-20",   "1999-11-02",        "",     NA,                   "2017-08-20",          "2014-12-01"),
        AEENDTC = c(NA, "NA", "2015-03-12", "2010-10", "2017-01-22",   "1999-11-07",        "",     NA,                   "2017-09-01",          "2015-01-01"),
        AEOUT   = c("", "",   "",           "",        "NOT RECOVERED","RECOVERED/RESOLVED","FATAL","RECOVERED/RESOLVED", "RECOVERING/RESOLVING","UNKNOWN"),
        AESPID  = "FORMNAME-R:13/L:13XXXX",
        stringsAsFactors = FALSE
    )
    AE$AEENDTC <- NULL
    
    expect_false(check_ae_aeout_aeendtc_nonfatal(AE))
    
})

test_that("function returns false when variable missing", {
    
    AE <- data.frame(
        USUBJID = 1:10,
        AETERM  = "AE",
        AESTDTC = c(NA, "NA", "2015-03-09", "2010-10", "2017-01-20",   "1999-11-02",        "",     NA,                   "2017-08-20",          "2014-12-01"),
        AEENDTC = c(NA, "NA", "2015-03-12", "2010-10", "2017-01-22",   "1999-11-07",        "",     NA,                   "2017-09-01",          "2015-01-01"),
        AEOUT   = c("", "",   "",           "",        "NOT RECOVERED","RECOVERED/RESOLVED","FATAL","RECOVERED/RESOLVED", "RECOVERING/RESOLVING","UNKNOWN"),
        AESPID  = "FORMNAME-R:13/L:13XXXX",
        stringsAsFactors = FALSE
    )
    AE$AEOUT <- NULL
    
    expect_false(check_ae_aeout_aeendtc_nonfatal(AE))
    
})



