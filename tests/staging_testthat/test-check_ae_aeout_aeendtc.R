
test_that("function returns true when no errors are present", {

    AE <- data.frame(
        USUBJID = 1:8,
        AEENDTC = c(NA, "NA", "NA",       "",                           "NA",                   "2022-01-05", "2012-05-01",         "1999-01-22"),
        AEOUT   = c("", "",   "UNKNOWN",  "NOT RECOVERED/NOT RESOLVED", "RECOVERING/RESOLVING", "FATAL",      "RECOVERED/RESOLVED", "RECOVERED/RESOLVED WITH SEQUELAE"),
        stringsAsFactors = FALSE
    )

    expect_true(check_ae_aeout_aeendtc(AE))

})

test_that("function returns false when errors are present", {

    AE <- data.frame(
        USUBJID = 1:7,
        AEENDTC = c("2011-01-01", "2011-01-09", "NA",                         "",                     "NA",   "",                   "2012-05-01"),
        AEOUT   = c("",           "UNKNOWN",    "NOT RECOVERED/NOT RESOLVED", "RECOVERING/RESOLVING", "FATAL","RECOVERED/RESOLVED", "RECOVERED/RESOLVED WITH SEQUELAE"),
        stringsAsFactors = FALSE
    )
    
    expect_false(check_ae_aeout_aeendtc(AE))


})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    AE <- data.frame(USUBJID =integer(),
                     AEENDTC=character(),
                     AEOUT=character(),
                     stringsAsFactors=FALSE)

    expect_true(check_ae_aeout_aeendtc(AE))
})

test_that("Date missing and outcome unknown - returns true", {

    AE <- data.frame(
        USUBJID = 1,
        AEENDTC = c(""),
        AEOUT   = c("UNKNOWN"),
        stringsAsFactors = FALSE
    )

    expect_true(check_ae_aeout_aeendtc(AE))
})

test_that("Date missing and outcome not recovered - returns true", {
    
    AE <- data.frame(
        USUBJID = 1,
        AEENDTC = c(""),
        AEOUT   = c("NOT RECOVERED/NOT RESOLVED"),
        stringsAsFactors = FALSE
    )
    
    expect_true(check_ae_aeout_aeendtc(AE))
})

test_that("Date missing and outcome unknown - returns true", {
    
    AE <- data.frame(
        USUBJID = 1,
        AEENDTC = c(""),
        AEOUT   = c("RECOVERING/RESOLVING"),
        stringsAsFactors = FALSE
    )
    
    expect_true(check_ae_aeout_aeendtc(AE))
})

test_that("Outcome fatal and date missing - returns false", {

    AE <- data.frame(
        USUBJID = 1,
        AEENDTC = c(""),
        AEOUT   = c("FATAL"),
        stringsAsFactors = FALSE
    )

    expect_false(check_ae_aeout_aeendtc(AE))
})

test_that("Outcome recovered and date missing - returns false", {
    
    AE <- data.frame(
        USUBJID = 1,
        AEENDTC = c(""),
        AEOUT   = c("FATAL"),
        stringsAsFactors = FALSE
    )
    
    expect_false(check_ae_aeout_aeendtc(AE))
})


test_that("Function returns the failed object in attr(data)", {

    AE <- data.frame(
        USUBJID = 1,
        AEENDTC = c("2023-03-12"),
        AEOUT   = c("UNKNOWN"),
        stringsAsFactors = FALSE
    )

    check <- check_ae_aeout_aeendtc(AE)

    expect_true(!is.null(attr(check, "data")))

})


