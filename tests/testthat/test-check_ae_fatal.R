test_that("function errors when given bad input", {
    #expect_error(check_ae_fatal(list()))
    expect_error(check_ae_fatal(data.frame(USUBJID = 1:6,
                                                 AESTDTC = "01JAN2017",
                                                 AEDECOD = c("AE1","AE2","AE3","AE4","AE5"),
                                                 AEOUT = "FATAL",
                                                 AEDTHDTC = c("01FEB2017",NA,"02FEB2017","03FEB2017",NA),
                                                 AESDTH = c("Y","Y","N","Y",NA),
                                                 AETOXGR = c("5","5","5",NA,NA),
                                                 stringsAsFactors = FALSE)))

})

test_that("function returns true when no errors are present", {

    AE <- data.frame(
        USUBJID = 1:5,
        AESTDTC = "01JAN2017",
        AEDECOD = c("AE1","AE2","AE3","AE4","AE5"),
        AEOUT = "FATAL",
        AEDTHDTC = c("01FEB2017","02FEB2017","03FEB2017","04FEB2017","05FEB2017"),
        AESDTH = c("Y","Y","Y","Y","Y"),
        AETOXGR = c("5","5","5","5","5"),
        stringsAsFactors = FALSE
    )

    expect_true(check_ae_fatal(AE))

})

test_that("function returns false when errors are present", {

    AE <- data.frame(
        USUBJID = 1:5,
        AESTDTC = "01JAN2017",
        AEDECOD = c("AE1","AE2","AE3","AE4","AE5"),
        AEOUT = "FATAL",
        AEDTHDTC = c("01FEB2017",NA,"02FEB2017","03FEB2017",NA),
        AESDTH = c("Y","Y","N","Y",NA),
        AETOXGR = c("5","5","5",NA,NA),
        stringsAsFactors = FALSE
    )

    expect_false(check_ae_fatal(AE))


})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    AE <- data.frame(
        USUBJID = integer(),
        AESTDTC = character(),
        AEDECOD = character(),
        AEOUT = character(),
        AEDTHDTC = character(),
        AESDTH = character(),
        AETOXGR = character(),
        stringsAsFactors = FALSE
    )

    expect_true(check_ae_fatal(AE))
})


test_that("Function returns true when no errors are present for a single input (one row)", {

    AE <- data.frame(
        USUBJID = 1:5,
        AESTDTC = "01JAN2017",
        AEDECOD = c("AE1"),
        AEOUT = "FATAL",
        AEDTHDTC = c("01FEB2017"),
        AESDTH = c("Y"),
        AETOXGR = c("5"),
        stringsAsFactors = FALSE
    )
    expect_true(check_ae_fatal(AE))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    AE <- data.frame(
        USUBJID = 1:5,
        AESTDTC = "01JAN2017",
        AEDECOD = c("AE1"),
        AEOUT = "FATAL",
        AEDTHDTC = c("01FEB2017"),
        AESDTH = c("N"),
        AETOXGR = c("5"),
        stringsAsFactors = FALSE
    )

    expect_false(check_ae_fatal(AE))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     AESTDTC = as.character(rep(seq(as.Date('2010-01-01'), as.Date('2010-03-01'), by = 1), times = 15)),
                     AEDTHDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     AESDTH = rep("Y", times = 900),
                     AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
                                         "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
                     AEOUT = "FATAL",
                     AESDTH = "Y",
                     AETOXGR = "5",
                     stringsAsFactors = FALSE)

    expect_true(check_ae_fatal(AE))
})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     AESTDTC = as.character(rep(seq(as.Date('2010-01-01'), as.Date('2010-03-01'), by = 1), times = 15)),
                     AEDTHDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     AESDTH = rep("Y", times = 900),
                     AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
                                         "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
                     AEOUT = "FATAL",
                     AESDTH = "Y",
                     AETOXGR = c(rep("5", times = 800), rep("3", times = 100)),
                     stringsAsFactors = FALSE)


    expect_false(check_ae_fatal(AE))
})

test_that("Function returns the failed object in attr(data)", {

    AE <- data.frame(
        USUBJID = 1:5,
        AESTDTC = "01JAN2017",
        AEDECOD = c("AE1","AE2","AE3","AE4","AE5"),
        AEOUT = "FATAL",
        AEDTHDTC = c("01FEB2017",NA,"02FEB2017","03FEB2017",NA),
        AESDTH = c("Y","Y","N","Y",NA),
        AETOXGR = c("5","5","5",NA,NA),
        stringsAsFactors = FALSE
    )

    check <- check_ae_fatal(AE)

    expect_true(!is.null(attr(check, "data")))
    # expect_equal(attr(check, "data"), filter(AE, !is.na(AEDTHDTC) & AESDTH != "Y"))

})


test_that("function returns false when errors are present", {
    
    AE <- data.frame(
        USUBJID = 1:5,
        AESTDTC = "01JAN2017",
        AEDECOD = c("AE1","AE2","AE3","AE4","AE5"),
        AEOUT = "FATAL",
        AEDTHDTC = c("01FEB2017",NA,"02FEB2017","03FEB2017",NA),
        AESDTH = c("Y","Y","N","Y",NA),
        AESEV = NA,
        AETOXGR = NA,
        AESPID = "FORMNAME-R:12/L:2XXXX",
        stringsAsFactors = FALSE
    )
    
    expect_false(check_ae_fatal(AE))
    
})






