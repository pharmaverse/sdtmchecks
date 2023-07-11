test_that("function errors when given bad input", {
    #expect_error(check_dd_ae_aeout_aedthdtc(list()))
    expect_error(check_dd_ae_aeout_aedthdtc(data.frame(USUBJID = 1:4,
                                                             AEDTHDTC = 1:3,
                                                             AEDECOD = 1:3,
                                                             AESTDTC = 1:3,
                                                             AEOUT = rep("FATAL", 3),
                                                             stringsAsFactors = FALSE)))

})

test_that("function returns true when no errors are present", {

    AE <- data.frame(USUBJID = 1:3,
                     AEDTHDTC = 1:3,
                     AEDECOD = 1:3,
                     AESTDTC = 1:3,
                     AEOUT = rep("FATAL", 3),
                     stringsAsFactors = FALSE)

    expect_true(check_dd_ae_aeout_aedthdtc(AE))

})

test_that("function returns false when errors are present", {

    AE <- data.frame(USUBJID = 1:3,
                     AEDTHDTC = 1:3,
                     AEDECOD = 1:3,
                     AESTDTC = 1:3,
                     AEOUT = rep("FATAL", 3),
                     stringsAsFactors = FALSE)

    AE[3, "AEDTHDTC"] <- NA
    #' AE[1, "AEOUT"] <- NA


    expect_false(check_dd_ae_aeout_aedthdtc(AE))

})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    AE <- data.frame(USUBJID =integer(),
                     AEDTHDTC=integer(),
                     AEDECOD = character(),
                     AESTDTC = integer(),
                     AEOUT = character(),
                     stringsAsFactors=FALSE)

    expect_true(check_dd_ae_aeout_aedthdtc(AE))
})

# test_that("Function returns false when errors are present for an empty dataframe (zero rows)", {
#
#     AE <- data.frame(USUBJID =NA,
#                      AESEQ=NA,
#                      AESTDTC=NA,
#                      AETERM="",
#                      AEDECOD ="NA",
#                      stringsAsFactors=FALSE)
#
#     expect_false(check_ae_aedecod(AE))
# })

test_that("Function returns true when no errors are present for a single input (one row)", {

    AE <- data.frame(USUBJID = 1,
                     AEDTHDTC = 1,
                     AEDECOD = 1,
                     AESTDTC = 1,
                     AEOUT = "FATAL",
                     stringsAsFactors = FALSE)


    expect_true(check_dd_ae_aeout_aedthdtc(AE))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    AE <- data.frame(USUBJID = 1,
                     AEDTHDTC = 1,
                     AEDECOD = 1,
                     AESTDTC = 1,
                     AEOUT = "FATAL",
                     stringsAsFactors = FALSE)

    AE[1, "AEDTHDTC"] <- NA
    # AE[1, "AEOUT"] <- NA

    expect_false(check_dd_ae_aeout_aedthdtc(AE))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     AEDTHDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
                                         "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
                     AESTDTC = as.character(rep(seq(as.Date('2010-01-01'), as.Date('2010-03-01'), by = 1), times = 15)),
                     AEOUT = "FATAL",
                     stringsAsFactors = FALSE)

    expect_true(check_dd_ae_aeout_aedthdtc(AE))
})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     AEDTHDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
                                         "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
                     AESTDTC = as.character(rep(seq(as.Date('2010-01-01'), as.Date('2010-03-01'), by = 1), times = 15)),
                     AEOUT = "FATAL",
                     stringsAsFactors = FALSE)

    AE[1:10, "AEDTHDTC"] <- NA

    expect_false(check_dd_ae_aeout_aedthdtc(AE))

})

test_that("Function returns the failed object in attr(data)", {

    AE <- data.frame(USUBJID = 1:3,
                     AEDECOD = 1:3,
                     AESTDTC = 1:3,
                     AEDTHDTC = 1:3,
                     AEOUT = rep("FATAL", 3),
                     stringsAsFactors = FALSE)

    AE[1, "AEDTHDTC"] <- NA
    #' AE[1, "AEOUT"] <- NA

    check <- check_dd_ae_aeout_aedthdtc(AE)

    expect_true(!is.null(attr(check, "data")))
    expect_equal(attr(check, "data"), filter(AE, is.na(AEDTHDTC) & AEOUT == "FATAL"))

})

