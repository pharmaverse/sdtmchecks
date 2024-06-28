# test_that("function errors when given bad input", {
#     expect_error(check_ae_aedthdtc_aesdth(list()))
#     expect_error(check_ae_aedthdtc_aesdth(data.frame(USUBJID = c(1),
#                                                    AEDTHDTC = c(1, "NA", NA),
#                                                    AESDTH = c(rep("", 2), "Y", rep("", 2)),
#                                                    AEDECOD = letters[1],
#                                                    AESTDTC = c(1),
#                                                    stringsAsFactors=FALSE)))
#
# })

test_that("function returns true when no errors are present", {

    AE <- data.frame(USUBJID = c(1:5), AEDTHDTC = c(1:5),
                      AESDTH = c(rep("Y", 5)),
                      AEDECOD = letters[1:5], AETERM = letters[1:5], AESTDTC = c(1:5),
                      stringsAsFactors=FALSE)

    expect_true(check_ae_aedthdtc_aesdth(AE))

})

test_that("function returns false when errors are present", {

    AE <- data.frame(USUBJID = c(1:5), AEDTHDTC = c(1:3, "NA", NA),
                     AESDTH = c(rep("", 2), "Y", rep("", 2)),
                     AEDECOD = letters[1:5], AETERM = letters[1:5], AESTDTC = c(1:5),
                     stringsAsFactors=FALSE)

    AE$AEDTHDTC[1:2] <- 1:2

    expect_false(check_ae_aedthdtc_aesdth(AE = AE))


})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    AE <- data.frame(USUBJID =integer(),
                     AEDTHDTC=integer(),
                     AESDTH=character(),
                     AETERM=integer(),
                     AEDECOD = character(),
                     AETERM = character(),
                     AESTDTC = integer(),
                     stringsAsFactors=FALSE)

    expect_true(check_ae_aedthdtc_aesdth(AE))
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
#     expect_false(check_all_1_ae_aedecod(AE))
# })

test_that("Function returns true when no errors are present for a single input (one row)", {

    AE <- data.frame(USUBJID = c(1),
                     AEDTHDTC = c(1),
                     AESDTH = c("Y"),
                     AEDECOD = letters[1],
                     AETERM = letters[1],
                     AESTDTC = c(1),
                     stringsAsFactors=FALSE)

    expect_true(check_ae_aedthdtc_aesdth(AE))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    AE <- data.frame(USUBJID = c(1),
                     AEDTHDTC = c(1),
                     AESDTH = c(""),
                     AEDECOD = letters[1],
                     AETERM = letters[1],
                     AESTDTC = c(1),
                     stringsAsFactors=FALSE)

    expect_false(check_ae_aedthdtc_aesdth(AE))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     # AESEQ = rep(x = seq(1, 60, by=1), times = 900),
                     AEDTHDTC = as.character(rep(seq(as.Date('2023-01-01'), as.Date('2023-03-01'), by = 1), times = 15)),
                     AESDTH = rep("Y", times = 900),
                     AETERM = rep(x = c("UPPER RESPIRATORY INFECTION","HEADACHE",
                                        "WORSENING THROMBOCYTOPENIA","LOW POTASSIUM","ANEMIA", "TREMORS"), times = 150),
                     AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
                                         "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
                     AESTDTC = as.character(rep(seq(as.Date('2023-01-01'), as.Date('2023-03-01'), by = 1), times = 15)),
                     stringsAsFactors = FALSE)

    expect_true(check_ae_aedthdtc_aesdth(AE))
})

# test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {
#
#     USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)
#
#     AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
#                      # AESEQ = rep(x = seq(1, 60, by=1), times = 900),
#                      AEDTHDTC = as.character(rep(seq(as.Date('2023-01-01'), as.Date('2023-03-01'), by = 1), times = 15)),
#                      # AESDTH = rep('N', times = 900),
#                      AESDTH = rep(NA, times = 900),
#                      # AETERM = rep(x = c("UPPER RESPIRATORY INFECTION","HEADACHE",
#                      #                    "WORSENING THROMBOCYTOPENIA","LOW POTASSIUM","ANEMIA", "TREMORS"), times = 9000),
#                      AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
#                                          "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
#                      AESTDTC = as.character(rep(seq(as.Date('2023-01-01'), as.Date('2023-03-01'), by = 1), times = 15)),
#                      stringsAsFactors = FALSE)
#
#
#     expect_false(check_ae_aedthdtc_aesdth(AE))
# })

# test_that("Function returns the failed object in attr(data)", {
#
#     AE <- data.frame(USUBJID = c(1:5),
#                      AEDECOD = letters[1:5], AESTDTC = c(1:5),
#                      AEDTHDTC = c(1:3, "NA", NA),
#                      AESDTH = c(rep("", 2), "Y", rep("", 2)),
#                      stringsAsFactors=FALSE)
#
#     AE$AEDTHDTC[1:2] <- 1:2
#
#     check <- check_ae_aedthdtc_aesdth(AE)
#
#     expect_true(!is.null(attr(check, "data")))
#     # is.na("NA") FALSE
#     expect_equal(attr(check, "data"), filter(AE, !is.na(AEDTHDTC) & AESDTH != "Y"))
#
# })


