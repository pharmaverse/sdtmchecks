test_that("function errors when given bad input", {
    expect_error(check_ae_aestdtc_after_dd(list()))

    # AE <- data.frame(STUDYID = 1:6, USUBJID = LETTERS[1:5],
    #                  AEDTHDTC = c(rep("", 4), "2016-01-01"),
    #                  AESTDTC = rep("2016-01-01", 5),
    #                  AEDECOD = LETTERS[1:5], AETERM = LETTERS[1:5],
    #                  stringsAsFactors = FALSE)
    #
    # DS <- data.frame(STUDYID = 1:6, USUBJID = LETTERS[1:5],
    #                  DSSTDTC = rep("2016-01-02", 5),
    #                  DSDECOD = c(LETTERS[1:4], "death"),
    #                  DSTERM = letters[1:5],
    #                  stringsAsFactors = FALSE)
    #
    # # AE$AESTDTC[1] <- "2016-01-03"
    # # AE$USUBJID[1] <- AE$USUBJID[5]
    #
    # expect_error(check_ae_aestdtc_after_dd(AE, DS))

})

test_that("function returns true when no errors are present", {

    AE <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
                     AEDTHDTC = c(rep("", 4), "2016-01-01"),
                     AESTDTC = rep("2016-01-01", 5),
                     AEDECOD = LETTERS[1:5], AETERM = LETTERS[1:5],
                     stringsAsFactors = FALSE)

    DS <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
                     DSSTDTC = rep("2016-01-02", 5),
                     DSDECOD = c(LETTERS[1:4], "death"),
                     DSTERM = letters[1:5],
                     stringsAsFactors = FALSE)


    expect_true(check_ae_aestdtc_after_dd(AE, DS))

})

test_that("function returns false when errors are present", {

    AE <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
                     AEDTHDTC = c(rep("", 4), "2016-01-01"),
                     AESTDTC = rep("2016-01-01", 5),
                     AEDECOD = LETTERS[1:5], AETERM = LETTERS[1:5],
                     stringsAsFactors = FALSE)

    DS <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
                     DSSTDTC = rep("2016-01-02", 5),
                     DSDECOD = c(LETTERS[1:4], "death"),
                     DSTERM = letters[1:5],
                     stringsAsFactors = FALSE)

    check_ae_aestdtc_after_dd(AE, DS)

    AE$AESTDTC[1] <- "2016-01-03"
    AE$USUBJID[1] <- AE$USUBJID[5]

    expect_false(check_ae_aestdtc_after_dd(AE, DS))

})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {


    AE <- data.frame(STUDYID = integer(),
                     USUBJID = character(),
                     AEDTHDTC = character(),
                     AESTDTC = character(),
                     AEDECOD = character(),
                     AETERM = character(),
                     stringsAsFactors = FALSE)

    DS <- data.frame(STUDYID = integer(),
                     USUBJID = character(),
                     DSSTDTC = character(),
                     DSDECOD = character(),
                     DSTERM = character(),
                     stringsAsFactors = FALSE)


    expect_true(check_ae_aestdtc_after_dd(AE, DS))

})

test_that("Function returns true when no errors are present for a single input (one row)", {

    AE <- data.frame(STUDYID = 1, USUBJID = LETTERS[1],
                     AEDTHDTC = "2016-01-01",
                     AESTDTC = "2016-01-01",
                     AEDECOD = LETTERS[1], AETERM = LETTERS[1],
                     stringsAsFactors = FALSE)

    DS <- data.frame(STUDYID = 1, USUBJID = LETTERS[1],
                     DSSTDTC = "2016-01-02",
                     DSDECOD = LETTERS[1],
                     DSTERM = letters[1],
                     stringsAsFactors = FALSE)


    expect_true(check_ae_aestdtc_after_dd(AE, DS))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    AE <- data.frame(STUDYID = 1, USUBJID = LETTERS[1],
                     AEDTHDTC = "2016-01-01",
                     AESTDTC = "2016-01-03",
                     AEDECOD = LETTERS[1], AETERM = LETTERS[1],
                     stringsAsFactors = FALSE)

    DS <- data.frame(STUDYID = 1, USUBJID = LETTERS[1],
                     DSSTDTC = "2016-01-02",
                     DSDECOD = LETTERS[1],
                     DSTERM = letters[1],
                     stringsAsFactors = FALSE)



    expect_false(check_ae_aestdtc_after_dd(AE, DS))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     STUDYID = 1:900,
                     AETERM = rep(x = c("UPPER RESPIRATORY INFECTION","HEADACHE", "WORSENING THROMBOCYTOPENIA","LOW POTASSIUM","ANEMIA", "TREMORS"), times = 150),
                     AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
                                         "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
                     AESTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     AEDTHDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     stringsAsFactors = FALSE)

    DS <- data.frame(STUDYID = 1:900,
                     USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     DSSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     DSDECOD = c(rep("ALIVE", 300), rep("COMPLETED", 300), rep("ENROLLMENT", 300)),
                     DSTERM = c(rep("ALIVE", 300), rep("COMPLETED", 300), rep("ENROLLMENT", 300)),
                     stringsAsFactors = FALSE)

    expect_true(check_ae_aestdtc_after_dd(AE, DS))

})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     STUDYID = 1:900,
                     AETERM = rep(x = c("UPPER RESPIRATORY INFECTION","HEADACHE", "WORSENING THROMBOCYTOPENIA","LOW POTASSIUM","ANEMIA", "TREMORS"), times = 150),
                     AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
                                         "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
                     AESTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     AEDTHDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     stringsAsFactors = FALSE)

    DS <- data.frame(STUDYID = 1:900,
                     USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     DSSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     DSDECOD = c(rep("ALIVE", 300), rep("COMPLETED", 300), rep("ENROLLMENT", 300)),
                     DSTERM = c(rep("ALIVE", 300), rep("COMPLETED", 300), rep("ENROLLMENT", 300)),
                     stringsAsFactors = FALSE)


    AE$AESTDTC[1] <- "2011-01-03"

    expect_false(check_ae_aestdtc_after_dd(AE, DS))

})

test_that("Function returns the failed object in attr(data)", {

    AE <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
                     AEDTHDTC = c(rep("", 4), "2016-01-01"),
                     AESTDTC = rep("2016-01-01", 5),
                     AEDECOD = LETTERS[1:5], AETERM = LETTERS[1:5],
                     stringsAsFactors = FALSE)

    DS <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
                     DSSTDTC = rep("2016-01-02", 5),
                     DSDECOD = c(LETTERS[1:4], "death"),
                     DSTERM = letters[1:5],
                     stringsAsFactors = FALSE)

    check_ae_aestdtc_after_dd(AE, DS)

    AE$AESTDTC[1] <- "2016-01-03"
    AE$USUBJID[1] <- AE$USUBJID[5]

    check <- check_ae_aestdtc_after_dd(AE, DS)

    expect_true(!is.null(attr(check, "data")))
    # expect_equal(attr(check, "data"), filter(AE, as.Date(AESTDTC) >= as.Date(AEENDTC)))

})
