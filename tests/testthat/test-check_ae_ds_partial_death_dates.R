test_that("function errors when given bad input", {
    expect_error(check_ae_ds_partial_death_dates(list()))

    # AE <- data.frame(
    #  USUBJID = 1:4,
    #  AEDECOD = c("AE1","AE2","AE3"),
    #  AEDTHDTC = c("2017-01-01","2017",NA),
    #  stringsAsFactors=FALSE
    # )
    #
    # DS <- data.frame(
    #  USUBJID = 1:5,
    #  DSSCAT = "STUDY DISCON",
    #  DSDECOD = "DEATH",
    #  DSSTDTC = c("2017-01-01","2017","2017-01-02","2016-10"),
    #  stringsAsFactors=FALSE
    # )


    expect_error(check_ae_ds_partial_death_dates(data.frame(
        USUBJID = 1:4,
        AEDECOD = c("AE1","AE2","AE3"),
        AEDTHDTC = c("2017-01-01","2017",NA),
        stringsAsFactors=FALSE
    ),data.frame(
        USUBJID = 1:5,
        DSSCAT = "STUDY DISCON",
        DSDECOD = "DEATH",
        DSSTDTC = c("2017-01-01","2017","2017-01-02","2016-10"),
        stringsAsFactors=FALSE
    )))

})

test_that("function returns true when no errors are present", {

    AE <- data.frame(
     USUBJID = 1:3,
     AEDECOD = c("AE1","AE2","AE3"),
     AEDTHDTC = c("2017-01-01","2017-01-02","2017-01-03"),
     stringsAsFactors=FALSE
    )

    DS <- data.frame(
     USUBJID = 1:4,
     DSSCAT = "STUDY DISCON",
     DSDECOD = "DEATH",
     DSSTDTC = c("2017-01-01","2017-01-01","2017-01-02","2016-10-01"),
     stringsAsFactors=FALSE
    )

    expect_true(check_ae_ds_partial_death_dates(AE, DS))

})

test_that("function returns false when errors are present", {

    AE <- data.frame(
     USUBJID = 1:3,
     AEDECOD = c("AE1","AE2","AE3"),
     AEDTHDTC = c("2017-01-01","2017",NA),
     stringsAsFactors=FALSE
    )

    DS <- data.frame(
     USUBJID = 1:4,
     DSSCAT = "STUDY DISCON",
     DSDECOD = "DEATH",
     DSSTDTC = c("2017-01-01","2017","2017-01-02","2016-10"),
     stringsAsFactors=FALSE
    )

    expect_false(check_ae_ds_partial_death_dates(AE, DS))


})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    AE <- data.frame(
     USUBJID = integer(),
     AEDECOD = character(),
     AEDTHDTC = character(),
     stringsAsFactors=FALSE
    )

    DS <- data.frame(
     USUBJID = integer(),
     DSSCAT = character(),
     DSDECOD = character(),
     DSSTDTC = character(),
     stringsAsFactors=FALSE
    )

    expect_true(check_ae_ds_partial_death_dates(AE, DS))
})
 

test_that("Function returns true when no errors are present for a single input (one row)", {

    AE <- data.frame(
     USUBJID = 1,
     AEDECOD = c("AE1"),
     AEDTHDTC = c("2017-01-01"),
     stringsAsFactors=FALSE
    )

    DS <- data.frame(
     USUBJID = 1,
     DSSCAT = "STUDY DISCON",
     DSDECOD = "DEATH",
     DSSTDTC = c("2017-01-01"),
     stringsAsFactors=FALSE
    )

    expect_true(check_ae_ds_partial_death_dates(AE, DS))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    AE <- data.frame(
        USUBJID = 1,
        AEDECOD = c("AE1"),
        AEDTHDTC = c("2017"),
        stringsAsFactors=FALSE
    )

    DS <- data.frame(
        USUBJID = 1,
        DSSCAT = "STUDY DISCON",
        DSDECOD = "DEATH",
        DSSTDTC = c("2017"),
        stringsAsFactors=FALSE
    )

    expect_false(check_ae_ds_partial_death_dates(AE, DS))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     AEDTHDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
                                         "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
                     # AESTDTC = rep(seq(as.Date('2010-01-01'), as.Date('2010-03-01'), by = 1), times = 15),
                     stringsAsFactors = FALSE)

    DS <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     DSSCAT = "STUDY DISCON",
                     DSDECOD = "DEATH",
                     DSSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     stringsAsFactors = FALSE
    )

    expect_true(check_ae_ds_partial_death_dates(AE, DS))
})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     AEDTHDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
                                         "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
                     # AESTDTC = rep(seq(as.Date('2010-01-01'), as.Date('2010-03-01'), by = 1), times = 15),
                     stringsAsFactors = FALSE)

    DS <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     DSSCAT = "STUDY DISCON",
                     DSDECOD = "DEATH",
                     DSSTDTC = as.character(rep('2011', times = 15)),
                     stringsAsFactors = FALSE
    )


    expect_false(check_ae_ds_partial_death_dates(AE, DS))
})

test_that("Function returns the failed object in attr(data)", {

    AE <- data.frame(
        USUBJID = 1:3,
        AEDECOD = c("AE1","AE2","AE3"),
        AEDTHDTC = c("2017-01-01","2017",NA),
        stringsAsFactors=FALSE
    )

    DS <- data.frame(
        USUBJID = 1:4,
        DSSCAT = "STUDY DISCON",
        DSDECOD = "DEATH",
        DSSTDTC = c("2017-01-01","2017","2017-01-02","2016-10"),
        stringsAsFactors=FALSE
    )

    check <- check_ae_ds_partial_death_dates(AE, DS)

    expect_true(!is.null(attr(check, "data")))
    # expect_equal(attr(check, "data"), filter(AE, !is.na(AEDTHDTC) & AESDTH != "Y"))

})


