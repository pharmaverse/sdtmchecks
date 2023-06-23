test_that("function errors when given bad input", {
    #expect_error(check_dd_ae_aedthdtc_ds_dsstdtc(list()))

    # AE <- data.frame(
    #     STUDYID = rep(1, 3),
    #     USUBJID = 1:4,
    #     AEDTHDTC = 1:3
    # )
    #
    # DS <- data.frame(
    #     STUDYID = rep(1, 3),
    #     USUBJID = 1:4,
    #     DSDECOD = rep("DEATH", 3),
    #     DSSTDTC = 1:3,
    #     stringsAsFactors = FALSE
    # )

    expect_error(check_dd_ae_aedthdtc_ds_dsstdtc(data.frame(
        STUDYID = rep(1, 3),
        USUBJID = 1:4,
        AEDTHDTC = 1:3
    ), data.frame(
        STUDYID = rep(1, 3),
        USUBJID = 1:4,
        DSDECOD = rep("DEATH", 3),
        DSSTDTC = 1:3,
        stringsAsFactors = FALSE
    )))

})

test_that("function returns true when no errors are present", {

    AE <- data.frame(
        STUDYID = rep(1, 3),
        USUBJID = 1:3,
        AEDTHDTC = 1:3
    )

    DS <- data.frame(
        STUDYID = rep(1, 3),
        USUBJID = 1:3,
        DSDECOD = rep("DEATH", 3),
        DSSTDTC = 1:3,
        stringsAsFactors = FALSE
    )

    expect_true(check_dd_ae_aedthdtc_ds_dsstdtc(AE, DS))

})

test_that("function returns false when errors are present", {

    AE <- data.frame(
        STUDYID = rep(1, 3),
        USUBJID = 1:3,
        AEDTHDTC = 1:3
    )

    DS <- data.frame(
        STUDYID = rep(1, 3),
        USUBJID = 1:3,
        DSDECOD = rep("DEATH", 3),
        DSSTDTC = 1:3,
        stringsAsFactors = FALSE
    )

    DS[3, "DSSTDTC"] <- 4

    expect_false(check_dd_ae_aedthdtc_ds_dsstdtc(AE, DS))


})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    AE <- data.frame(
        STUDYID = integer(),
        USUBJID = integer(),
        AEDTHDTC = integer()
    )

    DS <- data.frame(
        STUDYID = integer(),
        USUBJID = integer(),
        DSDECOD = character(),
        DSSTDTC = integer(),
        stringsAsFactors = FALSE
    )

    expect_true(check_dd_ae_aedthdtc_ds_dsstdtc(AE, DS))
}) 

test_that("Function returns true when no errors are present for a single input (one row)", {

    AE <- data.frame(
        STUDYID = 1,
        USUBJID = 1,
        AEDTHDTC = 1
    )

    DS <- data.frame(
        STUDYID = 1,
        USUBJID = 1,
        DSDECOD = "DEATH",
        DSSTDTC = 1,
        stringsAsFactors = FALSE
    )

    expect_true(check_dd_ae_aedthdtc_ds_dsstdtc(AE, DS))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    AE <- data.frame(
        STUDYID = 1,
        USUBJID = 1,
        AEDTHDTC = 1
    )

    DS <- data.frame(
        STUDYID = 1,
        USUBJID = 1,
        DSDECOD = "DEATH",
        DSSTDTC = 1,
        stringsAsFactors = FALSE
    )

    DS[1, "DSSTDTC"] <- 4

    expect_false(check_dd_ae_aedthdtc_ds_dsstdtc(AE, DS))

})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     STUDYID = 1,
                     AEDTHDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     stringsAsFactors = FALSE)

    DS <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     STUDYID = 1,
                     DSDECOD = "DEATH",
                     DSSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     stringsAsFactors = FALSE
    )

    expect_true(check_dd_ae_aedthdtc_ds_dsstdtc(AE, DS))
})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     STUDYID = 1,
                     AEDTHDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     stringsAsFactors = FALSE)

    DS <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     STUDYID = 1,
                     DSDECOD = "DEATH",
                     DSSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     stringsAsFactors = FALSE
    )

    DS[1, "DSSTDTC"] <- 4

    expect_false(check_dd_ae_aedthdtc_ds_dsstdtc(AE, DS))
})

test_that("Function returns the failed object in attr(data)", {

    AE <- data.frame(
        STUDYID = rep(1, 3),
        USUBJID = 1:3,
        AEDTHDTC = 1:3
    )

    DS <- data.frame(
        STUDYID = rep(1, 3),
        USUBJID = 1:3,
        DSDECOD = rep("DEATH", 3),
        DSSTDTC = 1:3,
        stringsAsFactors = FALSE
    )

    DS[3, "DSSTDTC"] <- 4

    check <- check_dd_ae_aedthdtc_ds_dsstdtc(AE, DS)

    expect_true(!is.null(attr(check, "data")))
    # expect_equal(attr(check, "data"), filter(AE, !is.na(AEDTHDTC) & AESDTH != "Y"))

})


