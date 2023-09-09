test_that("function errors when given bad input", {
    #expect_error(check_dd_death_date(list()))

    # AE <- data.frame(
    #     USUBJID = 1:4,
    #     AEDTHDTC = 1:5
    # )
    #
    # DS <- data.frame(
    #     USUBJID = 1:5,
    #     DSTERM = c("DEATH DUE TO ADVERSE EVENT","DEATH DUE TO PROGRESSIVE DISEASE",
    #                "DEATH DUE TO ADVERSE EVENT","DEATH DUE TO ADVERSE EVENT")
    #     ,
    #     DSDECOD = rep("DEATH",4),
    #     DSSTDTC = 1:4
    # )

    expect_error(check_dd_death_date(data.frame(
        USUBJID = 1:4,
        AEDTHDTC = 1:5
    ), data.frame(
        USUBJID = 1:5,
        DSTERM = c("DEATH DUE TO ADVERSE EVENT","DEATH DUE TO PROGRESSIVE DISEASE",
                   "DEATH DUE TO ADVERSE EVENT","DEATH DUE TO ADVERSE EVENT")
        ,
        DSDECOD = rep("DEATH",4),
        DSSTDTC = 1:4
    )))

})

test_that("function returns true when no errors are present", {

    AE <- data.frame(
        USUBJID = 1:4,
        AEDTHDTC = 1:4
    )

    DS <- data.frame(
        USUBJID = 1:4,
        DSTERM = c("DEATH DUE TO ADVERSE EVENT","DEATH DUE TO ADVERSE EVENT",
                   "DEATH DUE TO ADVERSE EVENT","DEATH DUE TO ADVERSE EVENT")
        ,
        DSDECOD = rep("DEATH",4),
        DSSTDTC = 1:4
    )

    expect_true(check_dd_death_date(AE, DS))

})

test_that("function returns false when errors are present", {

    AE <- data.frame(
        USUBJID = 1:4,
        AEDTHDTC = 1:4
    )

    DS <- data.frame(
        USUBJID = 1:4,
        DSTERM = c("DEATH DUE TO ADVERSE EVENT","DEATH DUE TO PROGRESSIVE DISEASE",
                   "DEATH DUE TO ADVERSE EVENT","DEATH DUE TO ADVERSE EVENT")
        ,
        DSDECOD = rep("DEATH",4),
        DSSTDTC = 1:4
    )


    expect_false(check_dd_death_date(AE, DS))

})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    AE <- data.frame(
        USUBJID = integer(),
        AEDTHDTC = integer()
    )

    DS <- data.frame(
        USUBJID = integer(),
        DSTERM = character(),
        DSDECOD = character(),
        DSSTDTC = integer()
    )


    expect_true(check_dd_death_date(AE, DS))
})

# test_that("Function returns false when errors are present for an empty dataframe (zero rows)", {
# 
#     DS <- data.frame(USUBJID =NA,
#                      DSSEQ=NA,
#                      DSSTDTC=NA,
#                      DSTERM="",
#                      DSDECOD ="NA",
#                      stringsAsFactors=FALSE)
# 
#     expect_false(check_dd_death_date(DS))
# })

test_that("Function returns true when no errors are present for a single input (one row)", {

    AE <- data.frame(
        USUBJID = 1,
        AEDTHDTC = 1
    )

    DS <- data.frame(
        USUBJID = 1,
        DSTERM = c("DEATH DUE TO ADVERSE EVENT"),
        DSDECOD = "DEATH",
        DSSTDTC = 1
    )

    expect_true(check_dd_death_date(AE, DS))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    AE <- data.frame(
        USUBJID = 1,
        AEDTHDTC = 1
    )

    DS <- data.frame(
        USUBJID = 1:4,
        DSTERM = c("DEATH DUE TO PROGRESSIVE DISEASE")
        ,
        DSDECOD = "DEATH",
        DSSTDTC = 1
    )

    expect_false(check_dd_death_date(AE, DS))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {


    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     AEDTHDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     stringsAsFactors = FALSE)

    DS <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     STUDYID = 1,
                     DSTERM = c("DEATH DUE TO ADVERSE EVENT"),
                     DSDECOD = "DEATH",
                     DSSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     stringsAsFactors = FALSE)


    expect_true(check_dd_death_date(AE, DS))
})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     AEDTHDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     stringsAsFactors = FALSE)

    DS <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     STUDYID = 1,
                     DSTERM = c("DEATH DUE TO PROGRESSIVE DISEASE"),
                     DSDECOD = "DEATH",
                     DSSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     stringsAsFactors = FALSE)

    expect_false(check_dd_death_date(AE, DS))

})

test_that("Function returns the failed object in attr(data)", {

    AE <- data.frame(
        USUBJID = 1:4,
        AEDTHDTC = 1:4
    )

    DS <- data.frame(
        USUBJID = 1:4,
        DSTERM = c("DEATH DUE TO ADVERSE EVENT","DEATH DUE TO PROGRESSIVE DISEASE",
                   "DEATH DUE TO ADVERSE EVENT","DEATH DUE TO ADVERSE EVENT")
        ,
        DSDECOD = rep("DEATH",4),
        DSSTDTC = 1:4
    )


    check <- check_dd_death_date(AE, DS)

    expect_true(!is.null(attr(check, "data")))
    # expect_equal(attr(check, "data"), ))

})


