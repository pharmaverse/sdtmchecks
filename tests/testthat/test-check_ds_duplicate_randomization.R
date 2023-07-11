test_that("function errors when given bad input", {
    #expect_error(check_ds_duplicate_randomization(list()))

    # DS <- data.frame(
    #     USUBJID = c("ID1","ID1","ID2","ID2","ID3","ID3", "ID4", "ID4"),
    #     DSDECOD = c("RANDOMIZATION","OTHER THING","RANDOMIZATION",
    #                 "OTHER THING","RANDOMIZATION","RANDOMIZATION")
    #     , stringsAsFactors = FALSE
    # )


    expect_error(check_ds_duplicate_randomization(data.frame(
        USUBJID = c("ID1","ID1","ID2","ID2","ID3","ID3", "ID4", "ID4"),
        DSDECOD = c("RANDOMIZATION","OTHER THING","RANDOMIZATION",
                    "OTHER THING","RANDOMIZATION","RANDOMIZATION")
        , stringsAsFactors = FALSE
    )))

})

test_that("function returns true when no errors are present", {

    DS <- data.frame(
        USUBJID = c("ID1","ID1","ID2","ID2","ID3","ID3"),
        DSDECOD = c("RANDOMIZATION","OTHER THING","RANDOMIZATION",
                    "OTHER THING","RANDOMIZATION","OTHER THING")
        , stringsAsFactors = FALSE)

    expect_true(check_ds_duplicate_randomization(DS))

})

test_that("function returns false when errors are present", {

    DS <- data.frame(
        USUBJID = c("ID1","ID1","ID2","ID2","ID3","ID3"),
        DSDECOD = c("RANDOMIZATION","OTHER THING","RANDOMIZATION",
                    "OTHER THING","RANDOMIZATION","RANDOMIZATION")
        , stringsAsFactors = FALSE)

    expect_false(check_ds_duplicate_randomization(DS))

})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {


    DS <- data.frame(
        USUBJID = character(),
        DSDECOD = character(),
        stringsAsFactors = FALSE)

    expect_true(check_ds_duplicate_randomization(DS))
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
#     expect_false(check_ds_duplicate_randomization(DS))
# })

test_that("Function returns true when no errors are present for a single input (one row)", {

    DS <- data.frame(
        USUBJID = c("ID1","ID1"),
        DSDECOD = c("RANDOMIZATION","OTHER THING")
        , stringsAsFactors = FALSE
    )
    expect_true(check_ds_duplicate_randomization(DS))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    DS <- data.frame(
        USUBJID = c("ID1","ID1"),
        DSDECOD = c("RANDOMIZATION","RANDOMIZATION")
        , stringsAsFactors = FALSE
    )

    expect_false(check_ds_duplicate_randomization(DS))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {


    USUBJID <- rep(rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1),2)

    DS <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     DSDECOD = c("RANDOMIZATION","OTHER THING"),
                     stringsAsFactors = FALSE)


    expect_true(check_ds_duplicate_randomization(DS))
})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1),2)

    DS <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     DSDECOD = c("RANDOMIZATION","RANDOMIZATION"),
                     stringsAsFactors = FALSE)

    expect_false(check_ds_duplicate_randomization(DS))

})

test_that("Function returns the failed object in attr(data)", {

    DS <- data.frame(
        USUBJID = c("ID1","ID1","ID2","ID2","ID3","ID3"),
        DSDECOD = c("RANDOMIZATION","OTHER THING","RANDOMIZATION",
                    "OTHER THING","RANDOMIZATION","RANDOMIZATION")
        , stringsAsFactors = FALSE)


    check <- check_ds_duplicate_randomization(DS)

    expect_true(!is.null(attr(check, "data")))
    # expect_equal(attr(check, "data"), ))

})


