test_that("function errors when given bad input", {
    #expect_error(check_ae_aeout(list()))
    expect_error(check_ae_aeout(data.frame(USUBJID = 1:6,
                                           AEDTHDTC = c(NA, "NA", "2022-01-05", "2012-05-01", "1999-01-22"),
                                           AEOUT = c("", "", "","FATAL","RECOVERED/RESOLVED"),
                                                     stringsAsFactors = FALSE)))

})

test_that("function returns true when no errors are present", {

    AE <- data.frame(
        USUBJID = 1:5,
        AEDTHDTC = c(NA, "NA", "2022-01-05", "2012-05-01", "1999-01-22"),
        AEOUT = c("", "", "FATAL","FATAL","FATAL"),
        stringsAsFactors = FALSE
    )

    expect_true(check_ae_aeout(AE))

})

test_that("function returns false when errors are present", {

    AE <- data.frame(
        USUBJID = 1:5,
        AEDTHDTC = c(NA, "NA", "2022-01-05", "2012-05-01", "1999-01-22"),
        AEOUT = c("", "", "","FATAL","RECOVERED/RESOLVED"),
        stringsAsFactors = FALSE
    )

    expect_false(check_ae_aeout(AE))


})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    AE <- data.frame(USUBJID =integer(),
                     AEDTHDTC=character(),
                     AEOUT=character(),
                     stringsAsFactors=FALSE)

    expect_true(check_ae_aeout(AE))
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
#     expect_false(check_ae_aeout(AE))
# })

test_that("Function returns true when no errors are present for a single input (one row)", {

    AE <- data.frame(
        USUBJID = 1,
        AEDTHDTC = c("2022-01-01"),
        AEOUT = c("FATAL"),
        stringsAsFactors = FALSE
    )

    expect_true(check_ae_aeout(AE))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    AE <- data.frame(
        USUBJID = 1,
        AEDTHDTC = c("2022-01-01"),
        AEOUT = c("RECOVERED/RESOLVED"),
        stringsAsFactors = FALSE
    )

    expect_false(check_ae_aeout(AE))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     # AESEQ = rep(x = seq(1, 60, by=1), times = 900),
                     AEDTHDTC = rep(seq(as.Date('2023-01-01'), as.Date('2023-03-01'), by = 1), times = 15),
                     AEOUT = rep("FATAL", times = 900),
                     stringsAsFactors = FALSE)

    expect_true(check_ae_aeout(AE))
})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     # AESEQ = rep(x = seq(1, 60, by=1), times = 900),
                     AEDTHDTC = rep(seq(as.Date('2023-01-01'), as.Date('2023-03-01'), by = 1), times = 15),
                     AEOUT = rep("RECOVERED/RESOLVED", times = 900),
                     stringsAsFactors = FALSE)


    expect_false(check_ae_aeout(AE))
})

test_that("Function returns the failed object in attr(data)", {

    AE <- data.frame(
        USUBJID = 1,
        AEDTHDTC = c("2023-03-12"),
        AEOUT = c("RECOVERED/RESOLVED"),
        stringsAsFactors = FALSE
    )

    check <- check_ae_aeout(AE)

    expect_true(!is.null(attr(check, "data")))
    # expect_equal(attr(check, "data"), filter(AE, !is.na(AEDTHDTC) & AESDTH != "Y"))

})


