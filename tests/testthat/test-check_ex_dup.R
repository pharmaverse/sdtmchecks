test_that("function errors when given bad input", {
    #expect_error(check_ex_dup(list()))

    # EX <- data.frame(
    #     USUBJID = 1:3,
    #     EXTRT = 1:2,
    #     EXDOSE = 1:2,
    #     EXSTDTC = 1:2,
    #     EXOCCUR = "Y"
    # )

    expect_error(check_ex_dup(data.frame(
        USUBJID = 1:3,
        EXTRT = 1:2,
        EXDOSE = 1:2,
        EXSTDTC = 1:2,
        EXOCCUR = "Y"
    )))

})

test_that("function returns true when no errors are present", {

    EX <- data.frame(
        USUBJID = 1:2,
        EXTRT = 1:2,
        EXDOSE = 1:2,
        EXSTDTC = 1:2,
        EXOCCUR = "Y"
    )

    expect_true(check_ex_dup(EX))

})

test_that("function returns false when errors are present", {

    EX <- data.frame(
        USUBJID = rep(1,2),
        EXTRT = rep(1,2),
        EXDOSE = rep(1,2),
        EXSTDTC = rep(1,2),
        EXOCCUR = "Y"
    )

    expect_false(check_ex_dup(EX))

})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {


    EX <- data.frame(
        USUBJID = integer(),
        EXTRT = integer(),
        EXDOSE = integer(),
        EXSTDTC = integer(),
        EXOCCUR = character()
    )


    expect_true(check_ex_dup(EX))
})

# test_that("Function returns false when errors are present for an empty dataframe (zero rows)", {
#
#     EX <- data.frame(USUBJID =NA,
#                      EXSEQ=NA,
#                      EXSTDTC=NA,
#                      EXTERM="",
#                      EXDECOD ="NA",
#                      stringsAsFactors=FALSE)
#
#     expect_false(check_ex_dup(EX))
# })

test_that("Function returns true when no errors are present for a single input (one row)", {

    EX <- data.frame(
        USUBJID = 1,
        EXTRT = 1,
        EXDOSE = 1,
        EXSTDTC = 1,
        EXOCCUR = "Y"
    )
    expect_true(check_ex_dup(EX))
})

# test_that("Function returns false when errors are present for a single input (one row)", {
#
#     EX <- data.frame(
#         USUBJID = c("ID1","ID1"),
#         EXDECOD = c("RANDOMIZATION","RANDOMIZATION")
#         , stringsAsFactors = FALSE
#     )
#
#     expect_false(check_ex_dup(EX))
# })

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {


    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    EX <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     EXTRT = c("DRUG1", "DRUG2"),
                     EXDOSE = rep(x = c("800", "1200", "80"), times = c(300, 300, 300)),
                     EXSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     EXOCCUR = "Y",
                     stringsAsFactors = FALSE)


    expect_true(check_ex_dup(EX))
})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1),2)

    EX <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     EXTRT = c("DRUG1A", "DRUG1A"),
                     EXDOSE = rep(x = c("800", "1200", "80"), times = c(300, 300, 300)),
                     EXSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     EXOCCUR = "Y",
                     stringsAsFactors = FALSE)

    EX$EXSTDTC[2] <- "2011-01-01"

    expect_false(check_ex_dup(EX))

})

test_that("Function returns the failed object in attr(data)", {

    EX <- data.frame(
        USUBJID = rep(1,2),
        EXTRT = rep(1,2),
        EXDOSE = rep(1,2),
        EXSTDTC = rep(1,2),
        EXOCCUR = "Y"
    )


    check <- check_ex_dup(EX)

    expect_true(!is.null(attr(check, "data")))
    expect_equal(attr(check, "data"), select(filter(EX, duplicated(USUBJID)), -EXOCCUR))

})




