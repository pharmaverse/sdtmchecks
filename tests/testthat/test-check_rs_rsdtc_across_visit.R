context("test-check_rs_rsdtc_across_visit")

test_that("function errors when given bad input", {
    #expect_error(check_rs_rsdtc_across_visit(list()))

    # RS <- data.frame(
    #     USUBJID = 1:11,
    #     RSDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
    #     VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
    #     stringsAsFactors=FALSE)

    expect_error(check_rs_rsdtc_across_visit(data.frame(
        USUBJID = 1:11,
        RSDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
        VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
        stringsAsFactors=FALSE)))

})

test_that("function returns true when no errors are present", {

    RS <- data.frame(
        USUBJID = 1,
        RSDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
        VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
        stringsAsFactors=FALSE)

    RS$RSDTC[7:8] <- "2016-01-02"

    expect_true(check_rs_rsdtc_across_visit(RS))

})

test_that("function returns false when errors are present", {

    RS <- data.frame(
        USUBJID = 1,
        RSDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
        VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
        stringsAsFactors=FALSE)

    expect_false(check_rs_rsdtc_across_visit(RS))

})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    RS <- data.frame(
        USUBJID = integer(),
        RSDTC = character(),
        VISIT = character(),
        stringsAsFactors=FALSE)

    expect_true(check_rs_rsdtc_across_visit(RS))

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
#     expect_false(check_ex_exdecod(EX))
# })

test_that("Function returns true when no errors are present for a single input (one row)", {

    RS <- data.frame(
        USUBJID = 1,
        RSDTC = "2016-01-01",
        VISIT = "C1D1",
        stringsAsFactors=FALSE)


    expect_true(check_rs_rsdtc_across_visit(RS))
})

# test_that("Function returns false when errors are present for a single input (one row)", {
# 
#     RS <- data.frame(
#         USUBJID = 1,
#         RSDTC = "2016-01-01",
#         VISIT = "",
#         stringsAsFactors=FALSE)
# 
#     expect_false(check_rs_rsdtc_across_visit(RS))
# })

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {


    RS <- data.frame(USUBJID = rep(rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 399)))), times = 1), 3),
                     RSDTC = as.character(rep(seq(as.Date('2011-04-01'), as.Date('2011-06-29'), by = 1), times = 10)),
                     VISIT = c(rep("C1D1",300), rep("C1D2",300), rep("C2D1",300)),
                     stringsAsFactors = FALSE)


    expect_true(check_rs_rsdtc_across_visit(RS))

})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    RS <- data.frame(USUBJID = rep(rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 399)))), times = 1), 3),
                     RSDTC = as.character(rep(seq(as.Date('2011-04-01'), as.Date('2011-04-30'), by = 1), times = 10)),
                     VISIT = c(rep("C1D1",300), rep("C1D2",300), rep("C2D1",300)),
                     stringsAsFactors = FALSE)


    expect_false(check_rs_rsdtc_across_visit(RS))

})

test_that("Function returns the failed object in attr(data)", {

    RS <- data.frame(
        USUBJID = 1,
        RSDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
        VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
        stringsAsFactors=FALSE)


    check <- check_rs_rsdtc_across_visit(RS)

    expect_true(!is.null(attr(check, "data")))
    # expect_equal(attr(check, "data"), select(filter(RS, ))

})
