test_that("function errors when given bad input", {
    #expect_error(check_dm_armcd(list()))
    expect_error(check_dm_armcd(data.frame(USUBJID = 1:4,
                                           ARM = 1:3,
                                           ARMCD = 1:3
    )))

})

test_that("function returns true when no errors are present", {

    DM <- data.frame(
        USUBJID = 1:3,
        ARM = 1:3,
        ARMCD = 1:3
    )

    expect_true(check_dm_armcd(DM))

})

test_that("function returns false when errors are present", {

    DM <- data.frame(
        USUBJID = 1:3,
        ARM = 1:3,
        ARMCD = 1:3
    )

    DM$ARMCD[1] <- NA

    expect_false(check_dm_armcd(DM))

})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    DM <- data.frame(USUBJID =integer(),
                     ARM =integer(),
                     ARMCD = integer())

    expect_true(check_dm_armcd(DM))
})

# test_that("Function returns false when errors are present for an empty dataframe (zero rows)", {
# 
#     DM <- data.frame(USUBJID =NA,
#                      DMSEQ=NA,
#                      DMSTDTC=NA,
#                      DMTERM="",
#                      DMDECOD ="NA",
#                      stringsAsFactors=FALSE)
# 
#     expect_false(check_dm_armcd(DM))
# })

test_that("Function returns true when no errors are present for a single input (one row)", {

    DM <- data.frame(
        USUBJID = 1,
        ARM = 1,
        ARMCD = 1
    )

    expect_true(check_dm_armcd(DM))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    DM <- data.frame(
        USUBJID = 1,
        ARM = NA,
        ARMCD = 1
    )

    expect_false(check_dm_armcd(DM))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    DM <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     # DMSEQ = rep(x = seq(1, 60, by=1), times = 900),
                     ARM = rep(c("ARM A COHORT", "ARM B COHORT"), times = c(450, 450)),
                     ARMCD = rep(c("ACOHORT1", "BCOHORT1"), times = c(450, 450)),
                     stringsAsFactors = FALSE)

    expect_true(check_dm_armcd(DM))
})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    DM <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     # DMSEQ = rep(x = seq(1, 60, by=1), times = 900),
                     ARM = rep(c("ARM A COHORT", "ARM B COHORT"), times = c(450, 450)),
                     ARMCD = rep(c("ACOHORT1", "BCOHORT1", NA), times = c(300, 300, 300)),
                     stringsAsFactors = FALSE)

    expect_false(check_dm_armcd(DM))
})

test_that("Function returns the failed object in attr(data)", {

    DM <- data.frame(
        USUBJID = 1,
        ARM = NA,
        ARMCD = 1
    )

    check <- check_dm_armcd(DM)

    expect_true(!is.null(attr(check, "data")))
    # expect_equal(attr(check, "data"), filter(DM, !is.na(DMDTHDTC) & DMSDTH != "Y"))

})


