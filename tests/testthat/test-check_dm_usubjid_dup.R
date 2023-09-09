test_that("function errors when given bad input", {
    #expect_error(check_dm_usubjid_dup(list()))
    expect_error(check_dm_usubjid_dup(data.frame(USUBJID = c(LETTERS[1:4], "A"),
                                                 AGE = 1:6, stringsAsFactors = FALSE)))

})

test_that("function returns true when no errors are present", {

    DM <- data.frame(USUBJID = c(LETTERS[1:4]),
                     AGE = 1:4,
                     stringsAsFactors = FALSE)

    expect_true(check_dm_usubjid_dup(DM))

})

test_that("function returns false when errors are present", {

    DM <- data.frame(USUBJID = c(LETTERS[1:4], "A"),
                     AGE = 1:5, stringsAsFactors = FALSE)

    expect_false(check_dm_usubjid_dup(DM))

    #remove USUBJID
    DM2 <- select(DM, -USUBJID)

    expect_false(check_dm_usubjid_dup(DM2))


})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    DM <- data.frame(USUBJID = character(),
                     AGE = integer(),
                     stringsAsFactors=FALSE)

    expect_true(check_dm_usubjid_dup(DM))
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
#     expect_false(check_dm_usubjid_dup(DM))
# })

test_that("Function returns true when no errors are present for a single input (one row)", {

    DM <- data.frame(USUBJID = c(LETTERS[1]),
                     AGE = 1,
                     stringsAsFactors = FALSE)


    expect_true(check_dm_usubjid_dup(DM))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    DM <- data.frame(USUBJID = c(LETTERS[1], "A"),
                     AGE = 1:2,
                     stringsAsFactors = FALSE)

    expect_false(check_dm_usubjid_dup(DM))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    DM <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     AGE = 60,
                     stringsAsFactors = FALSE)

    expect_true(check_dm_usubjid_dup(DM))
})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    DM <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     AGE = 60,
                     stringsAsFactors = FALSE)

     DM$USUBJID[2] <- "MYSTUDY-S0001-1100"
     
     DM

    expect_false(check_dm_usubjid_dup(DM))

})


test_that("Function returns false when errors are present for subject id across sites", {

    DM <- data.frame(USUBJID = c("GO12345-00000-1000",
                                           "GO12345-11111-1001",
                                           "GO12345-22222-1000",
                                           "GO12345-33333-1000"),
                               stringsAsFactors = FALSE)

    expect_false(check_dm_usubjid_dup(DM))

})

test_that("Function returns the failed object in attr(data)", {

    DM <- data.frame(USUBJID = c(LETTERS[1:4], "A"),
                     AGE = 1:5, stringsAsFactors = FALSE)


    check <- check_dm_usubjid_dup(DM)

    expect_true(!is.null(attr(check, "data")))
    # expect_equal(attr(check, "data"), DM %>% group_by(USUBJID) %>% filter(duplicated(USUBJID)))

})


