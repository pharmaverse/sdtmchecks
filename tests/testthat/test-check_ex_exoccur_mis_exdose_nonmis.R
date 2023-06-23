test_that("function errors when given bad input", {
    #expect_error(check_ex_exoccur_mis_exdose_nonmis(list()))

    # EX <- data.frame(
    #     USUBJID = 1:11,
    #     EXTRT = rep(1,10),
    #     EXOCCUR = c(rep(1,2),rep(NA,4),rep(2,4)),
    #     EXDOSE = c(rep(NA,4),rep(1,6)),
    #     EXSTDTC = 1:10
    # )

    expect_error(check_ex_exoccur_mis_exdose_nonmis(data.frame(
        USUBJID = 1:11,
        EXTRT = rep(1,10),
        EXOCCUR = c(rep(1,2),rep(NA,4),rep(2,4)),
        EXDOSE = c(rep(NA,4),rep(1,6)),
        EXSTDTC = 1:10
    )))

})

test_that("function returns true when no errors are present", {

    EX <- data.frame(
        USUBJID = 1:10,
        EXTRT = rep(1,10),
        EXOCCUR = c(rep(1,2),rep(2,4),rep(3,4)),
        EXDOSE = c(rep(1,4),rep(2,6)),
        EXSTDTC = 1:10
    )

    expect_true(check_ex_exoccur_mis_exdose_nonmis(EX))

})

test_that("function returns false when errors are present", {

    EX <- data.frame(
        USUBJID = 1:10,
        EXTRT = rep(1,10),
        EXOCCUR = c(rep(1,2),rep(NA,4),rep(2,4)),
        EXDOSE = c(rep(NA,4),rep(1,6)),
        EXSTDTC = 1:10
    )


    expect_false(check_ex_exoccur_mis_exdose_nonmis(EX))

})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    EX <- data.frame(
        USUBJID = integer(),
        EXTRT = integer(),
        EXOCCUR = integer(),
        EXDOSE = integer(),
        EXSTDTC = integer()
    )



    expect_true(check_ex_exoccur_mis_exdose_nonmis(EX))
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

    EX <- data.frame(
        USUBJID = 1,
        EXTRT = rep(1,1),
        EXOCCUR = c(rep(1,1)),
        EXDOSE = c(rep(1,1)),
        EXSTDTC = 1
    )

    expect_true(check_ex_exoccur_mis_exdose_nonmis(EX))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    EX <- data.frame(
        USUBJID = 1,
        EXTRT = rep(1,1),
        EXOCCUR = c(rep(1,1)),
        EXDOSE = c(rep(1,1)),
        EXSTDTC = 1
    )

    EX$EXOCCUR[1]="NA"

    expect_false(check_ex_exoccur_mis_exdose_nonmis(EX))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {


    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    EX <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     # VISIT = paste0("Visit ", 1:900),
                     EXTRT = c("TRT A", "TRT B"),
                     EXDOSE = rep(x = c(800, 1200, 80), times = c(300, 300, 300)),
                     EXSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     # EXENDTC  = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     EXOCCUR = "Y",
                     stringsAsFactors = FALSE)


    expect_true(check_ex_exoccur_mis_exdose_nonmis(EX))
})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    EX <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     # VISIT = paste0("Visit ", 1:900),
                     EXTRT = c("TRT A", "TRT B"),
                     EXDOSE = rep(x = c(800, 1200, 80), times = c(300, 300, 300)),
                     EXSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     # EXENDTC  = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     EXOCCUR = "NA",
                     stringsAsFactors = FALSE)

    expect_false(check_ex_exoccur_mis_exdose_nonmis(EX))

})

test_that("Function returns the failed object in attr(data)", {

    EX <- data.frame(
        USUBJID = 1:10,
        EXTRT = rep(1,10),
        EXOCCUR = c(rep(1,2),rep(NA,4),rep(2,4)),
        EXDOSE = c(rep(NA,4),rep(1,6)),
        EXSTDTC = 1:10
    )


    check <- check_ex_exoccur_mis_exdose_nonmis(EX)

    expect_true(!is.null(attr(check, "data")))
    expect_equal(attr(check, "data"), filter(EX, EXDOSE >= 0 & (EXOCCUR == "Y" | is.na(EXOCCUR) | EXOCCUR == "NA")))

}) 
