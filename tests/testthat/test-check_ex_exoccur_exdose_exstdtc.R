test_that("function errors when given bad input", {
    #expect_error(check_ex_exoccur_exdose_exstdtc(list()))

    # EX <- data.frame(USUBJID = LETTERS[1:6],
    #                  VISIT = paste0("Visit ", 1:5),
    #                  VISITNUM = 1:5,
    #                  EXOCCUR = c('Y', rep('', 4)),
    #                  EXTRT = LETTERS[1:5],
    #                  EXDOSE = 1:5,
    #                  EXSTDTC = c('2010-01-01', rep('', 4)),
    #                  EXENDTC = c('2010-01-01', rep('', 4)),
    #                  stringsAsFactors = FALSE)

    expect_error(check_ex_exoccur_exdose_exstdtc(data.frame(USUBJID = LETTERS[1:6],
                                                            VISIT = paste0("Visit ", 1:5),
                                                            VISITNUM = 1:5,
                                                            EXOCCUR = c('Y', rep('', 4)),
                                                            EXTRT = LETTERS[1:5],
                                                            EXDOSE = 1:5,
                                                            EXSTDTC = c('2010-01-01', rep('', 4)),
                                                            EXENDTC = c('2010-01-01', rep('', 4)),
                                                            stringsAsFactors = FALSE)))

})

test_that("function returns true when no errors are present", {

    EX <- data.frame(USUBJID = LETTERS[1:5],
                     VISIT = paste0("Visit ", 1:5),
                     VISITNUM = 1:5,
                     EXOCCUR = c('Y', rep('', 4)),
                     EXTRT = LETTERS[1:5],
                     EXDOSE = 1:5,
                     EXSTDTC = c('2010-01-01', rep('', 4)),
                     EXENDTC = c('2010-01-01', rep('', 4)),
                     stringsAsFactors = FALSE)

    expect_true(check_ex_exoccur_exdose_exstdtc(EX))

})

test_that("function returns false when errors are present", {

    EX <- data.frame(USUBJID = LETTERS[1:5],
                     VISIT = paste0("Visit ", 1:5),
                     VISITNUM = 1:5,
                     EXOCCUR = c('Y', rep('', 4)),
                     EXTRT = LETTERS[1:5],
                     EXDOSE = 1:5,
                     EXSTDTC = c('2010-01-01', rep('', 4)),
                     EXENDTC = c('2010-01-01', rep('', 4)),
                     stringsAsFactors = FALSE)

    EX$EXOCCUR[2] <- 'Y'
    EX$EXSTDTC[2] <- '2011'
    EX$EXDOSE[1] <- 0


    expect_false(check_ex_exoccur_exdose_exstdtc(EX))

})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    EX <- data.frame(USUBJID = character(),
                     VISIT = character(),
                     VISITNUM = integer(),
                     EXOCCUR = character(),
                     EXTRT = character(),
                     EXDOSE = integer(),
                     EXSTDTC = character(),
                     EXENDTC = character(),
                     stringsAsFactors = FALSE)



    expect_true(check_ex_exoccur_exdose_exstdtc(EX))
})

test_that("Function returns false when errors are present for an empty dataframe (zero rows)", {

    EX <- data.frame(USUBJID =NA,
                     EXSEQ=NA,
                     EXSTDTC=NA,
                     EXTERM="",
                     EXOCCUR='Y',
                     EXDECOD ="NA",
                     stringsAsFactors=FALSE)

    expect_false(check_ex_exoccur_exdose_exstdtc(EX))
})

test_that("Function returns true when no errors are present for a single input (one row)", {

    EX <- data.frame(USUBJID = LETTERS[1],
                     VISIT = paste0("Visit ", 1),
                     VISITNUM = 1,
                     EXOCCUR = c('Y'),
                     EXTRT = LETTERS[1],
                     EXDOSE = 1,
                     EXSTDTC = c('2010-01-01'),
                     EXENDTC = c('2010-01-01'),
                     stringsAsFactors = FALSE)

    expect_true(check_ex_exoccur_exdose_exstdtc(EX))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    EX <- data.frame(USUBJID = LETTERS[1],
                     VISIT = paste0("Visit ", 1),
                     VISITNUM = 1,
                     EXOCCUR = c('Y'),
                     EXTRT = LETTERS[1],
                     EXDOSE = 1,
                     EXSTDTC = c('2010'),
                     EXENDTC = c('2010-01-01'),
                     stringsAsFactors = FALSE)


    EX$EXDOSE[1] <- 0

    expect_false(check_ex_exoccur_exdose_exstdtc(EX))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {


    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    EX <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     VISIT = paste0("Visit ", 1:900),
                     VISITNUM = 1,
                     EXTRT = c("TRT A", "TRT B"),
                     EXDOSE = rep(x = c(800, 1200, 80), times = c(300, 300, 300)),
                     EXSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     EXENDTC  = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     EXOCCUR = "N",
                     stringsAsFactors = FALSE)


    expect_true(check_ex_exoccur_exdose_exstdtc(EX))
})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    EX <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     VISIT = paste0("Visit ", 1:900),
                     EXTRT = c("TRT A", "TRT B"),
                     EXDOSE = rep(x = c(800, 1200, 80), times = c(300, 300, 300)),
                     EXSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     EXENDTC  = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     EXOCCUR = "Y",
                     stringsAsFactors = FALSE)

    expect_false(check_ex_exoccur_exdose_exstdtc(EX))

})

test_that("Function returns the failed object in attr(data)", {

    EX <- data.frame(USUBJID = LETTERS[1:5],
                     VISIT = paste0("Visit ", 1:5),
                     VISITNUM = 1:5,
                     EXOCCUR = c('Y', rep('', 4)),
                     EXTRT = LETTERS[1:5],
                     EXDOSE = 1:5,
                     EXSTDTC = c('2010-01-01', rep('', 4)),
                     EXENDTC = c('2010-01-01', rep('', 4)),
                     stringsAsFactors = FALSE)

    EX$EXOCCUR[2] <- 'Y'
    EX$EXSTDTC[2] <- '2011'
    EX$EXDOSE[1] <- 0

    check <- check_ex_exoccur_exdose_exstdtc(EX)

    expect_true(!is.null(attr(check, "data")))
    expect_equal(attr(check, "data"), filter(EX, EXDOSE >= 0 & (EXOCCUR == "Y" | is.na(EXOCCUR) | EXOCCUR == "NA")))

})
