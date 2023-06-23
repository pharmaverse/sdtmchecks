test_that("function errors when given bad input", {

    expect_error(check_ex_extrt_exoccur(data.frame(
        USUBJID = 1:11,
        EXTRT = 1:10,
        EXOCCUR = c(rep("Y",5), rep("",5)),
        EXSEQ = 1:10,
        EXSTDTC = "2016-01-01",
        EXDOSE = 1:10,
        stringsAsFactors=FALSE
    )))

})

test_that("function returns true when no errors are present", {

    EX <- data.frame(
        USUBJID = 1:10,
        EXTRT = 1:10,
        EXOCCUR = c(rep("Y",5), rep("",5)),
        EXSEQ = 1:10,
        EXSTDTC = "2016-01-01",
        EXDOSE = 1:10,
        stringsAsFactors=FALSE
    )

    expect_true(check_ex_extrt_exoccur(EX))

})

test_that("function returns false when errors are present", {

    EX <- data.frame(
        USUBJID = 1:10,
        EXTRT = 1:10,
        EXOCCUR = c(rep("Y",5), rep("",5)),
        EXSEQ = 1:10,
        EXSTDTC = "2016-01-01",
        EXDOSE = 1:10,
        stringsAsFactors=FALSE
    )

    EX$EXTRT[1]=""
    EX$EXTRT[2]="NA"
    EX$EXTRT[3]=NA
    EX$EXTRT[6]=""
    EX$EXTRT[7]="NA"
    EX$EXTRT[8]=NA

    expect_false(check_ex_extrt_exoccur(EX))

})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    EX <- data.frame(
        USUBJID = integer(),
        EXTRT = integer(),
        EXOCCUR = character(),
        EXSEQ = integer(),
        EXSTDTC = character(),
        EXDOSE = integer(),
        stringsAsFactors=FALSE
    )

    expect_true(check_ex_extrt_exoccur(EX))
})


test_that("Function returns true when no errors are present for a single input (one row)", {

    EX <- data.frame(
        USUBJID = 1,
        EXTRT = 1,
        EXOCCUR = "Y",
        EXSEQ = 1,
        EXSTDTC = "2016-01-01",
        EXDOSE = 1,
        stringsAsFactors=FALSE
    )

    expect_true(check_ex_extrt_exoccur(EX))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    EX <- data.frame(
        USUBJID = 1,
        EXTRT = 1,
        EXOCCUR = "Y",
        EXSEQ = 1,
        EXSTDTC = "2016-01-01",
        EXDOSE = 1,
        stringsAsFactors=FALSE
    )

    EX$EXTRT[1]=""

    expect_false(check_ex_extrt_exoccur(EX))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {


    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    EX <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     EXSEQ = 1,
                     EXTRT = c("SOME DRUG"),
                     EXDOSE = rep(x = c(800, 1200, 80), times = c(300, 300, 300)),
                     EXSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     # EXENDTC  = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     EXOCCUR = "Y",
                     stringsAsFactors = FALSE)


    expect_true(check_ex_extrt_exoccur(EX))
})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    EX <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     EXSEQ = 1,
                     EXTRT = c(NA),
                     EXDOSE = rep(x = c(800, 1200, 80), times = c(300, 300, 300)),
                     EXSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     # EXENDTC  = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     EXOCCUR = "Y",
                     stringsAsFactors = FALSE)


    expect_false(check_ex_extrt_exoccur(EX))

})

test_that("Function returns the failed object in attr(data)", {

    EX <- data.frame(
        USUBJID = 1:10,
        EXSEQ = 1:10,
        EXSTDTC = "2016-01-01",
        EXTRT = 1:10,
        EXOCCUR = c(rep("Y",5), rep("",5)),
        EXDOSE = 1:10,
        stringsAsFactors=FALSE
    )

    EX$EXTRT[1]=""
    EX$EXTRT[2]="NA"
    EX$EXTRT[3]=NA


    check <- check_ex_extrt_exoccur(EX)

    expect_true(!is.null(attr(check, "data")))
    #expect_equal(attr(check, "data"), select(filter(EX, EXOCCUR == "Y" & (EXTRT == "" | EXTRT == "NA" | is.na(EXTRT))), -EXOCCUR))

})

