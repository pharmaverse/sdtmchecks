test_that("function errors when given bad input", {
    #expect_error(check_ex_exstdtc_after_exendtc(list()))

    # EX <- data.frame(
    #     STUDYID = 1,
    #     USUBJID = 1:13,
    #     EXTRT = "SOME DRUG",
    #     EXSTDTC = c("2017-01-01","2017-01-03","2017-01-01T14:26","2017","2017-02","2017"      ,""    ,
    #                 "2017"   ,"2017-01-01T14:26","2017-01-01T14:26","2017-01-01T14","2017-01-01T14:26:02")
    #     ,
    #     EXENDTC = c("2017-01-01","2017-01-02","2017-01-01T14:25","2015","2017-01","2016-01-01","2000",
    #                 "2017-02","2017-01-01"      ,"2017-01","2017-01-01T13","2017-01-01T14:26:01")
    #     ,
    #     EXOCCUR = "Y",
    #     VISIT = "CYCLE 1 DAY 1",
    #     stringsAsFactors=FALSE
    # )

    expect_error(check_ex_exstdtc_after_exendtc(data.frame(
        STUDYID = 1,
        USUBJID = 1:13,
        EXTRT = "SOME DRUG",
        EXSTDTC = c("2017-01-01","2017-01-03","2017-01-01T14:26","2017","2017-02","2017"      ,""    ,
                    "2017"   ,"2017-01-01T14:26","2017-01-01T14:26","2017-01-01T14","2017-01-01T14:26:02")
        ,
        EXENDTC = c("2017-01-01","2017-01-02","2017-01-01T14:25","2015","2017-01","2016-01-01","2000",
                    "2017-02","2017-01-01"      ,"2017-01","2017-01-01T13","2017-01-01T14:26:01")
        ,
        EXOCCUR = "Y",
        VISIT = "CYCLE 1 DAY 1",
        stringsAsFactors=FALSE
    )))

})

test_that("function returns true when no errors are present", {

    EX <- data.frame(
        STUDYID = 1,
        USUBJID = 1:6,
        EXTRT = "SOME DRUG",
        EXSTDTC = c("2017-01-01","2017-01-02","2017-01-01T14:25","2015","2017-01","2016-01-01","2000",
                    "2017-02","2017-01-01"      ,"2017-01","2017-01-01T13","2017-01-01T14:26:01"),
        EXENDTC= c("2017-01-01","2017-01-03","2017-01-01T14:26","2017","2017-02","2017"      ,""    ,
                    "2017"   ,"2017-01-01T14:26","2017-01-01T14:26","2017-01-01T14","2017-01-01T14:26:02"),
        EXOCCUR = "Y",
        VISIT = "CYCLE 1 DAY 1",
        stringsAsFactors=FALSE
    )

    expect_true(check_ex_exstdtc_after_exendtc(EX))

})

test_that("function returns false when errors are present", {

    EX <- data.frame(
        STUDYID = 1,
        USUBJID = 1:12,
        EXTRT = "SOME DRUG",
        EXSTDTC = c("2017-01-01","2017-01-03","2017-01-01T14:26","2017","2017-02","2017"      ,""    ,
                    "2017"   ,"2017-01-01T14:26","2017-01-01T14:26","2017-01-01T14","2017-01-01T14:26:02")
        ,
        EXENDTC = c("2017-01-01","2017-01-02","2017-01-01T14:25","2015","2017-01","2016-01-01","2000",
                    "2017-02","2017-01-01"      ,"2017-01","2017-01-01T13","2017-01-01T14:26:01")
        ,
        EXOCCUR = "Y",
        VISIT = "CYCLE 1 DAY 1",
        stringsAsFactors=FALSE
    )

    expect_false(check_ex_exstdtc_after_exendtc(EX))

})

# test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {
# 
#     EX <- data.frame(
#         STUDYID = integer(),
#         USUBJID = integer(),
#         EXTRT = character(),
#         EXSTDTC = character(),
#         EXENDTC= character(),
#         EXOCCUR = character(),
#         VISIT = character(),
#         stringsAsFactors=FALSE
#     )
# 
#     expect_true(check_ex_exstdtc_after_exendtc(EX))
# })

# test_that("Function returns false when errors are present for an empty dataframe (zero rows)", {
# 
#     EX <- data.frame(USUBJID =NA,
#                      EXSEQ=NA,
#                      EXSTDTC=NA,
#                      EXTERM="",
#                      EXDECOD ="NA",
#                      stringsAsFactors=FALSE)
# 
#     expect_false(check_ex_exstdtc_after_exendtc(EX))
# })

test_that("Function returns true when no errors are present for a single input (one row)", {

    EX <- data.frame(
        STUDYID = 1,
        USUBJID = 1,
        EXTRT = "SOME DRUG",
        EXSTDTC = c("2017-01-01"),
        EXENDTC = c("2017-01-01"),
        EXOCCUR = "Y",
        VISIT = "CYCLE 1 DAY 1",
        stringsAsFactors=FALSE
    )

    expect_true(check_ex_exstdtc_after_exendtc(EX))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    EX <- data.frame(
        STUDYID = 1,
        USUBJID = 1,
        EXTRT = "SOME DRUG",
        EXSTDTC = c("2017-01-03"),
        EXENDTC = c("2017-01-01"),
        EXOCCUR = "Y",
        VISIT = "CYCLE 1 DAY 1",
        stringsAsFactors=FALSE
    )

    expect_false(check_ex_exstdtc_after_exendtc(EX))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {


    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    EX <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     STUDYID = 1,
                     VISIT =  "CYCLE 1 DAY 1",
                     EXTRT = c("SOME DRUG"),
                     EXDOSE = rep(x = c(800, 1200, 80), times = c(300, 300, 300)),
                     EXSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     EXENDTC  = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     EXOCCUR = "Y",
                     stringsAsFactors = FALSE)


    expect_true(check_ex_exstdtc_after_exendtc(EX))
})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    EX <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     STUDYID = 1,
                     VISIT =  "CYCLE 1 DAY 1",
                     EXTRT = c("SOME DRUG"),
                     EXDOSE = rep(x = c(800, 1200, 80), times = c(300, 300, 300)),
                     EXSTDTC = as.character(rep(seq(as.Date('2011-04-03'), as.Date('2011-06-01'), by = 1), times = 15)),
                     EXENDTC  = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     EXOCCUR = "Y",
                     stringsAsFactors = FALSE)

    expect_false(check_ex_exstdtc_after_exendtc(EX))

})

test_that("Function returns the failed object in attr(data)", {

    EX <- data.frame(
        STUDYID = 1,
        USUBJID = 1:12,
        EXTRT = "SOME DRUG",
        EXSTDTC = c("2017-01-01","2017-01-03","2017-01-01T14:26","2017","2017-02","2017"      ,""    ,
                    "2017"   ,"2017-01-01T14:26","2017-01-01T14:26","2017-01-01T14","2017-01-01T14:26:02")
        ,
        EXENDTC = c("2017-01-01","2017-01-02","2017-01-01T14:25","2015","2017-01","2016-01-01","2000",
                    "2017-02","2017-01-01"      ,"2017-01","2017-01-01T13","2017-01-01T14:26:01")
        ,
        EXOCCUR = "Y",
        VISIT = "CYCLE 1 DAY 1",
        stringsAsFactors=FALSE
    )


    check <- check_ex_exstdtc_after_exendtc(EX)

    expect_true(!is.null(attr(check, "data")))
    #expect_equal(attr(check, "data"), filter(EX, EXENDTC < EXSTDTC))

})
