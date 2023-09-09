test_that("function errors when given bad input", {
    #expect_error(check_ex_exdose_pos_exoccur_no(list()))

    # EX <- data.frame(
    #     USUBJID = 1:6,
    #     EXSTDTC = rep("2017-01-01",5),
    #     EXTRT   = c(rep("TRT A",2),rep("TRT B",3)),
    #     EXOCCUR = c(".","", "N", "N", "Y"),
    #     EXDOSE  = 0:4,
    #     VISIT = "VISIT 1",
    #     stringsAsFactors = FALSE
    # )

    expect_error(check_ex_exdose_pos_exoccur_no(data.frame(
        USUBJID = 1:6,
        EXSTDTC = rep("2017-01-01",5),
        EXTRT   = c(rep("TRT A",2),rep("TRT B",3)),
        EXOCCUR = c(".","", "N", "N", "Y"),
        EXDOSE  = 0:4,
        VISIT = "VISIT 1",
        stringsAsFactors = FALSE
    )))

})

test_that("function returns true when no errors are present", {

    EX <- data.frame(
        USUBJID = 1:5,
        EXSTDTC = rep("2017-01-01",5),
        EXTRT   = c(rep("TRT A",2),rep("TRT B",3)),
        EXOCCUR = c(".","Y", "Y", "Y", "Y"),
        EXDOSE  = 0:4,
        VISIT = "VISIT 1",
        stringsAsFactors = FALSE
    )

    expect_true(check_ex_exdose_pos_exoccur_no(EX))

})

test_that("function returns false when errors are present", {

    EX <- data.frame(
        USUBJID = 1:5,
        EXSTDTC = rep("2017-01-01",5),
        EXTRT   = c(rep("TRT A",2),rep("TRT B",3)),
        EXOCCUR = c(".","", "N", "N", "Y"),
        EXDOSE  = 0:4,
        VISIT = "VISIT 1",
        stringsAsFactors = FALSE
    )


    expect_false(check_ex_exdose_pos_exoccur_no(EX))

})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {


    EX <- data.frame(
        USUBJID = integer(),
        EXSTDTC = character(),
        EXTRT   = character(),
        EXOCCUR = character(),
        EXDOSE  = integer(),
        VISIT = character(),
        stringsAsFactors = FALSE
    )



    expect_true(check_ex_exdose_pos_exoccur_no(EX))
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
#     expect_false(check_ex_exdose_pos_exoccur_no(EX))
# })

test_that("Function returns true when no errors are present for a single input (one row)", {

    EX <- data.frame(
        USUBJID = 1,
        EXSTDTC = "2017-01-01",
        EXTRT   = "TRT A",
        EXOCCUR = "Y",
        EXDOSE  = 1,
        VISIT = "VISIT 1",
        stringsAsFactors = FALSE
    )

    expect_true(check_ex_exdose_pos_exoccur_no(EX))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    EX <- data.frame(
        USUBJID = 1,
        EXSTDTC = rep("2017-01-01",1),
        EXTRT   = c(rep("TRT A",1)),
        EXOCCUR = c("N"),
        EXDOSE  = 4,
        VISIT = "VISIT 1",
        stringsAsFactors = FALSE
    )


    expect_false(check_ex_exdose_pos_exoccur_no(EX))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {


    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    EX <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     EXTRT = c("TRT A", "TRT B"),
                     EXDOSE = rep(x = c(800, 1200, 80), times = c(300, 300, 300)),
                     EXSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     EXOCCUR = "Y",
                     VISIT = "VISIT 1",
                     stringsAsFactors = FALSE)


    expect_true(check_ex_exdose_pos_exoccur_no(EX))
})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    EX <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     EXTRT = c("TRT A", "TRT B"),
                     EXDOSE = rep(x = c(0,800, 1200, 80), times = c(225, 225, 225, 225)),
                     EXSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     EXOCCUR = c(rep("Y",675), rep("N", 225)),
                     VISIT = "VISIT 1",
                     stringsAsFactors = FALSE)



    expect_false(check_ex_exdose_pos_exoccur_no(EX))

})

test_that("Function returns the failed object in attr(data)", {

    EX <- data.frame(
        USUBJID = 1:5,
        EXTRT   = c(rep("TRT A",2),rep("TRT B",3)),
        EXSTDTC = rep("2017-01-01",5),
        EXOCCUR = c(".","", "N", "N", "Y"),
        EXDOSE  = 0:4,
        VISIT = "VISIT 1",
        stringsAsFactors = FALSE
    )


    check <- check_ex_exdose_pos_exoccur_no(EX)

    expect_true(!is.null(attr(check, "data")))
    expect_equal(attr(check, "data"), filter(EX, EXDOSE >0 & EXOCCUR != "Y"))

})




