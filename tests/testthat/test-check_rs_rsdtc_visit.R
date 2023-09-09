test_that("function errors when given bad input", {
    #expect_error(check_rs_rsdtc_visit(list()))

    # RS <- data.frame(
    #     USUBJID = 1:11,
    #     RSDTC = 1:10,
    #     RSORRES = 1:10,
    #     VISIT = "C1D1",
    #     RSSTAT = 1:10,
    #     RSEVAL = c("NA","","INDEPENDENT ASSESSOR","investigator",rep("INVESTIGATOR",6)),
    #     stringsAsFactors=FALSE
    # )

    expect_error(check_rs_rsdtc_visit(data.frame(
        USUBJID = 1:11,
        RSDTC = 1:10,
        RSORRES = 1:10,
        VISIT = "C1D1",
        RSSTAT = 1:10,
        RSEVAL = c("NA","","INDEPENDENT ASSESSOR","investigator",rep("INVESTIGATOR",6)),
        stringsAsFactors=FALSE
    )))

})

test_that("function returns true when no errors are present", {

    RS <- data.frame(
        USUBJID = 1:10,
        RSDTC = 1:10,
        RSORRES = 1:10,
        VISIT = "C1D1",
        RSSTAT = 1:10,
        RSEVAL = c("NA","","INDEPENDENT ASSESSOR","investigator",rep("INVESTIGATOR",6)),
        stringsAsFactors=FALSE
    )

    expect_true(check_rs_rsdtc_visit(RS))

})

test_that("function returns false when errors are present", {

    RS <- data.frame(
        USUBJID = 1:10,
        RSDTC = 1:10,
        RSORRES = 1:10,
        VISIT = "C1D1",
        RSSTAT = 1:10,
        RSEVAL = c("NA","","INDEPENDENT ASSESSOR","investigator",rep("INVESTIGATOR",6)),
        stringsAsFactors=FALSE
    )

    RS$RSDTC[1]=""
    RS$RSDTC[2]="NA"
    RS$RSDTC[3]=NA
    RS$VISIT[3]=""
    RS$VISIT[4]="NA"
    RS$VISIT[5]=NA

    RS$RSORRES[1]=""

    expect_false(check_rs_rsdtc_visit(RS))

})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    RS <- data.frame(
        USUBJID = integer(),
        RSDTC = integer(),
        RSORRES = integer(),
        VISIT = character(),
        RSSTAT = integer(),
        RSEVAL = character(),
        stringsAsFactors=FALSE
    )

    expect_true(check_rs_rsdtc_visit(RS))

})

test_that("Function returns true when no errors are present for a single input (one row)", {

    RS <- data.frame(
        USUBJID = 1,
        RSDTC = 1,
        RSORRES = 1,
        VISIT = "C1D1",
        RSSTAT = 1,
        RSEVAL = "investigator",
        stringsAsFactors=FALSE
    )

    expect_true(check_rs_rsdtc_visit(RS))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    RS <- data.frame(
        USUBJID = 1,
        RSDTC = 1,
        RSORRES = 1,
        VISIT = "C1D1",
        RSSTAT = 1,
        RSEVAL = "investigator",
        stringsAsFactors=FALSE
    )

    RS$RSDTC[1]=""

    expect_false(check_rs_rsdtc_visit(RS))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    RS <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     RSDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     RSORRES = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     VISIT = "C1D1",
                     RSSTAT = c(rep("NOT DONE", 450), rep("", 450)),
                     RSEVAL = "investigator",
                     stringsAsFactors = FALSE)


    expect_true(check_rs_rsdtc_visit(RS))

})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    RS <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     RSDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     RSORRES = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     VISIT = "C1D1",
                     RSSTAT = rep("", 450),
                     RSEVAL = "investigator",
                     stringsAsFactors = FALSE)

    RS$RSDTC[1]=""
    RS$RSDTC[2]="NA"
    RS$RSDTC[3]=NA
    RS$VISIT[3]=""
    RS$VISIT[4]="NA"
    RS$VISIT[5]=NA

    RS$RSORRES[1]=""


    expect_false(check_rs_rsdtc_visit(RS))

})

test_that("Function returns the failed object in attr(data)", {

    RS <- data.frame(
        USUBJID = 1:10,
        RSDTC = 1:10,
        RSORRES = 1:10,
        VISIT = "C1D1",
        RSSTAT = 1:10,
        RSEVAL = c("NA","","INDEPENDENT ASSESSOR","investigator",rep("INVESTIGATOR",6)),
        stringsAsFactors=FALSE
    )

    RS$RSDTC[1]=""
    RS$RSDTC[2]="NA"
    RS$RSDTC[3]=NA
    RS$VISIT[3]=""
    RS$VISIT[4]="NA"
    RS$VISIT[5]=NA

    RS$RSORRES[1]=""

    check <- check_rs_rsdtc_visit(RS)

    df <- RS %>%
            filter( (RSDTC == "" | RSDTC == "NA" | is.na(RSDTC) | VISIT == "" | VISIT == "NA" | is.na(VISIT)) & (RSEVAL != "INDEPENDENT ASSESSOR") & (RSORRES != "") )

    expect_true(!is.null(attr(check, "data")))
    expect_equal(attr(check, "data"), df)

}) 
