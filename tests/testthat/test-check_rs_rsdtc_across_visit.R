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

test_that("function returns false when errors are present and displays permissible variable RSTESTCD", {
    
    RS <- data.frame(
        USUBJID = 1,
        RSDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
        VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
        RSTESTCD = c(rep("OVRLRESP", 2), rep("OTHER", 2), rep("OVRLRESP", 2), rep("OTHER", 2), rep("OVRLRESP", 2)),
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

test_that("Function returns true for an empty dataframe (zero rows)", {

    RS <- data.frame(USUBJID =character(),
                     RSSEQ=integer(),
                     RSDTC=character(),
                     VISIT=character(),
                     stringsAsFactors=FALSE)

    expect_true(check_rs_rsdtc_across_visit(RS))
})

test_that("Function returns true when no errors are present for a single input (one row)", {

    RS <- data.frame(
        USUBJID = 1,
        RSDTC = "2016-01-01",
        VISIT = "C1D1",
        stringsAsFactors=FALSE)

    expect_true(check_rs_rsdtc_across_visit(RS))
})

## Not applicable for _across_visit check
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
    
    
    RS_ret <- aggregate.data.frame(x = RS$USUBJID, by = list(USUBJID = RS$USUBJID, RSDTC = RS$RSDTC, VISIT = RS$VISIT), FUN=sum) 
    RS_ret_date <- aggregate.data.frame(x = RS$USUBJID, by = list(USUBJID = RS$USUBJID, RSDTC = RS$RSDTC), FUN=sum) 
    RS_ret_visit <- aggregate.data.frame(x = RS$USUBJID, by = list(USUBJID = RS$USUBJID, VISIT = RS$VISIT), FUN=sum)
    RS_ret_d <- merge(x=RS_ret, y=RS_ret_date, by = c("USUBJID", "RSDTC"), all = TRUE)
    RS_ret_v <- merge(x=RS_ret_d, y=RS_ret_visit, by = c("USUBJID", "VISIT"), all = TRUE)
    
    ## Note - below will show both repeats by RSDTC and repeats by VISIT 
    #RS_ret_dv <- subset(RS_ret_v, ((x.x != x.y) | (x.x != x) | (x.x != x)), select=c("USUBJID", "RSDTC", "VISIT"))
    
    ## Note - below will show only repeats by RSDTC (i.e., N records with same date at >1 visit -- same date across multiple visits)
    RS_ret_dv <- subset(RS_ret_v, (x.x != x.y), select=c("USUBJID", "RSDTC", "VISIT"))
    
    rownames(RS_ret_dv) <- NULL
    
    # compare to check function result
     
    check <- check_rs_rsdtc_across_visit(RS)

    #expect_true(!is.null(attr(check, "data")))
    #expect_equal(attr(check, "data"), select(filter(RS, )))
    expect_equal(attr(check, "data"), RS_ret_dv)
    

})


test_that("function returns false when errors are present and extra variables included", {
    
    RS <- data.frame(
        USUBJID = 1,
        RSDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
        VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
        RSSPID = c("FORMNAME-R:1/L:5XXXX"),
        RSSEQ = 1:10,
        RSNAM = c(NA),
        RSCAT = c(NA),
        stringsAsFactors=FALSE)
    
    expect_false(check_rs_rsdtc_across_visit(RS))
    
})


## note - issue #185 opened on this 
test_that("Returns true when no Investigator (INV) records and errors in dataset", {
    
    # error 
    RS <- data.frame(
        USUBJID = 1,
        RSEVAL = c("INDEPENDENT ASSESSOR"),
        RSDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
        VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
        RSSPID = c("FORMNAME-R:1/L:5XXXX"),
        RSCAT = c(NA),
        stringsAsFactors=FALSE)
    
    #expect_false(check_rs_rsdtc_across_visit(RS))
    expect_true(check_rs_rsdtc_across_visit(RS))
})


## note - issue #185 opened on this 
test_that("Returns true when no Investigator (INV) records and no other errors in dataset", {
    
    # no error 
    RS <- data.frame(USUBJID = rep(rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 399)))), times = 1), 3),
                     RSDTC = as.character(rep(seq(as.Date('2011-04-01'), as.Date('2011-06-29'), by = 1), times = 10)),
                     VISIT = c(rep("C1D1",300), rep("C1D2",300), rep("C2D1",300)),
                     stringsAsFactors = FALSE)
    
    RS$RSEVAL = c("INDEPENDENT ASSESSOR")
    
    #expect_false(check_rs_rsdtc_across_visit(RS))
    expect_true(check_rs_rsdtc_across_visit(RS))
})
