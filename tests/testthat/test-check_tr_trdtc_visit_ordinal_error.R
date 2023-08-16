
test_that("Returns true when no error present", {
  
  TR <- data.frame(USUBJID = 101:102,
                   TRSEQ=rep(1:5,2),
                   TRDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = rep(1:5,2),
                   VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "Visit 4","VIsit 5"), 2),
                   TREVAL="INVESTIGATOR",
                   TRSTAT = "",
                   stringsAsFactors = FALSE)
  
  expect_true(check_tr_trdtc_visit_ordinal_error(TR))
  
})


test_that("Returns false when no error present and TRSTAT =  NOT DONE for all", {
  
  TR <- data.frame(USUBJID = 101:102,
                   TRSEQ=rep(1:5,2),
                   TRDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = rep(1:5,2),
                   VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "Visit 4","VIsit 5"), 2),
                   TREVAL="INVESTIGATOR",
                   TRSTAT = "NOT DONE",
                   stringsAsFactors = FALSE)
  
  expect_false(check_tr_trdtc_visit_ordinal_error(TR))
  
})


test_that("Returns true when no error present and TRSTAT =  NOT DONE for one record", {
  
  TR <- data.frame(USUBJID = 101:102,
                   TRSEQ=rep(1:5,2),
                   TRDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = rep(1:5,2),
                   VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "Visit 4","VIsit 5"), 2),
                   TREVAL="INVESTIGATOR",
                   TRSTAT = "",
                   stringsAsFactors = FALSE)
  
  TR$TRSTAT[1] <- "NOT DONE" 
  
  expect_true(check_tr_trdtc_visit_ordinal_error(TR))
  
})


test_that("Returns false when no error present and TREVAL = NA", {
  
  TR <- data.frame(USUBJID = 101:102,
                   TRSEQ=rep(1:5,2),
                   TRDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = rep(1:5,2),
                   VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "Visit 4","VIsit 5"), 2),
                   TREVAL = NA,
                   TRSTAT = "",
                   stringsAsFactors = FALSE)
  
  expect_false(check_tr_trdtc_visit_ordinal_error(TR))
  
})
 

test_that("Returns false when no error present and no TREVAL variable present", {
  
  TR <- data.frame(USUBJID = 101:102,
                   TRSEQ=rep(1:5,2),
                   TRDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = rep(1:5,2),
                   VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "Visit 4","VIsit 5"), 2),
                   TRSTAT = "",
                   stringsAsFactors = FALSE)
  
  expect_false(check_tr_trdtc_visit_ordinal_error(TR))
  
})


test_that("Returns false when errors present - 1", {
  
  TR <- data.frame(USUBJID = 101:102,
                   TRSEQ=rep(1:5,2),
                   TRDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = rep(1:5,2),
                   VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "Visit 4","VIsit 5"), 2),
                   TREVAL="INVESTIGATOR",
                   TRSTAT = "",
                   stringsAsFactors = FALSE)
  
  # adding cases with earler date
  TR$TRDTC[TR$USUBJID == 101 & TR$VISIT == "Visit 4"] <- "2017-01-10T08:25"
  TR$TRDTC[TR$USUBJID == 102 & TR$VISIT == "Visit 2"] <- "2017-01-01T06:25"
  
  expect_false(check_tr_trdtc_visit_ordinal_error(TR))
})

test_that("Returns false when errors present - 2", {
  
  TR <- data.frame(USUBJID = 101:102,
                   TRSEQ=rep(1:5,2),
                   TRDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = rep(1:5,2),
                   VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "Visit 4","VIsit 5"), 2),
                   TREVAL="INVESTIGATOR",
                   TRSTAT = "",
                   stringsAsFactors = FALSE)
  
  # adding cases with duplicated date
  TR$TRDTC[TR$USUBJID == 101 & TR$VISIT == "Visit 5"] <- "2017-01-10T08:25"
  TR$TRDTC[TR$USUBJID == 102 & TR$VISIT == "Visit 3"] <- "2017-01-01T06:25"
  
  expect_false(check_tr_trdtc_visit_ordinal_error(TR))
})

test_that("Returns false when expected variable (USUBJID) not present", {
  
  TR <- data.frame(USUBJID = 101:102,
                   TRSEQ=rep(1:5,2),
                   TRDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = rep(1:5,2),
                   VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "Visit 4","VIsit 5"), 2),
                   TREVAL="INVESTIGATOR",
                   TRSTAT = "",
                   stringsAsFactors = FALSE)
  
  TR$USUBJID <- NULL
  
  expect_false(check_tr_trdtc_visit_ordinal_error(TR))
})



test_that("Returns false when no Investigator (INV) records", {
  
  TR <- data.frame(USUBJID = 101:102,
                   TRSEQ=rep(1:5,2),
                   TRDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = rep(1:5,2),
                   VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "Visit 4","VIsit 5"), 2),
                   TREVAL="INDEPENDENT ASSESSOR",
                   TREVALID = "ONCOLOGIST",
                   TRSTAT = "",
                   stringsAsFactors = FALSE)
  
  expect_false(check_tr_trdtc_visit_ordinal_error(TR))
})


test_that("Returns false when VISITNUM exists but only a single value (VISITNUM = 1)", {
  
  TR <- data.frame(USUBJID = 101:102,
                   TRSEQ=rep(1:5,2),
                   TRDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = 1,
                   VISIT = "SCREENING",
                   TREVAL="INVESTIGATOR", 
                   TRSTAT = "",
                   stringsAsFactors = FALSE)
  
  expect_false(check_tr_trdtc_visit_ordinal_error(TR))
})



test_that("Returns false when VISITNUM exists but only a single value (VISITNUM = 999)", {
  
  TR <- data.frame(USUBJID = 101:102,
                   TRSEQ=rep(1:5,2),
                   TRDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = 999,
                   VISIT = "UNSCHEDULED",
                   TREVAL="INVESTIGATOR",  
                   TRSTAT = "",
                   stringsAsFactors = FALSE)
  
  expect_false(check_tr_trdtc_visit_ordinal_error(TR))
})



test_that("Returns false when VISITNUM exists but only a single value (VISITNUM = 2) and VISIT = NOT UNSCHEDULED", {
  
  TR <- data.frame(USUBJID = 101:102,
                   TTRSEQ=rep(1:5,2),
                   TRDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = 2,
                   VISIT = "NOT UNSCHEDULED",
                   TREVAL="INVESTIGATOR",
                   TRSTAT = "",
                   stringsAsFactors = FALSE)

  expect_false(check_tr_trdtc_visit_ordinal_error(TR))
})




test_that("Returns false when VISITNUM exists but only a single value (VISITNUM = 1.4) and VISIT = CYCLE 2 DAY 1", {
  
  TR <- data.frame(USUBJID = 101:102,
                   TRSEQ=rep(1:5,2),
                   TRDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = 1.4,
                   VISIT = "CYCLE 2 DAY 1",
                   TREVAL = "INVESTIGATOR",
                   TREVALID = NA,
                   TRSTAT = "",
                   stringsAsFactors = FALSE)
  
  expect_false(check_tr_trdtc_visit_ordinal_error(TR))
})

test_that("Returns false when VISITNUM exists but only a single value (VISITNUM = 4) and VISIT = CYCLE 2 DAY 1", {
  
  TR <- data.frame(USUBJID = 101:102, 
                   TRSEQ=rep(1:5,2), 
                   TRDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = 4,
                   VISIT = "CYCLE 2 DAY 1",
                   TREVAL="INVESTIGATOR",
                   TREVALID = NA,
                   TRSTAT = "",
                   stringsAsFactors = FALSE)
  
  expect_false(check_tr_trdtc_visit_ordinal_error(TR))
})


test_that("Returns false when VISITNUM exists but only a single non-missing value (VISITNUM = 1.4)", {
  
  TR <- data.frame(USUBJID = 101:102, 
                   TRSEQ=rep(1:5,2), 
                   TRDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM =1.4,
                   VISIT = "UNSCHEDULED",
                   TREVAL="INVESTIGATOR",
                   TRSTAT = "",
                   stringsAsFactors = FALSE) 
  
  expect_false(check_tr_trdtc_visit_ordinal_error(TR))
})


