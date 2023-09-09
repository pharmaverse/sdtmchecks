test_that("Function returns true when no errors are present", {
  
  RS<- data.frame(USUBJID = 101:102,
                 RSDTC=rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                             "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                 VISITNUM=rep(1:5,2),
                 VISIT=rep(c("Screening", "Cycle 1", "Cycle 2","Cycle 3","Follow-up"),2),
                 RSTESTCD="OVRLRESP",
                 RSEVAL="INVESTIGATOR",
                 RSSTAT="",
                 stringsAsFactors=FALSE)

  expect_true(check_rs_rsdtc_visit_ordinal_error(RS))
})


test_that("Function returns false when errors are present", {
  
  RS<- data.frame(USUBJID = 101:102,
                  RSDTC=rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                              "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                  VISITNUM=rep(1:5,2),
                  VISIT=rep(c("Screening", "Cycle 1", "Cycle 2","Cycle 3","Follow-up"),2),
                  RSTESTCD="OVRLRESP",
                  RSEVAL="INVESTIGATOR",
                  RSSTAT="",
                  stringsAsFactors=FALSE)
  
  # adding cases with earlier date
  RS$RSDTC[RS$USUBJID == 101 & RS$VISIT == "Cycle 3"] <- "2017-01-02T08:25"
  RS$RSDTC[RS$USUBJID == 102 & RS$VISIT == "Cycle 1"] <- "2017-01-01T06:25"

  expect_false(check_rs_rsdtc_visit_ordinal_error(RS))
})


test_that("Function returns false when errors are present", {
  
  RS<- data.frame(USUBJID = 101:102,
                  RSDTC=rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                              "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                  VISITNUM=rep(1:5,2),
                  VISIT=rep(c("Screening", "Cycle 1", "Cycle 2","Cycle 3","Follow-up"),2),
                  RSTESTCD="OVRLRESP",
                  RSEVAL="INVESTIGATOR",
                  RSSTAT="",
                  stringsAsFactors=FALSE)
  
  # Only one value for VISITNUM
  RS$VISITNUM = rep(1, 10)
  
  expect_false(check_rs_rsdtc_visit_ordinal_error(RS))
})


test_that("Function returns false when expected column (USUBJID) not present", {
  
  RS<- data.frame(USUBJID = 101:102,
                  RSDTC=rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                              "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                  VISITNUM=rep(1:5,2),
                  VISIT=rep(c("Screening", "Cycle 1", "Cycle 2","Cycle 3","Follow-up"),2),
                  RSTESTCD="OVRLRESP",
                  RSEVAL="INVESTIGATOR",
                  RSSTAT="",
                  stringsAsFactors=FALSE)
  
  RS$USUBJID = NULL
  
  expect_false(check_rs_rsdtc_visit_ordinal_error(RS))
})


test_that("Function returns false when no Investigator (INV) records", {
  
  RS<- data.frame(USUBJID = 101:102,
                  RSDTC=rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                              "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                  VISITNUM=rep(1:5,2),
                  VISIT=rep(c("Screening", "Cycle 1", "Cycle 2","Cycle 3","Follow-up"),2),
                  RSTESTCD="OVRLRESP",
                  RSEVAL="INDEPENDENT ASSESSOR",
                  RSEVALID = "RADIOLOGIST 1", 
                  RSSTAT="",
                  stringsAsFactors=FALSE) 
  
  expect_false(check_rs_rsdtc_visit_ordinal_error(RS))
})



test_that("Function returns false when Investigator (INV) records but not OVRLRESP", {
  
  RS<- data.frame(USUBJID = 101:102,
                  RSDTC=rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                              "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                  VISITNUM=rep(1:5,2),
                  VISIT=rep(c("Screening", "Cycle 1", "Cycle 2","Cycle 3","Follow-up"),2),
                  RSTESTCD="DRCRIND",
                  RSEVAL="INVESTIGATOR",
                  RSSTAT="",
                  stringsAsFactors=FALSE) 
  
  expect_false(check_rs_rsdtc_visit_ordinal_error(RS))
})

