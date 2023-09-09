EG<- data.frame(USUBJID = 101:102,
                EGDTC=rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                            "2017-01-15T10:25", "2017-01-20T08:25", "2017-01-25T08:25"), 2),
                VISITNUM=rep(1:5,2),
                VISIT=rep(c("Screening", "Cycle 1", "Cycle 2",
                            "Cycle 3","UNschedUled"),2),
                EGSTAT="",
                stringsAsFactors=FALSE)


test_that("Function returns true when no errors are present", {
  
  expect_true(check_eg_egdtc_visit_ordinal_error(EG))
})

test_that("Function returns false when errors are present", {
  
  EG1 <- EG
  # Cases with earlier datetime
  EG1$EGDTC[EG1$USUBJID == 101 & EG1$VISIT == "Cycle 3"] <- "2017-01-10T08:25"
  EG1$EGDTC[EG1$USUBJID == 102 & EG1$VISIT == "Cycle 1"] <- "2017-01-01T06:25"
  
  expect_false(check_eg_egdtc_visit_ordinal_error(EG1))
})


test_that("Function returns false when errors are present ", {
  
  EG2 <- EG
  # Cases with earlier date
  EG2$EGDTC[EG2$USUBJID == 101 & EG2$VISIT == "Cycle 3"] <- "2017-01-15T10:25"
  EG2$EGDTC[EG2$USUBJID == 102 & EG2$VISIT == "Cycle 2"] <- "2017-01-01T06:25"
  
  
  expect_false(check_eg_egdtc_visit_ordinal_error(EG2))
})



test_that("Function returns true when duplicate datetimes for same visit", {
  
  # Not checking duplicates
  EG_DUP<- data.frame(USUBJID = rep("101",6),
                  EGDTC=rep("2017-01-01T08:25", 6),
                  VISITNUM=rep(1:2,3),
                  VISIT=rep("Screening",6),
                  EGSTAT="",
                  stringsAsFactors=FALSE)
   
  expect_true(check_eg_egdtc_visit_ordinal_error(EG_DUP))
})

