test_that("Function returns true when no errors are present", {
  
  AE <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
                  AEDTHDTC = c(rep("", 4), "2016-01-01"),
                  AESTDTC = rep("2016-01-01", 5),
                  AEDECOD = LETTERS[1:5], AETERM = LETTERS[1:5],
                  stringsAsFactors = FALSE)
  DS <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
                  DSSTDTC = rep("2016-01-02", 5),
                  DSDECOD = c(LETTERS[1:4], "death"),
                  DSTERM = letters[1:5],
                  stringsAsFactors = FALSE)
  LB <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
                  LBDTC = rep("2015-12-31", 5),
                  LBTESTCD = letters[1:5],
                  LBORRES = 1:5,
                  stringsAsFactors = FALSE)
  
  
  expect_true(check_lb_lbdtc_after_dd(AE, DS, LB))
})

test_that("Function returns false when errors are present", {
  
  AE <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
                   AEDTHDTC = c(rep("", 4), "2016-01-01"),
                   AESTDTC = rep("2016-01-01", 5),
                   AEDECOD = LETTERS[1:5], AETERM = LETTERS[1:5],
                   stringsAsFactors = FALSE)
  DS <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
                   DSSTDTC = rep("2016-01-02", 5),
                   DSDECOD = c(LETTERS[1:4], "death"),
                   DSTERM = letters[1:5],
                   stringsAsFactors = FALSE)
  LB <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
                   LBDTC = rep("2015-12-31", 5),
                   LBTESTCD = letters[1:5],
                   LBORRES = 1:5,
                   stringsAsFactors = FALSE)
  
  LB$LBDTC[1] <- "2016-01-03"
  LB$USUBJID[1] <- LB$USUBJID[5]
  
  
  expect_false(check_lb_lbdtc_after_dd(AE, DS, LB))
})



test_that("Function returns false when expected column not present", {
  
  AE <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
                   AEDTHDTC = c(rep("", 4), "2016-01-01"),
                   AESTDTC = rep("2016-01-01", 5),
                   AEDECOD = LETTERS[1:5], AETERM = LETTERS[1:5],
                   stringsAsFactors = FALSE)
  DS <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
                   DSSTDTC = rep("2016-01-02", 5),
                   DSDECOD = c(LETTERS[1:4], "death"),
                   DSTERM = letters[1:5],
                   stringsAsFactors = FALSE)
  LB <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
                   LBDTC = rep("2015-12-31", 5),
                   LBTESTCD = letters[1:5],
                   LBORRES = 1:5,
                   stringsAsFactors = FALSE)
  
  LB$LBDTC=NULL
  
  

  
  expect_false(check_lb_lbdtc_after_dd(AE, DS, LB))
})

