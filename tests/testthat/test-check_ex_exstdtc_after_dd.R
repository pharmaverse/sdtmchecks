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

EX <- data.frame(STUDYID = 1:5, USUBJID = LETTERS[1:5],
                 EXSTDTC = rep("2015-12-31", 5),
                 EXTRT = LETTERS[1:5],
                 EXDOSE = 1:5,
                 stringsAsFactors = FALSE)



test_that("Function returns true when no errors are present", {
  
  expect_true(check_ex_exstdtc_after_dd(AE, DS, EX))
  
})

test_that("Function returns false when errors are present (earlier EXTSTDTC relative to death date)", {
  
  EX1 <- EX

  EX1$EXSTDTC[1] <- "2016-01-03"
  EX1$USUBJID[1] <- EX1$USUBJID[5]

  
  expect_false(check_ex_exstdtc_after_dd(AE, DS, EX1))
  
})

test_that("Function returns false when mandatory variable missing", {
  
  EX2 <- EX
  
  EX2$USUBJID <- NULL
  expect_false(check_ex_exstdtc_after_dd(AE, DS, EX2))
  
})
