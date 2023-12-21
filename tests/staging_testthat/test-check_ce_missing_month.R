test_that("Function returns true when no errors are present", {
  CE <- data.frame(
    USUBJID = 1:3,
    CETERM = c("Headache", "Nausea", "Dizziness"),
    CESTDTC = c("2017-01-01", "2017---01", "2017-01-02"),
    CEENDTC = c("2017-02-01", "2017-03-01", "2017---01"),
    stringsAsFactors = FALSE
  )
  
  CE = CE[1,]
  
  
  expect_true(check_ce_missing_month(CE))
})

test_that("Function returns false when errors are present", {
  CE <- data.frame(
    USUBJID = 1:3,
    CETERM = c("Headache", "Nausea", "Dizziness"),
    CESTDTC = c("2017-01-01", "2017---01", "2017-01-02"),
    CEENDTC = c("2017-02-01", "2017-03-01", "2017---01"),
    stringsAsFactors = FALSE
  )
  
  expect_false(check_ce_missing_month(CE))
})

test_that("Function returns false when expected column not present", {
  CE <- data.frame(
    USUBJID = 1:3,
    CETERM = c("Headache", "Nausea", "Dizziness"),
    CESTDTC = c("2017-01-01", "2017---01", "2017-01-02"),
    CEENDTC = c("2017-02-01", "2017-03-01", "2017---01"),
    stringsAsFactors = FALSE
  )
  
  CE$CESTDTC = NULL
  expect_false(check_ce_missing_month(CE))
  
  CE$CESTDTC = c("2017-01-01", "2017---01", "2017-01-02")
  CE$CEENDTC = NULL
  expect_false(check_ce_missing_month(CE))
  
})


test_that("Confirms conditional logic to check for CEDTC and CESTDTC", {
  CE <- data.frame(
    USUBJID = 1:3,
    CETERM = c("Headache", "Nausea", "Dizziness"),
    CEDTC  = c("2017-01-01", "2017---01", "2017-01-02"),
    CEENDTC = c("2017-02-01", "2017-03-01", "2017---01"),
    stringsAsFactors = FALSE
  )
  
  CE = CE[1,]
  expect_true(check_ce_missing_month(CE))
  
  CE$CESTDTC = "2017-01-01"
  expect_true(check_ce_missing_month(CE))
  
  
  CE$CESTDTC <- NULL
  CE$CEENDTC <- NULL
  CE$CEDTC <- NULL 
  expect_false(check_ce_missing_month(CE))
})