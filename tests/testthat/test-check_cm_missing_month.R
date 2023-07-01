test_that("Function returns true when no errors are present", {
  
   CM <- data.frame(
    USUBJID = 1:3,
    CMTRT = c("CM1","CM2","CM3"),
    CMSTDTC = c("2017-01-01","2017---01","2017-01-02"),
    CMENDTC = c("2017-02-01","2017-03-01","2017---01"),
    CMSPID = "/F:XXX-D:12345-R:123",
    stringsAsFactors=FALSE
   )
   
   CM=CM[1,]
  
  
  expect_true(check_cm_missing_month(CM))
})

test_that("Function returns false when errors are present", {
  
  CM <- data.frame(
    USUBJID = 1:3,
    CMTRT = c("CM1","CM2","CM3"),
    CMSTDTC = c("2017-01-01","2017---01","2017-01-02"),
    CMENDTC = c("2017-02-01","2017-03-01","2017---01"),
    CMSPID = "/F:XXX-D:12345-R:123",
    stringsAsFactors=FALSE
  )
  
  expect_false(check_cm_missing_month(CM))
})



test_that("Function returns false when expected column not present", {
  
  CM <- data.frame(
    USUBJID = 1:3,
    CMTRT = c("CM1","CM2","CM3"),
    CMSTDTC = c("2017-01-01","2017---01","2017-01-02"),
    CMENDTC = c("2017-02-01","2017-03-01","2017---01"),
    CMSPID = "/F:XXX-D:12345-R:123",
    stringsAsFactors=FALSE
  )
  
  CM$CMSTDTC = NULL
  
  expect_false(check_cm_missing_month(CM))
})

