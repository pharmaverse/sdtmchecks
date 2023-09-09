test_that("Function returns true when no errors are present", {
  
 PR <- data.frame(
  USUBJID = 1:3,
  PRTRT = c("Surgery Name","Procedure Name","Procedure"),
  PRSTDTC = c("2017-01-01","2017---01","2017-01-02"),
  PRENDTC = c("2017-02-01","2017-03-01","2017---01"),
  PRSPID = "/F:SURG-D:12345-R:1",
  PRCAT = "Form 1",
  stringsAsFactors=FALSE
 )
   
   PR=PR[1,]
  
  
  expect_true(check_pr_missing_month(PR))
})

test_that("Function returns false when errors are present", {
  
  PR <- data.frame(
    USUBJID = 1:3,
    PRTRT = c("Surgery Name","Procedure Name","Procedure"),
    PRSTDTC = c("2017-01-01","2017---01","2017-01-02"),
    PRENDTC = c("2017-02-01","2017-03-01","2017---01"),
    PRSPID = "/F:SURG-D:12345-R:1",
    PRCAT = "Form 1",
    stringsAsFactors=FALSE
  )
  
  expect_false(check_pr_missing_month(PR))
})



test_that("Function returns false when expected column not present", {
  
  PR <- data.frame(
    USUBJID = 1:3,
    PRTRT = c("Surgery Name","Procedure Name","Procedure"),
    PRSTDTC = c("2017-01-01","2017---01","2017-01-02"),
    PRENDTC = c("2017-02-01","2017-03-01","2017---01"),
    PRSPID = "/F:SURG-D:12345-R:1",
    PRCAT = "Form 1",
    stringsAsFactors=FALSE
  )
  
  PR$PRSTDTC=NULL
  
  expect_false(check_pr_missing_month(PR))
})

