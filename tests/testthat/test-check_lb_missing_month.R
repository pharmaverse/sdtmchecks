test_that("Function returns true when no errors are present", {
  
  LB <- data.frame(
   USUBJID = 1:4,
   LBTEST = c("TEST1","TEST2","TEST3","TEST3"),
   LBDTC = c("2017-01-01","2017-02-01","2017---01", "2017----01"),
   VISIT = c("VISIT1","VISIT2","VISIT3","VISIT3"),
   stringsAsFactors=FALSE
  )
  
  LB=LB[1:2,]
  
  
  expect_true(check_lb_missing_month(LB))
})

test_that("Function returns false when errors are present", {
  
  LB <- data.frame(
    USUBJID = 1:4,
    LBTEST = c("TEST1","TEST2","TEST3","TEST3"),
    LBDTC = c("2017-01-01","2017-02-01","2017---01", "2017----01"),
    VISIT = c("VISIT1","VISIT2","VISIT3","VISIT3"),
    stringsAsFactors=FALSE
  )
  
  expect_false(check_lb_missing_month(LB))
})



test_that("Function returns false when expected column not present", {
  
  LB <- data.frame(
    USUBJID = 1:4,
    LBTEST = c("TEST1","TEST2","TEST3","TEST3"),
    LBDTC = c("2017-01-01","2017-02-01","2017---01", "2017----01"),
    VISIT = c("VISIT1","VISIT2","VISIT3","VISIT3"),
    stringsAsFactors=FALSE
  )
  
  LB$LBDTC=NULL
  
  expect_false(check_lb_missing_month(LB))
})

