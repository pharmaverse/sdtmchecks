test_that("Function returns true when no errors are present", {
  
  LB <- data.frame(
  USUBJID = "1",
  LBTEST = "Albumin",
  LBSTRESN = 1:10,
  LBSTNRLO = 1:10,
  LBSTNRHI = 1:10,
  stringsAsFactors=FALSE
  )
  
  DM <- data.frame(
  USUBJID = "1",
  SITEID = "123456",
  stringsAsFactors=FALSE
  )
  
  expect_true(check_lb_lbstnrlo_lbstnrhi(DM, LB))
})

test_that("Function returns false when errors are present", {
  
  LB <- data.frame(
    USUBJID = "1",
    LBTEST = "Albumin",
    LBSTRESN = 1:10,
    LBSTNRLO = 1:10,
    LBSTNRHI = 1:10,
    stringsAsFactors=FALSE
  )
  
 LB$LBSTNRLO[1]=""
 LB$LBSTNRLO[2]="NA"
 LB$LBSTNRLO[3]=NA
 LB$LBSTNRHI[3]=""
 LB$LBSTNRHI[4]="NA"
 LB$LBSTNRHI[5]=NA
  
  DM <- data.frame(
    USUBJID = "1",
    SITEID = "123456",
    stringsAsFactors=FALSE
  )
  
  expect_false(check_lb_lbstnrlo_lbstnrhi(DM, LB))
  
})





test_that("Function returns false when expected column not present", {

  LB <- data.frame(
    USUBJID = "1",
    LBTEST = "Albumin",
    LBSTRESN = 1:10,
    LBSTNRLO = 1:10,
    LBSTNRHI = 1:10,
    stringsAsFactors=FALSE
  )
  
LB$LBSTNRLO=NULL
LB$LBSTNRHI=NULL
  
  DM <- data.frame(
    USUBJID = "1",
    SITEID = "123456",
    stringsAsFactors=FALSE
  )
  
  expect_false(check_lb_lbstnrlo_lbstnrhi(DM, LB))
    
  })

