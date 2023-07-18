test_that("Function returns true when no errors are present", {
  
  LB <- data.frame(
   USUBJID = c("Patient 1","Patient 2","Patient 3"),
   LBTEST   = "Test A",
   LBTESTCD   = "TA",
   LBDTC   = "2017-01-01",
   LBORRES = c("5","6","7"),
   LBSTRESC = c("5","6","7"),
   LBORRESU = rep("mg",3),
   LBSTRESN  = c(5,6,NA),
   stringsAsFactors=FALSE
   )
  
  expect_true(check_lb_lbstresn_missing(LB))
})

test_that("Function returns false when errors are present", {
  
  LB <- data.frame(
    USUBJID = c("Patient 1","Patient 2","Patient 3"),
    LBTEST   = "Test A",
    LBTESTCD   = "TA",
    LBDTC   = "2017-01-01",
    LBORRES = c("5","6","7"),
    LBSTRESC = c("5","6","7"),
    LBORRESU = rep("mg",3),
    LBSTRESN  = c(5,6,NA),
    stringsAsFactors=FALSE
  )
  
  LB$LBSTRESC[3] = ""
  LB$LBSTRESC[1] = ""
  LB$VISIT = "SCREENING"
  
  expect_false(check_lb_lbstresn_missing(LB))
  
})





test_that("Function returns false when expected column not present", {

  LB <- data.frame(
    USUBJID = c("Patient 1","Patient 2","Patient 3"),
    LBTEST   = "Test A",
    LBTESTCD   = "TA",
    LBDTC   = "2017-01-01",
    LBORRES = c("5","6","7"),
    LBSTRESC = c("5","6","7"),
    LBORRESU = rep("mg",3),
    LBSTRESN  = c(5,6,NA),
    stringsAsFactors=FALSE
  )
  
  LB$LBSTRESC=NULL
  LB$LBSTRESN=NULL
  
  expect_false(check_lb_lbstresn_missing(LB))
    
  })

