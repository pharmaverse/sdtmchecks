test_that("Function returns true when no errors are present", {
  
  LB <- data.frame(
  USUBJID = c("Patient 1","Patient 2","Patient 3"),
  LBTEST   = "Test A",
  LBDTC   = "2017-01-01",
  LBORRES = c("5","3","7"),
  LBORRESU = rep("mg",3),
  LBSTRESC  = c("5","3","7"),
  LBSTRESN  = c(5,3,7),
  stringsAsFactors = FALSE
  )
  
  
  expect_true(check_lb_lbstresc_char(LB))
})

test_that("Function returns false when errors are present - 1", {
  
  LB <- data.frame(
  USUBJID = c("Patient 1","Patient 2","Patient 3"),
  LBTEST   = rep("Test A", 3),
  LBDTC   = "2017-01-01",
  LBORRES = c("5","3","<7"),
  LBORRESU = rep("mg",3),
  LBSTRESC  = c("5","3","<7"),
  LBSTRESN  = c(5,3,NA),
  stringsAsFactors = FALSE
  )
  
  expect_false(check_lb_lbstresc_char(LB))
  
})


test_that("Function returns false when errors are present - 2", {
  
  LB <- data.frame(
  USUBJID = c("Patient 1","Patient 2","Patient 3"),
  LBTEST   = rep("Test A", 3),
  LBDTC   = rep("2017-01-01", 3),
  LBORRES = c("5","BLQ","<7"),
  LBORRESU = rep("mg",3),
  LBSTRESC  = c("5","BLQ","<7"),
  LBSTRESN  = c(5,NA,NA),
  stringsAsFactors = FALSE
  )
  
  expect_false(check_lb_lbstresc_char(LB))
  
})










test_that("Function returns false when expected column not present", {

  LB <- data.frame(
    USUBJID = c("Patient 1","Patient 2","Patient 3"),
    LBTEST   = rep("Test A", 3),
    LBDTC   = rep("2017-01-01", 3),
    LBORRES = c("5","BLQ","<7"),
    LBORRESU = rep("mg",3),
    LBSTRESC  = c("5","BLQ","<7"),
    LBSTRESN  = c(5,NA,NA),
    stringsAsFactors = FALSE
  )
    
    LB$LBSTRESC=NULL
    
    expect_false(check_lb_lbstresc_char(LB))
    
  })

