test_that("Function returns true when no errors are present", {
  
  LB <- data.frame(
      USUBJID = 1:10,
      LBSTRESC = "5",
      LBSTRESN = 1:10,
      LBORRES = "5",
      LBSTRESU = "g/L",
      LBTESTCD = "ALB",
      LBDTC = 1:10,
      stringsAsFactors=FALSE
  )
  
  
  expect_true(check_lb_lbstresu(LB))
})

test_that("Function returns false when errors are present", {
  
  LB <- data.frame(
    USUBJID = 1:10,
    LBSTRESC = "5",
    LBSTRESN = 1:10,
    LBORRES = "5",
    LBSTRESU = "g/L",
    LBTESTCD = "ALB",
    LBDTC = 1:10,
    stringsAsFactors=FALSE
  )
  
 LB$LBSTRESU[1]=""
 LB$LBSTRESU[2]="NA"
 LB$LBSTRESU[3]=NA
  
  expect_false(check_lb_lbstresu(LB))
  
})



test_that("Function returns false when expected column not present", {

    LB <- data.frame(
      USUBJID = 1:10,
      LBSTRESC = "5",
      LBSTRESN = 1:10,
      LBORRES = "5",
      LBSTRESU = "g/L",
      LBTESTCD = "ALB",
      LBDTC = 1:10,
      stringsAsFactors=FALSE
    )
    
    LB$LBSTRESU=NULL
    
    expect_false(check_lb_lbstresu(LB))
    
  })

