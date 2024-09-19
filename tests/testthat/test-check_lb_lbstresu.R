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



test_that("Function returns true when no errors are present, DM is present with SITEID variable", {
  
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
  
  DM <- data.frame(
    USUBJID = 1:10,
    SITEID = 111:120,
    stringsAsFactors=FALSE
  )
  
  expect_true(check_lb_lbstresu(LB, DM))
})



test_that("Function returns false when errors are present, DM is present with SITEID variable", {
  
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
  
  DM <- data.frame(
    USUBJID = 1:10,
    SITEID = 111:120,
    stringsAsFactors=FALSE
  )
  
  LB$LBSTRESU[1]=""
  LB$LBSTRESU[2]="NA"
  LB$LBSTRESU[3]=NA
  
  expect_false(check_lb_lbstresu(LB, DM))
  
})



test_that("Function returns false when expected column not present, DM is present with SITEID variable", {
  
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
  
  DM <- data.frame(
    USUBJID = 1:10,
    SITEID = 111:120,
    stringsAsFactors=FALSE
  )
  
  LB$LBSTRESU=NULL
  
  expect_false(check_lb_lbstresu(LB, DM))
  
})



test_that("Function returns true when no errors are present, DM is present without SITEID variable", {
  
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
  
  DM2 <- data.frame(
    USUBJID = 1:10,
    stringsAsFactors=FALSE
  )
  
  expect_true(check_lb_lbstresu(LB, DM2))
})



test_that("Function returns false when errors are present, DM is present without SITEID variable", {
  
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
  
  DM2 <- data.frame(
    USUBJID = 1:10,
    stringsAsFactors=FALSE
  )
  
  expect_false(check_lb_lbstresu(LB, DM2))
  
})



test_that("Function returns false when expected column not present, DM is present without SITEID variable", {
  
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
  
  DM2 <- data.frame(
    USUBJID = 1:10,
    stringsAsFactors=FALSE
  )
  
  expect_false(check_lb_lbstresu(LB, DM2))
  
})



