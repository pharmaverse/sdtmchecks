EX <- data.frame(
  USUBJID = 1:3,
  EXSEQ   = 1:3,
  EXSTDTC = 1:3,
  EXTRT   = c(1,2,NA),
  EXOCCUR = "Y",
  EXDOSE  = 1:3,
  VISIT = c("CYCLE 1 DAY 1", "CYCLE 2 DAY 1", "CYCLE 3 DAY 1")
)



test_that("Function returns true when no errors are present", {
  
  expect_true(check_ex_exdose_exoccur(EX))
  
})




test_that("Function returns false when errors are present", {
  
  EX1 <- EX
  EX1$EXDOSE[3]=NA
  
  expect_false(check_ex_exdose_exoccur(EX1))
  
})

test_that("Function returns true when no errors are present when EXOCCUR = Y", {
  
  EX1b <- EX
  EX1b$EXDOSE[3]=NA
  EX1b$EXOCCUR[3]=''
  
  expect_true(check_ex_exdose_exoccur(EX1b))
  
})


test_that("Function returns false when true when non-mandatory variable missing", {
  
  EX2 <- EX
  EX2$VISIT=NULL
  
  expect_true(check_ex_exdose_exoccur(EX2))
  
})



test_that("Function returns false when true when non-mandatory variable missing", {
  
  EX2b <- EX
  EX2b$EXOCCUR=NULL
  
  expect_true(check_ex_exdose_exoccur(EX2b))
  
})


test_that("Function returns false when mandatory variable missing", {
  
  EX3 <- EX
  EX3$EXDOSE=NULL
  
  expect_false(check_ex_exdose_exoccur(EX3))
  
})
