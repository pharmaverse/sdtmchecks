
EX <- data.frame(
 USUBJID = 1:10,
 EXTRT = 1:10,
 EXSTDTC = 1:10,
 EXDOSE = 1:10,
 EXOCCUR = as.character(c(rep("Y",5),rep("N",5))),
 EXDOSU = as.character(rep("mg",10))
)


test_that("function returns true when no errors are present", {
  
  expect_true(check_ex_exdosu(EX))
})

test_that("function returns false when errors are present", {
  
  EX0 <- EX
  
  EX0$EXDOSU[1] = ""
  EX0$EXDOSU[2] = "NA"
  EX0$EXDOSU[3] = NA
  
  expect_false(check_ex_exdosu(EX0))
})

test_that("function returns false when mandatory variable missing", {
  
  EX1 <- EX
  EX1$EXSTDTC = NULL
  
  expect_false(check_ex_exdosu(EX1))
})


test_that("function returns true when optional variable missing", {
  
  EX3 <- EX
  EX3$EXOCCUR = NULL
  
  expect_true(check_ex_exdosu(EX3))
})


