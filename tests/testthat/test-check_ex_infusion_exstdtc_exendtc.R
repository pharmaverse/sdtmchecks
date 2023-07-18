EX <- data.frame(
 STUDYID = 1,
 USUBJID = 1:12,
 EXTRT = "SOME DRUG",
 EXROUTE = "INTRAVENOUS",
 EXSTDTC = c("2017-01-01","2017-01-02","2017-01-01T14:36","2015","2017-02","2017"      ,""    ,
             "2017"   ,"2017-01-01T14:26","2017-01-01T14:26","2017-01-01T14","2017-01-01T14:26:01")
             ,
 EXENDTC = c("2017-01-01","2017-01-03","2017-01-01T14:35","2017","2017-01","2016-01-01","2000",
             "2017-02","2017-01-01"      ,"2017-01","2017-01-01T13","2017-01-02T14:26:02")
             ,
 EXOCCUR = "Y",
 VISIT = "CYCLE 1 DAY 1",
 stringsAsFactors=FALSE
)


test_that("Function returns false when no errors are present", {
  
  expect_false(check_ex_infusion_exstdtc_exendtc(EX))
  
})


EX2 <- data.frame(
  STUDYID = 1,
  USUBJID = 1:4,
  EXTRT = "SOME DRUG",
  EXROUTE = "INTRAVENOUS",
  EXSTDTC = c("2017-01-03", "", "2017-02-01T14:26", ""),
  EXENDTC = c("", "2017-02-03", "", "2017-02-02T14:26:02"),
  EXOCCUR = "Y",
  VISIT = "CYCLE 1 DAY 1",
  stringsAsFactors = FALSE
)


test_that("Function returns false when errors are present", {
  
  expect_false(check_ex_infusion_exstdtc_exendtc(EX2))
  
})

test_that("Function returns false when errors are present and EXOCCUR not present", {
  
  EX2b <- EX2
  EX2b$EXOCCUR = NULL
  
  expect_false(check_ex_infusion_exstdtc_exendtc(EX2b))
  
})

EX3 <- data.frame(
  STUDYID = 1,
  USUBJID = 1:3,
  EXTRT = "SOME DRUG",
  EXROUTE = "INTRAVENOUS",
  EXSTDTC = c("2017-01-01", "2017-01-01T14:26", "2017-01-01T14:26"),
  EXENDTC = c("2017-01-01", "2017-01-01", "2017-01"),
  EXOCCUR = "Y",
  VISIT = "CYCLE 1 DAY 1",
  stringsAsFactors=FALSE
)

test_that("Function returns true when no errors are present", {
  

  expect_true(check_ex_infusion_exstdtc_exendtc(EX3))
  
})


test_that("Function returns true when no errors are present", {
  
  EX3b <- EX3
  EX3b$EXOCCUR = NULL
  expect_true(check_ex_infusion_exstdtc_exendtc(EX3b))
  
})

test_that("Function returns false when required variable is missing", {
  
  EX4 <- EX3
  EX4$EXSTDTC = NULL
  expect_false(check_ex_infusion_exstdtc_exendtc(EX4))
  
})
