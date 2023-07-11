test_that("Function returns true when no errors are present", {
  
  MH <- data.frame(USUBJID = LETTERS[1:5],
                   MHTERM = LETTERS[5:1],
                   MHSTDTC = c("2014", NA, "2014-01", "", "2014---02"),
                   stringsAsFactors = FALSE)
   
   MH=MH[1:3,]
  
  
  expect_true(check_mh_missing_month(MH))
})

test_that("Function returns false when errors are present", {
  
  MH <- data.frame(USUBJID = LETTERS[1:5],
                   MHTERM = LETTERS[5:1],
                   MHSTDTC = c("2014", NA, "2014-01", "", "2014---02"),
                   stringsAsFactors = FALSE)
  
  expect_false(check_mh_missing_month(MH))
})



test_that("Function returns false when expected column not present", {
  
  MH <- data.frame(USUBJID = LETTERS[1:5],
                   MHTERM = LETTERS[5:1],
                   MHSTDTC = c("2014", NA, "2014-01", "", "2014---02"),
                   stringsAsFactors = FALSE)
  
  MH$MHSTDTC=NULL
  
  expect_false(check_mh_missing_month(MH))
})

