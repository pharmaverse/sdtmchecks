
EX <- data.frame(USUBJID = 101:102,
                EXTRT = rep(c("A", "B"), 5),
                EXSTDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                VISITNUM = rep(1:5,2),
                VISIT = rep(c("Cycle 1", "Cycle 2", "Cycle 3", "Cycle 4", "uNscheDuledd"), 2),
                stringsAsFactors = FALSE)


test_that("Function returns true when no errors are present", {
  
  expect_true(check_ex_exstdtc_visit_ordinal_error(EX))
  
})

test_that("Function returns false when errors are present (earlier date)", {
  
  EX1 <- EX
  EX1$EXSTDTC[EX1$USUBJID == 101 & EX1$VISIT == "Cycle 4"] <- "2017-01-10T08:25"
  EX1$EXSTDTC[EX1$USUBJID == 102 & EX1$VISIT == "Cycle 2"] <- "2017-01-01T06:25"
  
  
  expect_false(check_ex_exstdtc_visit_ordinal_error(EX1))
  
})

test_that("Function returns false when errors are present (same date, but different time)", {
  
  EX2 <- EX
  EX2$EXSTDTC[EX2$USUBJID == 101 & EX2$VISIT == "Cycle 5"] <- "2017-01-10T08:25"
  EX2$EXSTDTC[EX2$USUBJID == 102 & EX2$VISIT == "Cycle 3"] <- "2017-01-01T06:25"  
  
  expect_false(check_ex_exstdtc_visit_ordinal_error(EX2))
  
})



test_that("Function returns true when no errors are present in records not starting with 'UNSCHED'", {
  
  EX2 <- EX
  EX2$EXSTDTC[EX2$USUBJID == 101 & EX2$VISIT == "UNSCHEDULED 1.03"] <- "2017-01-10T08:25"
  EX2$EXSTDTC[EX2$USUBJID == 102 & EX2$VISIT == "UNSCHEDULED 1.01"] <- "2017-01-01T06:25"  
  
  expect_true(check_ex_exstdtc_visit_ordinal_error(EX2))
  
})
