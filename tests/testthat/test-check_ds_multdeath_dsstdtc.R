test_that("function returns false when errors are present", {
  
  DS_error1 <- data.frame(STUDYID = rep(1, 6),
                          USUBJID = c(1, 1, 1, 2, 1,1),
                          DSDECOD = c("DEATH", "DEATH", rep("", 2),"DEATH","DEATH"),
                          DSSCAT = LETTERS[1:6],
                          DSSTDTC = c("", "2016-01-01", "", "", "2016-01-02","2016-01-01"),
                          stringsAsFactors = FALSE)
  
  expect_false(check_ds_multdeath_dsstdtc(DS_error1))
  
})


test_that("function returns false when errors are present", {
  
  
  DS_error2 <- data.frame(STUDYID = rep(1, 6),
                          USUBJID = c(1, 1, 1, 2, 1,1),
                          DSDECOD = c("DEATH", "DEATH", rep("", 2),"DEATH","DEATH"),
                          DSSCAT = LETTERS[1:6],
                          DSSTDTC = c("", "2016-01", "", "", "2016-01-01","2016-01-01"),
                          stringsAsFactors = FALSE)
  
  expect_false(check_ds_multdeath_dsstdtc(DS_error2))
  
})



test_that("function returns true when no errors are present - even if USUBJID has a missing death date in one record but non-missing death date in another", {
  
  
  DS_noerror2 <- data.frame(STUDYID = rep(1, 6),
                            USUBJID = c(1, 1, 1, 2, 1,1),
                            DSDECOD = c("DEATH", "DEATH", rep("", 2),"DEATH","DEATH"),
                            DSSCAT = LETTERS[1:6],
                            DSSTDTC = c("", "2016-01", "", "", "2016-01-01","2016-01-01"),
                            stringsAsFactors = FALSE)
  
  expect_false(check_ds_multdeath_dsstdtc(DS_noerror2))
  
})


test_that("function returns true when no errors are present", {
  
  DS_noerror <- data.frame(STUDYID = rep(1, 6),
                           USUBJID = c(1, 2, 3, 4, 5, 6),
                           DSDECOD = c("DEATH", "DEATH", rep("", 2),"DEATH","DEATH"),
                           DSSCAT = LETTERS[1:6],
                           DSSTDTC = c("", "2016-01-01", "", "", "2016-01-01","2016-01-01"),
                           stringsAsFactors = FALSE)
  
  expect_true(check_ds_multdeath_dsstdtc(DS_noerror))
  
})
