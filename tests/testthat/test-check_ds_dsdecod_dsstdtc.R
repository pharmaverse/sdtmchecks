test_that("Returns true when no errors present", {
  DS <- data.frame(STUDYID = rep(1, 5),
                   USUBJID = c(1, 1, 1, 2, 3),
                   DSDECOD = c("DEATH", "DEATH", rep("", 3)),
                   DSSCAT = LETTERS[1:5],
                   DSSTDTC = c("", "2016-01-01", "", "", "2016-01-02"),
                   stringsAsFactors = FALSE)
  
  expect_true(check_ds_dsdecod_dsstdtc(DS))
})

test_that("Returns false when errors present", {
  DS <- data.frame(STUDYID = rep(1, 5),
                   USUBJID = c(1, 1, 1, 2, 3),
                   DSDECOD = c("DEATH", "DEATH", rep("", 3)),
                   DSSCAT = LETTERS[1:5],
                   DSSTDTC = c("", "2016-01-01", "", "", "2016-01-02"),
                   stringsAsFactors = FALSE)
  
  DS$DSSTDTC[2] <- ""
  
  expect_false(check_ds_dsdecod_dsstdtc(DS))
})

test_that("Returns false when expected column not present", {
  
  DS <- data.frame(STUDYID = rep(1, 5),
                   USUBJID = c(1, 1, 1, 2, 3),
                   DSDECOD = c("DEATH", "DEATH", rep("", 3)),
                   DSSCAT = LETTERS[1:5],
                   DSSTDTC = c("", "2016-01-01", "", "", "2016-01-02"),
                   stringsAsFactors = FALSE)
  DS$USUBJID <- NULL
  
  expect_false(check_ds_dsdecod_dsstdtc(DS))
})
