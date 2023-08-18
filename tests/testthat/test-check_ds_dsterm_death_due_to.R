
test_that("Returns true when no errors present", {
  
  DS <- data.frame(
    STUDYID = 1,
    USUBJID = 1:4,
    DSTERM = c("DEATH DUE TO UNKNOWN",
               "DEATH DUE TO UNKNOWN",
               "DEATH DUE TO ADVERSE EVENT",
               "DEATH DUE TO UNKNOWN"),
    DSDECOD = "DEATH",
    DSDTC = "2017-01-01",
    DSSTDTC = "2017-01-01",
    stringsAsFactors=FALSE
  )
  
  expect_true(check_ds_dsterm_death_due_to(DS))
  
}) 

test_that("Returns false when errors present", {
  DS <- data.frame(
    STUDYID = 1,
    USUBJID = 1:4,
    DSTERM = c("DEATH DUE TO",
               "DEATH DUE TO ",
               "DEATH DUE TO ADVERSE EVENT",
               "DEATH DUE TO UNKNOWN"),
    DSDECOD = "DEATH",
    DSDTC = "2017-01-01",
    DSSTDTC = "2017-01-01",
    stringsAsFactors=FALSE
  )
  
  expect_false(check_ds_dsterm_death_due_to(DS))
})

test_that("Returns false when expected column not present", {
  DS <- data.frame(
    STUDYID = 1,
    USUBJID = 1:4,
    DSTERM = c("DEATH DUE TO UNKNOWN",
               "DEATH DUE TO UNKNOWN",
               "DEATH DUE TO ADVERSE EVENT",
               "DEATH DUE TO UNKNOWN"),
    DSDECOD = "DEATH",
    DSDTC = "2017-01-01",
    DSSTDTC = "2017-01-01",
    stringsAsFactors=FALSE
  )
  
  DS$DSDECOD <- NULL
  expect_false(check_ds_dsterm_death_due_to(DS))
})
