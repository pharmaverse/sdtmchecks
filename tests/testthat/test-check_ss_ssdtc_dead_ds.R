test_that("Returns true when no errors present", {
  
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-02",
    SSSTRESC  = c(rep("DEAD", 5)),
    VISIT = "FOLLOW-UP"
  )
  
  DS <- data.frame(
    USUBJID = 1:5,
    DSSTDTC  = c("2020-01-02","2020-01-02", "2020-01-01", "2020-01-02", "2020-01-01"),
    DSDECOD = c(rep('DEATH', 5)),
    DSCAT   = c(rep("DISPOSITION EVENT", 5))
  )
  
  expect_true(check_ss_ssdtc_dead_ds(SS, DS))
  
})

#missing value
test_that("Returns false when errors present - 1", {
  
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-02",
    SSSTRESC  = c("DEAD","DEAD","ALIVE","DEAD","ALIVE"),
    VISIT = "FOLLOW-UP"
  )
  
  
  DS <- data.frame(
    USUBJID = 1:5,
    DSSTDTC  = c("2020-01-02","2020-01-02", "2020-01-01", "2020-01-03", "2020-01-01"),
    DSDECOD = c(rep('DEATH', 5)),
    DSCAT   = c("OTHER EVENT", rep("DISPOSITION EVENT", 4))
  )
  
  expect_false(check_ss_ssdtc_dead_ds(SS, DS))
  
})

#assessment date less than disposition event date
test_that("Returns false when errors present - 2", {
  
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-02",
    SSSTRESC  = c( rep("DEAD", 5)),
    VISIT = "FOLLOW-UP"
  )
  
  
  DS <- data.frame(
    USUBJID = 1:5,
    DSSTDTC  = c("2020-01-02","2020-01-02", "2020-01-01", "2020-01-03", "2020-01-01"),
    DSDECOD = c(rep('DEATH', 5)),
    DSCAT   = c(rep("DISPOSITION EVENT", 5))
  )
  
  expect_false(check_ss_ssdtc_dead_ds(SS, DS))
  
})


test_that("Returns false when missing expected column - 1", {
  
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-02",
    SSSTRESC  = c( rep("DEAD", 5)),
    VISIT = "FOLLOW-UP"
  )
  
  
  DS <- data.frame(
    USUBJID = 1:5,
    DSSTDTC  = c("2020-01-02","2020-01-02", "2020-01-01", "2020-01-02", "2020-01-01"),
    DSDECOD = c(rep('DEATH', 5)),
    DSCAT   = c(rep("DISPOSITION EVENT", 5))
  )
  
  SS$USUBJID <- NULL
  
  expect_false(check_ss_ssdtc_dead_ds(SS, DS))
  
})


test_that("Returns false when missing expected column - 2", {
  
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-02",
    SSSTRESC  = c( rep("DEAD", 5)),
    VISIT = "FOLLOW-UP"
  )
  
  
  DS <- data.frame(
    USUBJID = 1:5,
    DSSTDTC  = c("2020-01-02","2020-01-02", "2020-01-01", "2020-01-02", "2020-01-01"),
    DSDECOD = c(rep('DEATH', 5)),
    DSCAT   = c(rep("DISPOSITION EVENT", 5))
  )
  
  DS$USUBJID <- NULL
  
  expect_false(check_ss_ssdtc_dead_ds(SS, DS))
  
})


 
 