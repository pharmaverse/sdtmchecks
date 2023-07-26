 
test_that("Returns true when no errors present", {
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-02",
    SSSTRESC  = c("DEAD","DEAD","ALIVE","DEAD","ALIVE"),
    VISIT = "DAY 10"
  )
  
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = "2020-01-02"
  )
  
  expect_true(check_ss_ssdtc_dead_dthdtc(SS, DM))
  
})

test_that("Returns false when errors present", {
  
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-02",
    SSSTRESC  = c("DEAD","DEAD","ALIVE","DEAD","ALIVE"),
    VISIT = "FOLLOW-UP"
  )
  
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = c("2020-01-01","2020-01-02","2020-01-03","2020-01-04","2020-01-02")
  )
  
  expect_false(check_ss_ssdtc_dead_dthdtc(SS, DM))
  
})

test_that("Returns false when expected column not present - 1", {
  
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-02",
    SSSTRESC  = c("DEAD","DEAD","ALIVE","DEAD","ALIVE"),
    VISIT = "DAY 10"
  )
  
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = "2020-01-02"
  )
  
  SS$USUBJID <- NULL
  
  expect_false(check_ss_ssdtc_dead_dthdtc(SS, DM))
})

test_that("Returns false when expected column not present - 2", {
  
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-02",
    SSSTRESC  = c("DEAD","DEAD","ALIVE","DEAD","ALIVE"),
    VISIT = "DAY 10"
  )
  
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = "2020-01-02"
  )
  
  DM$USUBJID <- NULL
  
  expect_false(check_ss_ssdtc_dead_dthdtc(SS, DM))
})
