test_that("Test returns true when no errors present", {
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-02",
    SSTESTCD = "SURVSTAT",
    SSORRES  = c("DEAD","DEAD","ALIVE","DEAD","ALIVE"),
    VISIT = "WEEK 4"
  )
  
  
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = "2020-01-03"
  )
  
  expect_true(check_ss_ssdtc_alive_dm(SS, DM))
  
})
 

test_that("Test returns false when errors present", {
  
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-04",
    SSTESTCD = "SURVSTAT",
    SSORRES  = c("DEAD","DEAD","ALIVE","DEAD","ALIVE"),
    VISIT = "WEEK 4"
  )
  
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = c("2020-01-04", "2020-01-05", "2020-01-03", "2020-01-04", "2020-01-05")
  )
  
  expect_false(check_ss_ssdtc_alive_dm(SS, DM))
  
})

test_that("Test returns false when expected column not present - SS", {
  
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-04",
    SSTESTCD = "SURVSTAT",
    SSORRES  = c("DEAD","DEAD","ALIVE","DEAD","ALIVE"),
    VISIT = "WEEK 4"
  )
  
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = c("2020-01-04", "2020-01-05", "2020-01-03", "2020-01-04", "2020-01-05")
  )
  
  SS$SSTESTCD <- NULL
  expect_false(check_ss_ssdtc_alive_dm(SS, DM))
  
})

test_that("Test returns false when expected column not present - DM", {
  
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-04",
    SSTESTCD = "SURVSTAT",
    SSORRES  = c("DEAD","DEAD","ALIVE","DEAD","ALIVE"),
    VISIT = "WEEK 4"
  )
  
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = c("2020-01-04", "2020-01-05", "2020-01-03", "2020-01-04", "2020-01-05")
  )
  
  DM$DTHDTC <- NULL
  expect_false(check_ss_ssdtc_alive_dm(SS, DM))
  
})
