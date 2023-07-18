 
test_that("Returns true when no errors present - 1", {
  
  TU <- data.frame(
    USUBJID = "1001",
    TUDTC = "2020-05-05",
    VISIT = "C1D1",
    TUORRES = 1:10,
    TUSPID = "FORMNAME-R:19/L:19XXXX",
    TUEVAL = "INVESTIGATOR",
    TUTESTCD = "TUMIDENT",
    stringsAsFactors = FALSE
  )
  
  
  expect_true(check_tu_tudtc(TU))
})

test_that("Returns false when no errors present - 2", {
  
  TU <- data.frame(
    USUBJID = "1001",
    TUDTC = "2020-05-05",
    VISIT = "C1D1",
    TUORRES = 1:10,
    TUSPID = "FORMNAME-R:19/L:19XXXX",
    TUEVAL = "INVESTIGATOR",
    TUTESTCD = "TUMIDENT",
    stringsAsFactors = FALSE
  )
  
  #checks that it filters out data with missing TUEVAL
  TU$TUEVAL[1]=""
  TU$TUTESTCD=NULL
  
  expect_true(check_tu_tudtc(TU,preproc=roche_derive_rave_row))
  
})

test_that("Returns false when errors present - 1", {
  
  TU <- data.frame(
    USUBJID = "1001",
    TUDTC = "2020-05-05",
    VISIT = "C1D1",
    TUORRES = 1:10,
    TUSPID = "FORMNAME-R:19/L:19XXXX",
    TUEVAL = "INVESTIGATOR",
    TUTESTCD = "TUMIDENT",
    stringsAsFactors = FALSE
  )
  
  TU$TUDTC[1]=""
  TU$TUDTC[2]="NA"
  TU$TUDTC[3]=NA
  
  expect_false(check_tu_tudtc(TU,preproc=roche_derive_rave_row))
  
})


test_that("Returns false when errors present - 2", {
  
  TU <- data.frame(
    USUBJID = "1001",
    TUDTC = "2020-05-05",
    VISIT = "C1D1",
    TUORRES = 1:10,
    TUSPID = "FORMNAME-R:19/L:19XXXX",
    TUEVAL = "INVESTIGATOR",
    TUTESTCD = "TUMIDENT",
    stringsAsFactors = FALSE
  )
  
  TU$TUEVAL[2]="INDEPENDENT ASSESSOR"
  TU$TUEVAL[3]="INDEPENDENT ASSESSOR"
  TU$TUDTC[4]=""
  
  expect_false(check_tu_tudtc(TU))
})

test_that("Returns true when optional column not present", {
  
  TU <- data.frame(
    USUBJID = "1001",
    TUDTC = "2020-05-05",
    VISIT = "C1D1",
    TUORRES = 1:10,
    TUSPID = "FORMNAME-R:19/L:19XXXX",
    TUEVAL = "INVESTIGATOR",
    TUTESTCD = "TUMIDENT",
    stringsAsFactors = FALSE
  )
  
  
  TU$TUSPID=NULL
  expect_true(check_tu_tudtc(TU))
  
})

test_that("Returns false when expected column not present", {
  
  TU <- data.frame(
    USUBJID = "1001",
    TUDTC = "2020-05-05",
    VISIT = "C1D1",
    TUORRES = 1:10,
    TUSPID = "FORMNAME-R:19/L:19XXXX",
    TUEVAL = "INVESTIGATOR",
    TUTESTCD = "TUMIDENT",
    stringsAsFactors = FALSE
  )
  
  TU$VISIT=NULL
  expect_false(check_tu_tudtc(TU))
})

