
test_that("Returns true when no errors present", {
  
  TU <- data.frame(
    USUBJID = 1:10,
    TUDTC = 1:10,
    VISIT = "C1D1",
    TUORRES = "TARGET",
    TULOC = "LIVER",
    TUSPID = "FORMNAME-R:19/L:19XXXX",
    stringsAsFactors=FALSE
  )
  
  expect_true(check_tu_tuloc_missing(TU))
  
})

test_that("Returns false when errors present", {
  
  TU <- data.frame(
    USUBJID = 1:10,
    TUDTC = 1:10,
    VISIT = "C1D1",
    TUORRES = "TARGET",
    TULOC = "LIVER",
    TUSPID = "FORMNAME-R:19/L:19XXXX",
    stringsAsFactors=FALSE
  )
  
  TU$TULOC[1] = "NA"
  TU$TULOC[2] = ""
  TU$TULOC[3] = NA
  
  expect_false(check_tu_tuloc_missing(TU,preproc=roche_derive_rave_row))
  
})

test_that("Returns false when expected variable not present", {
  
  TU <- data.frame(
    USUBJID = 1:10,
    TUDTC = 1:10,
    VISIT = "C1D1",
    TUORRES = "TARGET",
    TULOC = "LIVER",
    TUSPID = "FORMNAME-R:19/L:19XXXX",
    stringsAsFactors=FALSE
  )
  
  
  TU$VISIT <- NULL
  
  expect_false(check_tu_tuloc_missing(TU))
})
