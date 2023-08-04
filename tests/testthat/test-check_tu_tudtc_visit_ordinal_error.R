
test_that("Returns true when no error present", {
  
  TU <- data.frame(USUBJID = 101:102,
                   TUORRES = rep(c("NEW", "TARGET"), 5),
                   TULOC=rep(c("BONE","LIVER"),5),
                   TUDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = rep(1:5,2),
                   VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "Visit 4","VIsit 5"), 2),
                   TUEVAL="INVESTIGATOR",
                   stringsAsFactors = FALSE)
  
  expect_true(check_tu_tudtc_visit_ordinal_error(TU))
  
})

test_that("Returns false when no error present and TUEVAL = NA", {
  
  TU <- data.frame(USUBJID = 101:102,
                   TUORRES = rep(c("NEW", "TARGET"), 5),
                   TULOC=rep(c("BONE","LIVER"),5),
                   TUDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = rep(1:5,2),
                   VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "Visit 4","VIsit 5"), 2),
                   TUEVAL = NA,
                   stringsAsFactors = FALSE)
  
  expect_false(check_tu_tudtc_visit_ordinal_error(TU))
  
})
 

test_that("Returns false when no error present and no TUEVAL variable present", {
  
  TU <- data.frame(USUBJID = 101:102,
                   TUORRES = rep(c("NEW", "TARGET"), 5),
                   TULOC=rep(c("BONE","LIVER"),5),
                   TUDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = rep(1:5,2),
                   VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "Visit 4","VIsit 5"), 2),
                   stringsAsFactors = FALSE)
  
  expect_false(check_tu_tudtc_visit_ordinal_error(TU))
  
})

test_that("Returns false when errors present - 1", {
  
  TU <- data.frame(USUBJID = 101:102,
                   TUORRES = rep(c("NEW", "TARGET"), 5),
                   TULOC=rep(c("BONE","LIVER"),5),
                   TUDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = rep(1:5,2),
                   VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "Visit 4","VIsit 5"), 2),
                   TUEVAL="INVESTIGATOR",
                   stringsAsFactors = FALSE)
  
  # adding cases with earler date
  TU$TUDTC[TU$USUBJID == 101 & TU$VISIT == "Visit 4"] <- "2017-01-10T08:25"
  TU$TUDTC[TU$USUBJID == 102 & TU$VISIT == "Visit 2"] <- "2017-01-01T06:25"
  
  expect_false(check_tu_tudtc_visit_ordinal_error(TU))
})

test_that("Returns false when errors present - 2", {
  
  TU <- data.frame(USUBJID = 101:102,
                   TUORRES = rep(c("NEW", "TARGET"), 5),
                   TULOC=rep(c("BONE","LIVER"),5),
                   TUDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = rep(1:5,2),
                   VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "Visit 4","VIsit 5"), 2),
                   TUEVAL="INVESTIGATOR",
                   stringsAsFactors = FALSE)
  
  # adding cases with duplicated date
  TU$TUDTC[TU$USUBJID == 101 & TU$VISIT == "Visit 5"] <- "2017-01-10T08:25"
  TU$TUDTC[TU$USUBJID == 102 & TU$VISIT == "Visit 3"] <- "2017-01-01T06:25"
  
  expect_false(check_tu_tudtc_visit_ordinal_error(TU))
})

test_that("Returns false when expected variable (USUBJID) not present", {
  
  TU <- data.frame(USUBJID = 101:102,
                   TUORRES = rep(c("NEW", "TARGET"), 5),
                   TULOC=rep(c("BONE","LIVER"),5),
                   TUDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = rep(1:5,2),
                   VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "Visit 4","VIsit 5"), 2),
                   TUEVAL="INVESTIGATOR",
                   stringsAsFactors = FALSE)
  
  TU$USUBJID <- NULL
  
  expect_false(check_tu_tudtc_visit_ordinal_error(TU))
})



test_that("Returns false when no Investigator (INV) records", {
  
  TU <- data.frame(USUBJID = 101:102,
                   TUORRES = rep(c("NEW", "TARGET"), 5),
                   TULOC=rep(c("BONE","LIVER"),5),
                   TUDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = rep(1:5,2),
                   VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "Visit 4","VIsit 5"), 2),
                   TUEVAL="INDEPENDENT ASSESSOR",
                   TUEVALID = "ONCOLOGIST",
                   stringsAsFactors = FALSE)
  
  expect_false(check_tu_tudtc_visit_ordinal_error(TU))
})


test_that("Returns false when VISITNUM exists but only a single value (VISITNUM = 1)", {
  
  TU <- data.frame(USUBJID = 101:102,
                   TUORRES = rep(c("NEW", "TARGET"), 5),
                   TULOC=rep(c("BONE","LIVER"),5),
                   TUDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = 1,
                   VISIT = "SCREENING",
                   TUEVAL="INVESTIGATOR", 
                   stringsAsFactors = FALSE)
  
  expect_false(check_tu_tudtc_visit_ordinal_error(TU))
})



test_that("Returns false when VISITNUM exists but only a single value (VISITNUM = 999)", {
  
  TU <- data.frame(USUBJID = 101:102,
                   TUORRES = rep(c("NEW", "TARGET"), 5),
                   TULOC=rep(c("BONE","LIVER"),5),
                   TUDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = 999,
                   VISIT = "UNSCHEDULED",
                   TUEVAL="INVESTIGATOR",  
                   stringsAsFactors = FALSE)
  
  expect_false(check_tu_tudtc_visit_ordinal_error(TU))
})



test_that("Returns false when VISITNUM exists but only a single value (VISITNUM = 2) and VISIT = NOT UNSCHEDULED", {
  
  TU <- data.frame(USUBJID = 101:102,
                   TUORRES = rep(c("NEW", "TARGET"), 5),
                   TULOC=rep(c("BONE","LIVER"),5),
                   TUDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = 2,
                   VISIT = "NOT UNSCHEDULED",
                   TUEVAL="INVESTIGATOR",
                   stringsAsFactors = FALSE)

  expect_false(check_tu_tudtc_visit_ordinal_error(TU))
})




test_that("Returns false when VISITNUM exists but only a single value (VISITNUM = 1.4) and VISIT = CYCLE 2 DAY 1", {
  
  TU <- data.frame(USUBJID = 101:102,
                   TUORRES = rep(c("NEW", "TARGET"), 5),
                   TULOC=rep(c("BONE","LIVER"),5),
                   TUDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = 1.4,
                   VISIT = "CYCLE 2 DAY 1",
                   TUEVAL = "INVESTIGATOR",
                   TUEVALID = NA,
                   stringsAsFactors = FALSE)
  
  expect_false(check_tu_tudtc_visit_ordinal_error(TU))
})

test_that("Returns false when VISITNUM exists but only a single value (VISITNUM = 4) and VISIT = CYCLE 2 DAY 1", {
  
  TU <- data.frame(USUBJID = 101:102,
                   TUORRES = rep(c("NEW", "TARGET"), 5),
                   TULOC=rep(c("BONE","LIVER"),5),
                   TUDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM = 4,
                   VISIT = "CYCLE 2 DAY 1",
                   TUEVAL="INVESTIGATOR",
                   TUEVALID = NA,
                   stringsAsFactors = FALSE)
  
  expect_false(check_tu_tudtc_visit_ordinal_error(TU))
})


test_that("Returns false when VISITNUM exists but only a single non-missing value (VISITNUM = 1.4)", {
  
  TU <- data.frame(USUBJID = 101:102,
                   TUORRES = rep(c("NEW", "TARGET"), 5),
                   TULOC=rep(c("BONE","LIVER"),5),
                   TUDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                   VISITNUM =1.4,
                   VISIT = "UNSCHEDULED",
                   TUEVAL="INVESTIGATOR",
                   stringsAsFactors = FALSE) 
  
  expect_false(check_tu_tudtc_visit_ordinal_error(TU))
})


