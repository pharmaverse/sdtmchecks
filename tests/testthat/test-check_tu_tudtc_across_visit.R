test_that("Returns true when no errors present", {
  
  TU <- data.frame(USUBJID = 1,
                   TUDTC = c(rep("2016-01-01",3), rep("2016-06-01",3), rep("2016-06-24",4)),
                   VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
                   stringsAsFactors=FALSE)
  
  expect_true(check_tu_tudtc_across_visit(TU))
  
}) 

test_that("Returns false when errors present", {
  
  TU <- data.frame(USUBJID = 1,
                   TUDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
                   VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
                   stringsAsFactors=FALSE)
  
  expect_false(check_tu_tudtc_across_visit(TU))
  
})


test_that("Returns true when no errors present among eligible Investigator records, only among IRF records", {
  
  TU <- data.frame(USUBJID = 1,
                   TUDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
                   VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
                   TUEVAL = "INDEPENDENT ASSESSOR",
                   stringsAsFactors=FALSE)
  
  expect_true(check_tu_tudtc_across_visit(TU))
  
})


test_that("Returns false when errors present and additional permissible variable shown", {
  
  TU <- data.frame(USUBJID = 1,
                   TUDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
                   VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
                   TUTESTCD = c(rep("TUMIDENT", 2), rep("OTHER", 2), rep("TUMIDENT", 2), rep("OTHER", 2), rep("TUMIDENT", 2)),
                   stringsAsFactors=FALSE)
  
  expect_false(check_tu_tudtc_across_visit(TU))
  
})


test_that("Returns false when expected column not present", {
  
  TU <- data.frame(USUBJID = 1,
                   TUDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
                   VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
                   stringsAsFactors=FALSE)
  
  TU$USUBJID <- NULL
  
  expect_false(check_tu_tudtc_across_visit(TU))
  
})
