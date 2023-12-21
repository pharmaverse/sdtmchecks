
# test_that("Returns true when no errors present", {
#   
#   t <-  Sys.Date()
#   y <-  substring(t,1,4)
#   
#   # Work out the main MedDRA version of the year; start on version 24.0
#   
#   meddra_version <- as.numeric(y) - 2021 + 24
#   
#   TS1 <- data.frame(
#     STUDYID = 1,
#     TSPARMCD = "AEDICT",
#     TSVAL = paste("MedDRA", paste0(meddra_version, ".0")),
#     TSVAL2 = ""
#   )
#   
#   expect_true(check_ts_aedict(TS1))
#   
# })

test_that("Returns false when errors present - 1", {
  
  TS1 <- data.frame(
    STUDYID = 1,
    TSPARMCD = "AEDICT",
    TSVAL = "MedDRA 22.0",
    TSVAL2 = ""
  )
  
  expect_false(check_ts_aedict(TS1))
})

test_that("Returns false when errors present - 2", {
  
  TS2 <- data.frame(
    STUDYID = 2,
    TSPARMCD = "AEDICT",
    TSVAL = "",
    TSVAL1 = "meddra v22.0"
  )
  
  expect_false( check_ts_aedict(TS2))
  
})

test_that("Returns false when errors present - 3", {
  
  TS3 <- data.frame(
    STUDYID = 3,
    TSPARMCD = "AEDICT",
    TSVAL = ""
  )
  
  expect_false(check_ts_aedict(TS3))
  
})

test_that("Returns false when errors present - 4", {
  
  TS4 <-data.frame(
    STUDYID = 4,
    TSPARMCD = "CMDICT",
    TSVAL = ""
  )
  
  expect_false(check_ts_aedict(TS4))
  
})

test_that("Returns false when errors present - 5", {
  
  TS5 <- data.frame(
    STUDYID = 1,
    TSPARMCD = "AEDICT",
    TSVAL = "meddra 24.0",
    TSVAL2 = ""
  )
  
  expect_false(check_ts_aedict(TS5))
  
})

test_that("Returns false when errors present - 6", {
  
  TS6 <- data.frame(
    STUDYID = 1,
    TSPARMCD = "AEDICT",
    TSVAL = "    meddra    23.0   ",
    TSVAL2 = ""
  )
  
  expect_false(check_ts_aedict(TS6))
  
})


test_that("Returns false when errors present - 7", {
  
  
  TS1 <- data.frame(
    STUDYID = 1,
    TSPARMCD = "AEDICT",
    TSVAL = "MedDRA 22.0",
    TSVAL2 = ""
  )
  
  expect_false(check_ts_aedict(rbind(TS1,TS1)))
  
})


test_that("Returns false when expected column not present", {
  
  t <-  Sys.Date()
  y <-  substring(t,1,4)
  
  # Work out the main MedDRA version of the year; start on version 24.0
  
  meddra_version <- as.numeric(y) - 2021 + 24
  
  TS1 <- data.frame(
    STUDYID = 1,
    TSPARMCD = "AEDICT",
    TSVAL = paste("MedDRA", paste0(meddra_version, ".0")),
    TSVAL2 = ""
  )
  
  TS1$TSVAL <- NULL
  
  expect_false(check_ts_aedict(TS1))
  
})



