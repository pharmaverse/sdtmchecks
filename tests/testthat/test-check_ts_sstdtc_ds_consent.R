test_that("Returns true when no errors present", {
  
  TS4 <- data.frame(
    STUDYID = 1,
    TSPARMCD = "SSTDTC",
    TSPARM = "Study Start Date",
    TSVAL = "2020-01-02",
    TSVAL1 = "",
    TSVAL2 = ""
  )
  
  DS1 <- data.frame(
    USUBJID = c(1,1,2,3,4),
    DSCAT   = rep("PROTOCOL MILESTONE", 5),
    DSSCAT   = rep("PROTOCOL MILESTONE", 5),
    DSDECOD = c("INFORMED CONSENT OBTAINED", "OTHER", "PHYSICIAN DECISION",
                "OTHER", "INFORMED CONSENT OBTAINED"),
    DSSTDTC = c("2021-01-01", "2021-01-02", "2021-01-02", "2021-01-02", "2020-01-02"),
    stringsAsFactors = FALSE
  )
  
  expect_true(check_ts_sstdtc_ds_consent(DS=DS1, TS=TS4))
  
  
}) 

test_that("Returns false when errors present - 1", {
  
  TS1 <- data.frame(
    STUDYID = 1,
    TSPARMCD = "SSTDTC",
    TSPARM = "Study Start Date",
    TSVAL = "2017-01-01",
    TSVAL1 = "",
    TSVAL2 = ""
  )
  
  DS1 <- data.frame(
    USUBJID = c(1,1,2,3,4),
    DSCAT   = rep("PROTOCOL MILESTONE", 5),
    DSSCAT   = rep("PROTOCOL MILESTONE", 5),
    DSDECOD = c("INFORMED CONSENT OBTAINED", "OTHER", "PHYSICIAN DECISION",
                "OTHER", "INFORMED CONSENT OBTAINED"),
    DSSTDTC = c("2021-01-01", "2021-01-02", "2021-01-02", "2021-01-02", "2020-01-02"),
    stringsAsFactors = FALSE
  )
  
  expect_false(check_ts_sstdtc_ds_consent(DS=DS1, TS=TS1))
  
})


test_that("Returns false when errors present - 2", {
  
  TS2 <- data.frame(
    STUDYID = 2,
    TSPARMCD = "AEDICT",
    TSPARM = "Study Start Date",
    TSVAL = "MedDRA v23.0",
    TSVAL1 = ""
  )
  
  DS1 <- data.frame(
    USUBJID = c(1,1,2,3,4),
    DSCAT   = rep("PROTOCOL MILESTONE", 5),
    DSSCAT   = rep("PROTOCOL MILESTONE", 5),
    DSDECOD = c("INFORMED CONSENT OBTAINED", "OTHER", "PHYSICIAN DECISION",
                "OTHER", "INFORMED CONSENT OBTAINED"),
    DSSTDTC = c("2021-01-01", "2021-01-02", "2021-01-02", "2021-01-02", "2020-01-02"),
    stringsAsFactors = FALSE
  )
  
  
  expect_false(check_ts_sstdtc_ds_consent(DS=DS1, TS=TS2))
  
})

test_that("Returns false when errors present - 2", {
  
  TS3 <- data.frame(
    STUDYID = 3,
    TSPARMCD = "SSTDTC",
    TSPARM = "Study Start Date",
    TSVAL = ""
  )
  
  DS1 <- data.frame(
    USUBJID = c(1,1,2,3,4),
    DSCAT   = rep("PROTOCOL MILESTONE", 5),
    DSSCAT   = rep("PROTOCOL MILESTONE", 5),
    DSDECOD = c("INFORMED CONSENT OBTAINED", "OTHER", "PHYSICIAN DECISION",
                "OTHER", "INFORMED CONSENT OBTAINED"),
    DSSTDTC = c("2021-01-01", "2021-01-02", "2021-01-02", "2021-01-02", "2020-01-02"),
    stringsAsFactors = FALSE
  )
  
  
  expect_false(check_ts_sstdtc_ds_consent(DS=DS1, TS=TS3))
})

test_that("Returns false when errors present - 3", {
  
  TS1 <- data.frame(
    STUDYID = 1,
    TSPARMCD = "SSTDTC",
    TSPARM = "Study Start Date",
    TSVAL = "2017-01-01",
    TSVAL1 = "",
    TSVAL2 = ""
  )
  
  TS4 <- data.frame(
    STUDYID = 1,
    TSPARMCD = "SSTDTC",
    TSPARM = "Study Start Date",
    TSVAL = "2020-01-02",
    TSVAL1 = "",
    TSVAL2 = ""
  )
  
  DS1 <- data.frame(
    USUBJID = c(1,1,2,3,4),
    DSCAT   = rep("PROTOCOL MILESTONE", 5),
    DSSCAT   = rep("PROTOCOL MILESTONE", 5),
    DSDECOD = c("INFORMED CONSENT OBTAINED", "OTHER", "PHYSICIAN DECISION",
                "OTHER", "INFORMED CONSENT OBTAINED"),
    DSSTDTC = c("2021-01-01", "2021-01-02", "2021-01-02", "2021-01-02", "2020-01-02"),
    stringsAsFactors = FALSE
  )
  
  TS5 = rbind(TS1, TS4)
  
  expect_false(check_ts_sstdtc_ds_consent(DS=DS1, TS=TS5))
  
})


test_that("Returns false when errors present - 4", {
  
  TS6 <- data.frame(
    STUDYID = 1,
    TSPARMCD = "SSTDTC",
    TSPARM = "Study Start Date",
    TSVAL = "2020-01",
    TSVAL1 = "",
    TSVAL2 = ""
  )
  
  DS1 <- data.frame(
    USUBJID = c(1,1,2,3,4),
    DSCAT   = rep("PROTOCOL MILESTONE", 5),
    DSSCAT   = rep("PROTOCOL MILESTONE", 5),
    DSDECOD = c("INFORMED CONSENT OBTAINED", "OTHER", "PHYSICIAN DECISION",
                "OTHER", "INFORMED CONSENT OBTAINED"),
    DSSTDTC = c("2021-01-01", "2021-01-02", "2021-01-02", "2021-01-02", "2020-01-02"),
    stringsAsFactors = FALSE
  )
  
  
  expect_false(check_ts_sstdtc_ds_consent(DS=DS1, TS=TS6))
})

test_that("Returns false when expected column not present - 1", {
  
  TS4 <- data.frame(
    STUDYID = 1,
    TSPARMCD = "SSTDTC",
    TSPARM = "Study Start Date",
    TSVAL = "2020-01-02",
    TSVAL1 = "",
    TSVAL2 = ""
  )
  
  DS1 <- data.frame(
    USUBJID = c(1,1,2,3,4),
    DSCAT   = rep("PROTOCOL MILESTONE", 5),
    DSSCAT   = rep("PROTOCOL MILESTONE", 5),
    DSDECOD = c("INFORMED CONSENT OBTAINED", "OTHER", "PHYSICIAN DECISION",
                "OTHER", "INFORMED CONSENT OBTAINED"),
    DSSTDTC = c("2021-01-01", "2021-01-02", "2021-01-02", "2021-01-02", "2020-01-02"),
    stringsAsFactors = FALSE
  )
  
  TS4$TSPARMCD <- NULL
  
  expect_false(check_ts_sstdtc_ds_consent(DS=DS1, TS=TS4))
})

test_that("Returns false when expected column not present - 2", {
  
  TS4 <- data.frame(
    STUDYID = 1,
    TSPARMCD = "SSTDTC",
    TSPARM = "Study Start Date",
    TSVAL = "2020-01-02",
    TSVAL1 = "",
    TSVAL2 = ""
  )
  
  DS1 <- data.frame(
    USUBJID = c(1,1,2,3,4),
    DSCAT   = rep("PROTOCOL MILESTONE", 5),
    DSSCAT   = rep("PROTOCOL MILESTONE", 5),
    DSDECOD = c("INFORMED CONSENT OBTAINED", "OTHER", "PHYSICIAN DECISION",
                "OTHER", "INFORMED CONSENT OBTAINED"),
    DSSTDTC = c("2021-01-01", "2021-01-02", "2021-01-02", "2021-01-02", "2020-01-02"),
    stringsAsFactors = FALSE
  )
  
  DS1$DSSCAT <- NULL
  
  expect_false(check_ts_sstdtc_ds_consent(DS=DS1, TS=TS4))
})

