test_that("Returns true when no errors present", {
  
  DM <- data.frame(
    USUBJID = 1:7,
    DTHFL = 1:7,
    DTHDTC = 1:7
  )
  
  DM$DTHFL[1] = ""
  DM$DTHDTC[1] = "2020-01-01"
  DM$DTHFL[2] = "N"
  DM$DTHDTC[2] = "2020-01-01"
  DM$DTHFL[3] = "Y"
  DM$DTHDTC[3] = "2020-01-01"
  DM$DTHFL[4] = "Y"
  DM$DTHDTC[4] = ""
  DM$DTHFL[5] = "N"
  DM$DTHDTC[5] = ""
  DM$DTHFL[6] = "Y"
  DM$DTHDTC[6] = "2020"
  DM$DTHFL[7] = ""
  DM$DTHDTC[7] = ""
  
  expect_true(check_dm_dthfl_dthdtc(DM[3,]))
  
})

test_that("Returns false when errors present", {
  DM <- data.frame(
    USUBJID = 1:7,
    DTHFL = 1:7,
    DTHDTC = 1:7
  )
  
  DM$DTHFL[1] = ""
  DM$DTHDTC[1] = "2020-01-01"
  DM$DTHFL[2] = "N"
  DM$DTHDTC[2] = "2020-01-01"
  DM$DTHFL[3] = "Y"
  DM$DTHDTC[3] = "2020-01-01"
  DM$DTHFL[4] = "Y"
  DM$DTHDTC[4] = ""
  DM$DTHFL[5] = "N"
  DM$DTHDTC[5] = ""
  DM$DTHFL[6] = "Y"
  DM$DTHDTC[6] = "2020"
  DM$DTHFL[7] = ""
  DM$DTHDTC[7] = ""
  
  expect_false(check_dm_dthfl_dthdtc(DM))
})

test_that("Returns false when expected column not present", {
  
  DM <- data.frame(
    USUBJID = 1:7,
    DTHFL = 1:7,
    DTHDTC = 1:7
  )
  
  DM$DTHFL[1] = ""
  DM$DTHDTC[1] = "2020-01-01"
  DM$DTHFL[2] = "N"
  DM$DTHDTC[2] = "2020-01-01"
  DM$DTHFL[3] = "Y"
  DM$DTHDTC[3] = "2020-01-01"
  DM$DTHFL[4] = "Y"
  DM$DTHDTC[4] = ""
  DM$DTHFL[5] = "N"
  DM$DTHDTC[5] = ""
  DM$DTHFL[6] = "Y"
  DM$DTHDTC[6] = "2020"
  DM$DTHFL[7] = ""
  DM$DTHDTC[7] = ""
  
  DM$DTHDTC <- NULL
  expect_false(check_dm_dthfl_dthdtc(DM[3,]))
  
})

