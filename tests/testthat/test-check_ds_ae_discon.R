test_that("Returns true when no errors present - 1", {
  
  AE <- data.frame(
    USUBJID = 1:5,
    AESTDTC = "01JAN2017",
    AETERM  = c("AE1","AE2","AE3","AE4","AE5"),
    AEDECOD = c("AE1","AE2","AE3","AE4","AE5"),
    AEACN = c("DOSE REDUCED", "DOSE REDUCED", "DOSE NOT CHANGED",
              "DOSE NOT CHANGED", "NOT APPLICABLE"),
    stringsAsFactors = FALSE
  )
  
  
  DS <- data.frame(
    USUBJID = 1:5,
    DSSPID  = c('XXXDISCTXXXXX'),
    DSSTDTC = '01JAN2017',
    DSCAT   = rep("DISPOSITION EVENT", 5),
    DSSCAT   = rep("TX FORM", 5),
    DSDECOD = c("PHYSICIAN DECISION", "OTHER", "PHYSICIAN DECISION", "OTHER", "DEATH"),
    stringsAsFactors = FALSE
  )
  
  # no case
  expect_true(check_ds_ae_discon(DS, AE))
  
}) 

test_that("Returns true when no errors present - 2", {
  
  DS <- data.frame(
    USUBJID = 1:5,
    DSSPID  = c('XXXDISCTXXXXX'),
    DSSTDTC = '01JAN2017',
    DSCAT   = rep("DISPOSITION EVENT", 5),
    DSSCAT   = rep("TX FORM", 5),
    DSDECOD = c("PHYSICIAN DECISION", "OTHER", "PHYSICIAN DECISION", "OTHER", "DEATH"),
    stringsAsFactors = FALSE
  )
  
  # mutliple AEACNx
  AE <- data.frame(
    USUBJID = 1:5,
    AESTDTC = c("01JAN2017"),
    AETERM  = c("AE1","AE2","AE3","AE4","AE5"),
    AEDECOD = c("AE1","AE2","AE3","AE4","AE5"),
    AEACN   = rep("MULTIPLE", 5),
    AEACN1  = c("DOSE REDUCED", "DOSE NOT CHANGED", "DOSE NOT CHANGED",
                "DOSE NOT CHANGED", "NOT APPLICABLE"),
    stringsAsFactors = FALSE
  )
  
  expect_true(check_ds_ae_discon(DS, AE))
})

test_that("Returns false when errors present - 1", {
  AE <- data.frame(
    USUBJID = 1:5,
    AESTDTC = "01JAN2017",
    AETERM  = c("AE1","AE2","AE3","AE4","AE5"),
    AEDECOD = c("AE1","AE2","AE3","AE4","AE5"),
    AEACN = c("DOSE REDUCED", "DOSE REDUCED", "DOSE NOT CHANGED",
              "DOSE NOT CHANGED", "NOT APPLICABLE"),
    stringsAsFactors = FALSE
  )
  
  
  DS <- data.frame(
    USUBJID = 1:5,
    DSSPID  = c('XXXDISCTXXXXX'),
    DSSTDTC = '01JAN2017',
    DSCAT   = rep("DISPOSITION EVENT", 5),
    DSSCAT   = rep("TX FORM", 5),
    DSDECOD = c("PHYSICIAN DECISION", "OTHER", "PHYSICIAN DECISION", "OTHER", "DEATH"),
    stringsAsFactors = FALSE
  )
  
  # 1 case
  DS[3, "DSDECOD"] <- 'ADVERSE EVENT'
  expect_false(check_ds_ae_discon(DS, AE))
})

test_that("Returns true when no errors present - 2", {
  
  DS <- data.frame(
    USUBJID = 1:5,
    DSSPID  = c('XXXDISCTXXXXX'),
    DSSTDTC = '01JAN2017',
    DSCAT   = rep("DISPOSITION EVENT", 5),
    DSSCAT   = rep("TX FORM", 5),
    DSDECOD = c("PHYSICIAN DECISION", "OTHER", "PHYSICIAN DECISION", "OTHER", "DEATH"),
    stringsAsFactors = FALSE
  )
  
  # mutliple AEACNx
  AE <- data.frame(
    USUBJID = 1:5,
    AESTDTC = c("01JAN2017"),
    AETERM  = c("AE1","AE2","AE3","AE4","AE5"),
    AEDECOD = c("AE1","AE2","AE3","AE4","AE5"),
    AEACN   = rep("MULTIPLE", 5),
    AEACN1  = c("DOSE REDUCED", "DOSE NOT CHANGED", "DOSE NOT CHANGED",
                "DOSE NOT CHANGED", "NOT APPLICABLE"),
    stringsAsFactors = FALSE
  )
  
  DS[3, "DSDECOD"] <- 'ADVERSE EVENT'
  expect_false(check_ds_ae_discon(DS, AE))
})

test_that("Returns flase when expected column not present - 1", {
  
  AE <- data.frame(
    USUBJID = 1:5,
    AESTDTC = "01JAN2017",
    AETERM  = c("AE1","AE2","AE3","AE4","AE5"),
    AEDECOD = c("AE1","AE2","AE3","AE4","AE5"),
    AEACN = c("DOSE REDUCED", "DOSE REDUCED", "DOSE NOT CHANGED",
              "DOSE NOT CHANGED", "NOT APPLICABLE"),
    stringsAsFactors = FALSE
  )
  
  
  DS <- data.frame(
    USUBJID = 1:5,
    DSSPID  = c('XXXDISCTXXXXX'),
    DSSTDTC = '01JAN2017',
    DSCAT   = rep("DISPOSITION EVENT", 5),
    DSSCAT   = rep("TX FORM", 5),
    DSDECOD = c("PHYSICIAN DECISION", "OTHER", "PHYSICIAN DECISION", "OTHER", "DEATH"),
    stringsAsFactors = FALSE
  )
  
  AE$USUBJID <- NULL
  expect_false(check_ds_ae_discon(DS, AE))
  
}) 

test_that("Returns flase when expected column not present - 2", {
  
  AE <- data.frame(
    USUBJID = 1:5,
    AESTDTC = "01JAN2017",
    AETERM  = c("AE1","AE2","AE3","AE4","AE5"),
    AEDECOD = c("AE1","AE2","AE3","AE4","AE5"),
    AEACN = c("DOSE REDUCED", "DOSE REDUCED", "DOSE NOT CHANGED",
              "DOSE NOT CHANGED", "NOT APPLICABLE"),
    stringsAsFactors = FALSE
  )
  
  
  DS <- data.frame(
    USUBJID = 1:5,
    DSSPID  = c('XXXDISCTXXXXX'),
    DSSTDTC = '01JAN2017',
    DSCAT   = rep("DISPOSITION EVENT", 5),
    DSSCAT   = rep("TX FORM", 5),
    DSDECOD = c("PHYSICIAN DECISION", "OTHER", "PHYSICIAN DECISION", "OTHER", "DEATH"),
    stringsAsFactors = FALSE
  )
  
  DS$USUBJID <- NULL
  expect_false(check_ds_ae_discon(DS, AE))
  
}) 




