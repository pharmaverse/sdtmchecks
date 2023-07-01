test_that("Function returns true when no errors are present", {
  
  CM <- data.frame(
    USUBJID = 1:5,
    DOMAIN = rep("CM", 5),
    CMTRT = rep("DRUG TERM", 5),
    CMDECOD = rep("CODED DRUG TERM", 5),
    CMSTDTC = 1:5,
    CMENDTC = 1:5,
    CMCAT = "CONCOMITANT MEDICATIONS",
    CMSPID = c("FORMNAME-R:13/L:13XXXX",
               "FORMNAME-R:16/L:16XXXX",
               "FORMNAME-R:2/L:2XXXX",
               "FORMNAME-R:19/L:19XXXX",
               "FORMNAME-R:5/L:5XXXX"),
    stringsAsFactors=FALSE
  )
  
  
  expect_true(check_cm_cmdecod(CM))
})




test_that("Function returns false when errors are present", {
  
  CM <- data.frame(
    USUBJID = 1:5,
    DOMAIN = rep("CM", 5),
    CMTRT = rep("DRUG TERM", 5),
    CMDECOD = rep("CODED DRUG TERM", 5),
    CMSTDTC = 1:5,
    CMENDTC = 1:5,
    CMCAT = "CONCOMITANT MEDICATIONS",
    CMSPID = c("FORMNAME-R:13/L:13XXXX",
               "FORMNAME-R:16/L:16XXXX",
               "FORMNAME-R:2/L:2XXXX",
               "FORMNAME-R:19/L:19XXXX",
               "FORMNAME-R:5/L:5XXXX"),
    stringsAsFactors=FALSE
  )
  CM$CMDECOD[1] = NA
  CM$CMDECOD[2] = "NA"
  CM$CMDECOD[3:5] = ""
  
  expect_false(check_cm_cmdecod(CM))
})



test_that("Function returns false when errors are present - with preprocessing function specified", {
  
  CM <- data.frame(
    USUBJID = 1:5,
    DOMAIN = rep("CM", 5),
    CMTRT = rep("DRUG TERM", 5),
    CMDECOD = rep("CODED DRUG TERM", 5),
    CMSTDTC = 1:5,
    CMENDTC = 1:5,
    CMCAT = "CONCOMITANT MEDICATIONS",
    CMSPID = c("FORMNAME-R:13/L:13XXXX",
               "FORMNAME-R:16/L:16XXXX",
               "FORMNAME-R:2/L:2XXXX",
               "FORMNAME-R:19/L:19XXXX",
               "FORMNAME-R:5/L:5XXXX"),
    stringsAsFactors=FALSE
  )
  CM$CMDECOD[1] = NA
  CM$CMDECOD[2] = "NA"
  CM$CMDECOD[3:5] = ""
  
  expect_false(check_cm_cmdecod(CM,preproc=roche_derive_rave_row))
})



test_that("Function returns false when expected column not present", {
  
  CM <- data.frame(
    USUBJID = 1:5,
    DOMAIN = rep("CM", 5),
    CMTRT = rep("DRUG TERM", 5),
    CMDECOD = rep("CODED DRUG TERM", 5),
    CMSTDTC = 1:5,
    CMENDTC = 1:5,
    CMCAT = "CONCOMITANT MEDICATIONS",
    CMSPID = c("FORMNAME-R:13/L:13XXXX",
               "FORMNAME-R:16/L:16XXXX",
               "FORMNAME-R:2/L:2XXXX",
               "FORMNAME-R:19/L:19XXXX",
               "FORMNAME-R:5/L:5XXXX"),
    stringsAsFactors=FALSE
  )
  
  CM$CMDECOD <- NULL
  
  expect_false(check_cm_cmdecod(CM))
})
