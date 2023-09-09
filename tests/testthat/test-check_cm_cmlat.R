test_that("Function returns true when no errors are present", {
  
   CM <- data.frame(
      USUBJID = 1:7,
      CMCAT = "CONCOMITANT MEDICATIONS",
      CMSTDTC = 1:7,
      CMLAT   = c("Left", "","Bilateral", "", "", "LEFT", ""),
      CMTRT  = c("A", "B", "A", "B", "A", "A", "B"),
      CMDECOD = c("A", "B", "A", "B", "A", "A", "B"),
      CMROUTE = c("","OPHTHALMIC","INTRAVITREAL","INTRAVITREAL", 
                  "opHTHALMIC", "INTRaOCULAr", "INTRaOCULAr"),
      CMSPID  = "FORMNAME-R:13/L:13XXXX",
      stringsAsFactors = FALSE)
   
   CM=CM[CM$USUBJID %in% c(3,6),]
   
  expect_true(check_cm_cmlat(CM))
})

test_that("Function returns false when errors are present - 1", {
  
  CM <- data.frame(
    USUBJID = 1:7,
    CMCAT = "CONCOMITANT MEDICATIONS",
    CMSTDTC = 1:7,
    CMLAT   = c("Left", "","Bilateral", "", "", "LEFT", ""),
    CMTRT  = c("A", "B", "A", "B", "A", "A", "B"),
    CMDECOD = c("A", "B", "A", "B", "A", "A", "B"),
    CMROUTE = c("","OPHTHALMIC","INTRAVITREAL","INTRAVITREAL", 
                "opHTHALMIC", "INTRaOCULAr", "INTRaOCULAr"),
    CMSPID  = "FORMNAME-R:13/L:13XXXX",
    stringsAsFactors = FALSE)

  
  expect_false(check_cm_cmlat(CM))
})



test_that("Function returns false when errors are present - 2", {
  
  CM <- data.frame(
    USUBJID = 1:7,
    CMCAT = "CONCOMITANT MEDICATIONS",
    CMSTDTC = 1:7,
    CMLAT   = c("Left", "","Bilateral", "", "", "LEFT", ""),
    CMTRT  = c("A", "B", "A", "B", "A", "A", "B"),
    CMDECOD = c("A", "B", "A", "B", "A", "A", "B"),
    CMROUTE = c("","OPHTHALMIC","INTRAVITREAL","INTRAVITREAL", 
                "opHTHALMIC", "INTRaOCULAr", "INTRaOCULAr"),
    CMSPID  = "FORMNAME-R:13/L:13XXXX",
    stringsAsFactors = FALSE)
  
  
  expect_false(check_cm_cmlat(CM))
})


test_that("Function returns false when expected column not present - 1", {
  
  CM <- data.frame(
     USUBJID = 1:5,
     CMCAT = "CONCOMITANT MEDICATIONS",
     CMSTDTC = 1:5,
     CMLAT   = c("Left", "LEFT","Bilateral", "RIGHT", "RIgHT"),
     CMTRT  = c("A", "B", "A", "B", "A"),
     CMDECOD = c("A", "B", "A", "B", "A"),
     stringsAsFactors = FALSE)
  
  
  expect_false(check_cm_cmlat(CM))
})



test_that("Function returns false when expected column not present - 2", {
  
  CM <- data.frame(
    USUBJID = 1:7,
    CMCAT = "CONCOMITANT MEDICATIONS",
    CMSTDTC = 1:7,
    CMLAT   = c("Left", "","Bilateral", "", "", "LEFT", ""),
    CMTRT  = c("A", "B", "A", "B", "A", "A", "B"),
    CMDECOD = c("A", "B", "A", "B", "A", "A", "B"),
    CMROUTE = c("","OPHTHALMIC","INTRAVITREAL","INTRAVITREAL", 
                "opHTHALMIC", "INTRaOCULAr", "INTRaOCULAr"),
    CMSPID  = "FORMNAME-R:13/L:13XXXX",
    stringsAsFactors = FALSE)
  
  CM$CMLAT = NULL
  
  expect_false(check_cm_cmlat(CM))
})

