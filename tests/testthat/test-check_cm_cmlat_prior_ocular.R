test_that("Function returns true when no errors are present - 1", {
  
  CM <- data.frame(
    USUBJID = 1:5,
    CMCAT = "PRIOR OCULAR THERAPIES AND TREATMENTS",
    CMSTDTC = 1:5,
    CMLAT   = c("Left", "","Bilateral", "", ""),
    CMTRT   = c("A", "B", "A", "B", "A"),
    CMDECOD = c("A", "B", "A", "B", "A"),
    CMROUTE = c("","OPHTHALMIC","INTRAVITREAL","INTRAVITREAL", "opHTHALMIC"),
    CMSPID  = "FORMNAME-R:13/L:13XXXX",
    stringsAsFactors = FALSE)
   
   CM=CM[CM$USUBJID %in% c(1,3),]
   
  expect_true(check_cm_cmlat_prior_ocular(CM))
})


test_that("Function returns true when no errors are present - 2", {
  
  CM <- data.frame(
    USUBJID = 1:5,
    CMCAT = "CONCOMITANT MEDICATIONS",
    CMSTDTC = 1:5,
    CMLAT   = c("Left", "LEFT","Bilateral", "RIGHT", "RIgHT"),
    CMTRT  = c("A", "B", "A", "B", "A"),
    CMDECOD = c("A", "B", "A", "B", "A"),
    CMROUTE = c("","OPHTHALMIC","INTRAVITREAL","INTRAVITREAL", "opHTHALMIC"),
    stringsAsFactors = FALSE)
  
  expect_true(check_cm_cmlat_prior_ocular(CM))
})


test_that("Function returns true when no errors are present - 3", {
  
  
  CM <- data.frame(
    USUBJID = 1:5,
    CMCAT = "CONCOMITANT MEDICATIONS",
    CMSTDTC = 1:5,
    CMLAT   = c("Left", "LEFT","Bilateral", "RIGHT", "RIgHT"),
    CMTRT  = c("A", "B", "A", "B", "A"),
    CMDECOD = c("A", "B", "A", "B", "A"),
    #CMROUTE = c("","OPHTHALMIC","INTRAVITREAL","INTRAVITREAL", "opHTHALMIC"),
    stringsAsFactors = FALSE)

  
  expect_true(check_cm_cmlat_prior_ocular(CM))
})





test_that("Function returns false when errors are present - 1", {
  
  CM <- data.frame(
    USUBJID = 1:5,
    CMCAT = "PRIOR OCULAR THERAPIES AND TREATMENTS",
    CMSTDTC = 1:5,
    CMLAT   = c("Left", "","Bilateral", "", ""),
    CMTRT   = c("A", "B", "A", "B", "A"),
    CMDECOD = c("A", "B", "A", "B", "A"),
    CMROUTE = c("","OPHTHALMIC","INTRAVITREAL","INTRAVITREAL", "opHTHALMIC"),
    CMSPID  = "FORMNAME-R:13/L:13XXXX",
    stringsAsFactors = FALSE)
  
  expect_false(check_cm_cmlat_prior_ocular(CM))
})



test_that("Function returns false when errors are present - 2", {
  
  CM <- data.frame(
     USUBJID = 1:5,
     CMCAT = c(rep("Prior Ocular Therapies/Treatments",3), rep("Non-Ocular Therapies/Treatments",2)),
     CMSTDTC = 1:5,
     CMLAT   = c("", "LEFT","Bilateral", "", ""),
     CMTRT  = c("A", "B", "A", "B", "A"),
     CMDECOD = c("A", "B", "A", "B", "A"),
     #CMROUTE = c("","OPHTHALMIC","INTRAVITREAL","ORAL", "ORAL"),
     stringsAsFactors = FALSE)
  

  
  expect_false(check_cm_cmlat_prior_ocular(CM))
})


test_that("Function returns false when expected column not present", {
  
  CM <- data.frame(
     USUBJID = 1:5,
     CMCAT = "CONCOMITANT MEDICATIONS",
     CMSTDTC = 1:5,
     CMLAT   = c("Left", "LEFT","Bilateral", "RIGHT", "RIgHT"),
     CMTRT  = c("A", "B", "A", "B", "A"),
     CMDECOD = c("A", "B", "A", "B", "A"),
     stringsAsFactors = FALSE)
  
  CM$CMLAT=NULL
  
  expect_false(check_cm_cmlat_prior_ocular(CM))
})
