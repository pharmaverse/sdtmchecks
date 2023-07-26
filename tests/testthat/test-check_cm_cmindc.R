test_that("Function returns true when no errors are present", {
  
  CM <- data.frame(
  USUBJID = c(rep(1,3),rep(2,3),rep(3,3)),
  CMTRT = letters[1:9],
  CMSTDTC = rep("2017-01-01",9),
  CMINDC = c(rep("INDICATION 1",2), rep("indication 2",2),
             rep("Prophylaxis",2),rep("PROPHYLACTIC",2),"PROPHYLAXIS FOR XYZ"),
  CMPROPH = c(rep("Y",3),rep(NA,2),rep("",2),"NA","."),
  CMSPID = "/F:XXX-D:12345-R:123",
  stringsAsFactors=FALSE
  )
   
  CM$CMPROPH[7] = "Y"
  CM=CM[c(1:4,7),]
   
  expect_true(check_cm_cmindc(CM))
})

test_that("Function returns false when errors are present", {
  
  CM <- data.frame(
    USUBJID = c(rep(1,3),rep(2,3),rep(3,3)),
    CMTRT = letters[1:9],
    CMSTDTC = rep("2017-01-01",9),
    CMINDC = c(rep("INDICATION 1",2), rep("indication 2",2),
               rep("Prophylaxis",2),rep("PROPHYLACTIC",2),"PROPHYLAXIS FOR XYZ"),
    CMPROPH = c(rep("Y",3),rep(NA,2),rep("",2),"NA","."),
    CMSPID = "/F:XXX-D:12345-R:123",
    stringsAsFactors=FALSE
  )

  
  expect_false(check_cm_cmindc(CM))
})



test_that("Function returns false when expected column not present", {
  
  CM <- data.frame(
    USUBJID = c(rep(1,3),rep(2,3),rep(3,3)),
    CMTRT = letters[1:9],
    CMSTDTC = rep("2017-01-01",9),
    CMINDC = c(rep("INDICATION 1",2), rep("indication 2",2),
               rep("Prophylaxis",2),rep("PROPHYLACTIC",2),"PROPHYLAXIS FOR XYZ"),
    CMPROPH = c(rep("Y",3),rep(NA,2),rep("",2),"NA","."),
    CMSPID = "/F:XXX-D:12345-R:123",
    stringsAsFactors=FALSE
  )
  
  CM$CMPROPH = NULL
  
  expect_false(check_cm_cmindc(CM))
})

