test_that("Function returns dataframe with the new RAVE column", {
  
  # created version of data with RAVE variable
  TR <- data.frame(USUBJID = 1:5,
                   TRTESTCD = c("OTHER", rep("LDIAM", 4)),
                   TRLINKID = 1:5,
                   TRDTC = 1:5,
                   VISIT = LETTERS[1:5],
                   TRORRES = LETTERS[1:5],
                   TRSTRESN = 1:5,
                   TRSTAT = "",
                   TREVAL = "INVESTIGATOR",
                   TRSPID = "FORMNAME-R:19/L:19XXXX",
                   stringsAsFactors = FALSE)
  
  TR_exp <- TR
  TR_exp$RAVE <-"FORMNAME-R:19" 
    
  # compare with version that uses the roche_derive_rave_row utility function
  TR_act <- roche_derive_rave_row(TR) 
  
  expect_s3_class(TR_exp, "data.frame")
  
  expect_s3_class(TR_act, "data.frame")
  
  expect_equal(TR_act, TR_exp, ignore_attr=TRUE)
  
})
 