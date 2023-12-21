 
test_that("Returns true when no errors present", {
  
  TR <- data.frame(
    USUBJID = 1,
    TRDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
    VISIT = c(rep("C1D1",3), rep("C1D2",5), rep("C2D1",2)),
    TRTESTCD = c(rep("LDIAM",7),rep("SAXIS",3)),
    stringsAsFactors=FALSE)
  
  expect_true(check_tr_trdtc_across_visit(TR))
  
})

test_that("Returns false when errors present",{
  
  TR <- data.frame(
    USUBJID = 1,
    TRDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
    VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
    TRTESTCD = c(rep("LDIAM",7),rep("SAXIS",3)),
    stringsAsFactors=FALSE)
  
  expect_false(check_tr_trdtc_across_visit(TR))
  
})


test_that("Returns true when no errors present among INV records, only errors among IRF present",{
  
  TR <- data.frame(
    USUBJID = 1,
    TRDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
    VISIT = c(rep("C1D1",3), rep("C1D2",3), rep("C2D1",4)),
    TRTESTCD = c(rep("LDIAM",7),rep("SAXIS",3)),
    TREVAL = "INDEPENDENT ASSESSOR",
    stringsAsFactors=FALSE)
  
  expect_true(check_tr_trdtc_across_visit(TR))
  
})

test_that("Returns false when expected column not present", {
  
  
  TR <- data.frame(
    USUBJID = 1,
    TRDTC = c(rep("2016-01-01",3), rep("2016-06-01",5), rep("2016-06-24",2)),
    VISIT = c(rep("C1D1",3), rep("C1D2",5), rep("C2D1",2)),
    TRTESTCD = c(rep("LDIAM",7),rep("SAXIS",3)),
    stringsAsFactors=FALSE)
  
  TR$USUBJID <- NULL
  
  expect_false(check_tr_trdtc_across_visit(TR))
  
})
