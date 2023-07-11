
test_that("Returns true when no errors present", {
  
  TR <- data.frame(
    USUBJID  = c(1,1,2,2),
    TRCAT    = c(1,1,2,2),
    TRTESTCD = c(1,1,2,2),
    TRLINKID = c(1,1,2,2),
    TRSPID   = c(1,1,2,2),
    TRDTC    = c(1,1,2,2),
    TRSTRESC = c(1,1,2,2)
  )
  
  TR <- TR[c(1,3),]
  
  expect_true(check_tr_dup(TR))
  
})

test_that("Returns false when errors present", {
  
  
  TR <- data.frame(
    USUBJID  = c(1,1,2,2),
    TRCAT    = c(1,1,2,2),
    TRTESTCD = c(1,1,2,2),
    TRLINKID = c(1,1,2,2),
    TRSPID   = c(1,1,2,2),
    TRDTC    = c(1,1,2,2),
    TRSTRESC = c(1,1,2,2)
  )
  
  expect_false(check_tr_dup(TR))
  
})

test_that("Returns false when expected column not present - 1", {
  
  TR <- data.frame(
    USUBJID  = c(1,2,2,1),
    TRCAT    = c(1,1,2,2),
    TRTESTCD = c(1,2,2,1),
    TRLINKID = c(1,1,2,2),
    TRSPID   = c(1,2,2,1),
    TRDTC    = c(1,1,2,2),
    TRSTRESC = c(1,2,2,1)
  )
  
  TR$USUBJID <- NULL
  
  expect_false(check_tr_dup(TR))

})


test_that("Returns false when expected column not present - 2", {
  
  TR <- data.frame(
    USUBJID  = c(1,2,2,1),
    TRCAT    = c(1,1,2,2),
    TRTESTCD = c(1,2,2,1),
    TRLINKID = c(1,1,2,2),
    TRSPID   = c(1,2,2,1),
    TRDTC    = c(1,1,2,2),
    TRSTRESC = c(1,2,2,1)
  )
  
  TR$TRLINKID <- NULL
  
  expect_false(check_tr_dup(TR))
  
})

