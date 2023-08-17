
test_that("Returns true when no errors present", {
  DS <- data.frame(
    USUBJID = c(rep(1,3),rep(2,3),rep(3,3)),
    DSSCAT= rep(c("STUDY DISCONTINUATION", "ADVERSE EVENT", "PROTOCOL"),3),
    stringsAsFactors=FALSE
  )
  expect_true(check_ds_dsscat(DS))
})

test_that("Returns false when errors present - 1", {
  DS <- data.frame(
    USUBJID = c(rep(1,3),rep(2,3),rep(3,3)),
    DSSCAT= rep(c("STUDY DISCONTINUATION", "ADVERSE EVENT", "PROTOCOL"),3),
    stringsAsFactors=FALSE
  )
  
  DS$DSSCAT[8] = "STUDY DISCONTINUATION"
  expect_false(check_ds_dsscat(DS))
})

test_that("Returns false when errors present - 2", {
  DS <- data.frame(
    USUBJID = numeric(),
    DSSCAT= character(),
    stringsAsFactors=FALSE
  )
  expect_false(check_ds_dsscat(DS))
})

test_that("Returns false when expected column not present", {
  DS <- data.frame(
    USUBJID = c(rep(1,3),rep(2,3),rep(3,3)),
    DSSCAT= rep(c("STUDY DISCONTINUATION", "ADVERSE EVENT", "PROTOCOL"),3),
    stringsAsFactors=FALSE
  )
 
  DS$DSSCAT = NULL
  expect_false(check_ds_dsscat(DS))
  
})

