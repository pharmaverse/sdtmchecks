test_that("Returns true when no errors present", {
  DM <- data.frame(
    USUBJID = 1:10,
    AGE = c(50,60,50,89,19,33,50,40,22,25)
  )
  
  expect_true(check_dm_age_missing(DM))
})

test_that("Returns false when errors present - 1", {
  DM <- data.frame(
    USUBJID = 1:10,
    AGE =c(50,60,50,89,NA,33,50,40,22,25)
  )
  
  expect_false(check_dm_age_missing(DM))
})

test_that("Returns false when errors present - 2", {
  DM <- data.frame(
    USUBJID = 1:10,
    AGE =c(50,60,50,89,91,33,50,40,22,25)
  )
  
  expect_false(check_dm_age_missing(DM))
})

test_that("Returns false when errors present - 3", {
  DM <- data.frame(
    USUBJID = 1:10,
    AGE =c(50,60,50,89,17,33,50,40,22,25)
  )
  
  expect_false(check_dm_age_missing(DM))
})


test_that("Returns false when expected column not present", {
  DM <- data.frame(
    USUBJID = 1:10,
    AGE = c(50,60,50,89,19,33,50,40,22,25)
  )
  
  DM$AGE = NULL
  
  expect_false(check_dm_age_missing(DM))
})
