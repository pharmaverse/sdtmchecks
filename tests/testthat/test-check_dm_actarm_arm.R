test_that("Returns true when no errors present",{
  DM <- data.frame(USUBJID = 1:5,
                   ARM = c(letters[1:5]),
                   ACTARM = letters[1:5],
                   stringsAsFactors = FALSE)
  
  
  expect_true(check_dm_actarm_arm(DM))
  
}) 

test_that("Returns false when errors present - 1",{
  DM <- data.frame(USUBJID = 1:5,
                   ARM = c(letters[1:3], letters[5:6]),
                   ACTARM = letters[1:5],
                   stringsAsFactors = FALSE)
  
  expect_false(check_dm_actarm_arm(DM))
})

test_that("Returns false when errors present - 2", {
  DM <- data.frame(USUBJID = 1:5,
                   ACTARM = c(letters[1:3], letters[5:6]),
                   ARM = letters[1:5],
                   stringsAsFactors = FALSE)
  
  expect_false(check_dm_actarm_arm(DM))
})

test_that("Returns false when expected column not present", {
  DM <- data.frame(USUBJID = 1:5,
                   ARM = c(letters[1:5]),
                   ACTARM = letters[1:5],
                   stringsAsFactors = FALSE)
  
  DM$ARM <- NULL
  
  expect_false(check_dm_actarm_arm(DM))
})
