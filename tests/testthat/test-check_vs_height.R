test_that("Returns true when no errors present", {
  
  DM <- data.frame(
    STUDYID = 1,
    USUBJID = 1:10
  )
  
  VS <- data.frame(
    STUDYID = 1,
    USUBJID = 1:10,
    VSTEST = "HEIGHT",
    VSTESTCD = "HEIGHT",
    VSSTRESN = 1:10,
    VISIT = 1:10
  )
  
  expect_true(check_vs_height(VS,DM))
}) 

test_that("Returns false when errors present", {
  
  DM <- data.frame(
    STUDYID = 1,
    USUBJID = 1:11
  )
  
  VS <- data.frame(
    STUDYID = 1,
    USUBJID = 1:10,
    VSTEST = "HEIGHT",
    VSTESTCD = "HEIGHT",
    VSSTRESN = 1:10,
    VISIT = 1:10
  )
  
  expect_false(check_vs_height(VS,DM))
})

test_that("Returns false when errors present", {
  
  DM <- data.frame(
    STUDYID = 1,
    USUBJID = 1:10
  )
  
  VS <- data.frame(
    STUDYID = 1,
    USUBJID = 1:10,
    VSTEST = "HEIGHT",
    VSTESTCD = "HEIGHT",
    VSSTRESN = 1:10,
    VISIT = 1:10
  )
  
  VS$VSSTRESN[1] = NA
  VS$VSSTRESN[2] = "NA"
  VS$VSSTRESN[3] = ""
  VS$VSSTRESN[4] = "."
  
  expect_false(check_vs_height(VS,DM))
  
})

test_that("Returns false when expected column not present - 1", {
  
  DM <- data.frame(
    STUDYID = 1,
    USUBJID = 1:10
  )
  
  VS <- data.frame(
    STUDYID = 1,
    USUBJID = 1:10,
    VSTEST = "HEIGHT",
    VSTESTCD = "HEIGHT",
    VSSTRESN = 1:10,
    VISIT = 1:10
  )
  
  VS$VSSTRESN <- NULL
  
  expect_false(check_vs_height(VS,DM))
})

test_that("Returns false when expected column not present - 2", {
  
  DM <- data.frame(
    STUDYID = 1,
    USUBJID = 1:10
  )
  
  VS <- data.frame(
    STUDYID = 1,
    USUBJID = 1:10,
    VSTEST = "HEIGHT",
    VSTESTCD = "HEIGHT",
    VSSTRESN = 1:10,
    VISIT = 1:10
  )
  
  DM$USUBJID <- NULL
  
  expect_false(check_vs_height(VS,DM))
  
})
