test_that("Function returns true when no errors are present - 1", {
  
  PR <- data.frame(
    USUBJID = 1:5,
    PRCAT = "PRIOR OCULAR SURGERIES AND PROCEDURES",
    PRSTDTC = 1:5,
    PRLAT   = c("Left", "","Bilateral", "", ""),
    PRTRT   = c("A", "B", "A", "B", "A"),
    PROCCUR = c("Y", "N", "N", "Y", "Y"),
    PRPRESP = "Y",
    PRSPID  = "FORMNAME-R:2/L:2XXXX",
    stringsAsFactors = FALSE)
  
  PR=PR[1:3,]
   
  expect_true(check_pr_prlat(PR))
})



test_that("Function returns true when no errors are present - 2", {
  
  PR <- data.frame(
    USUBJID = 1:5,
    PRCAT = "CONCURRENT OCULAR PROCEDURE",
    PRSTDTC = 1:5,
    PRLAT   = c("Left", "LEFT","Bilateral", "RIGHT", "RIgHT"),
    PRTRT  = c("A", "B", "A", "B", "A"),
    PROCCUR = NA,
    PRPRESP = NA,
    stringsAsFactors = FALSE)
  
  
  expect_true(check_pr_prlat(PR))
})



test_that("Function returns false when errors are present - 1", {
  
  PR <- data.frame(
    USUBJID = 1:5,
    PRCAT = "PRIOR OCULAR SURGERIES AND PROCEDURES",
    PRSTDTC = 1:5,
    PRLAT   = c("Left", "","Bilateral", "", ""),
    PRTRT   = c("A", "B", "A", "B", "A"),
    PROCCUR = c("Y", "N", "N", "Y", "Y"),
    PRPRESP = "Y",
    PRSPID  = "FORMNAME-R:2/L:2XXXX",
    stringsAsFactors = FALSE)

  
  expect_false(check_pr_prlat(PR))
})



test_that("Function returns false when errors are present - 2", {
  
  PR <- data.frame(
     USUBJID = 1:5,
     PRCAT = "CONCURRENT OCULAR PROCEDURE",
     PRSTDTC = 1:5,
     PRLAT   = c("Left", "LEFT","Bilateral", "RIGHT", ""),
     PRTRT  = c("A", "B", "A", "B", "A"),
     PROCCUR = NA,
     PRPRESP = NA,
     stringsAsFactors = FALSE)
  
  
  expect_false(check_pr_prlat(PR))
})


test_that("Function returns false when errors are present - 3", {
  
  PR <- data.frame(
    USUBJID = 1:5,
    PRCAT = "CONCURRENT OCULAR PROCEDURE",
    PRSTDTC = 1:5,
    PRLAT   = c("Left", "","Bilateral", "RIGHT", ""),
    PRTRT  = c("A", "B", "A", "B", "A"),
    PROCCUR = c("Y", "N", "N", "Y", "Y"),
    PRPRESP = "Y",
    stringsAsFactors = FALSE)
  
  
  expect_false(check_pr_prlat(PR))
})



test_that("Function returns false when errors are present - 4", {
  
  PR <- data.frame(
    USUBJID = 1:5,
    PRCAT = c(rep("CONCURRENT NON-OCULAR PROCEDURE",3),rep("CONCURRENT OCULAR PROCEDURE",2)),
    PRSTDTC = 1:5,
    PRLAT   = c("", "","", "RIGHT", ""),
    PRTRT  = c("A", "B", "A", "B", "A"),
    PROCCUR = c("Y", "N", "N", "Y", "Y"),
    PRPRESP = "Y",
    stringsAsFactors = FALSE)
  
  
  expect_false(check_pr_prlat(PR))
})
















test_that("Function returns false when expected column not present", {
  
  PR <- data.frame(
    USUBJID = 1:5,
    PRCAT = "PRIOR OCULAR SURGERIES AND PROCEDURES",
    PRSTDTC = 1:5,
    PRLAT   = c("Left", "","Bilateral", "", ""),
    PRTRT   = c("A", "B", "A", "B", "A"),
    PROCCUR = c("Y", "N", "N", "Y", "Y"),
    PRPRESP = "Y",
    PRSPID  = "FORMNAME-R:2/L:2XXXX",
    stringsAsFactors = FALSE)
  
  PR$PRLAT = NULL
  
  expect_false(check_pr_prlat(PR))
})

