test_that("creates R script - 1", {
   # All checks are output to a file
   fileName <- file.path(tempdir(), "run_all_checks.R")
   invisible(capture.output(a <- create_R_script(file = fileName)))
   expect_true(file.exists(fileName))
})

test_that("creates R script - 2", {
   fileName <- file.path(tempdir(), "run_some_checks.R")
   mymetads = sdtmchecksmeta[sdtmchecksmeta$category == "ALL" & sdtmchecksmeta$priority == "High",]
   create_R_script(metads = mymetads, file = fileName, verbose = FALSE)
   expect_true(file.exists(fileName))
})

test_that("Creates R script - 3", {
   fileName <- file.path(tempdir(), "run_all_checks_roche.R")
   mymetads = sdtmchecksmeta
   mymetads$fxn_in=mymetads$fxn_in_roche
   create_R_script(metads = mymetads, file = fileName, verbose = FALSE)
   expect_true(file.exists(fileName))
})
