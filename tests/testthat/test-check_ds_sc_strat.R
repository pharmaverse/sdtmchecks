test_that("function returns true when no errors are present", {
  
  ds <- data.frame(USUBJID = c(1,2,2),
                   DSDECOD = c("RANDOMIZATION", "RANDOMIZED", "Randomized"),
                   DSSTDTC = c("2021-01-01", "2021-01-02", "2021-02-01"))
  sc <- data.frame(USUBJID  = c(1,1,1,2,2,2),
                   SCCAT = rep("STRATIFICATION", 6),
                   SCTESTCD = c("STRAT 1", "STRAT 2", "STRAT 3", "STRAT 1", "STRAT 2", "STRAT 3"),
                   SCTEST   = c("Factor 1", "Factor 2", "Factor 3",
                                "Factor 1", "Factor 2", "Factor 3"),
                   SCORRES  = c("US", "Left", "Score > x", "RoW", "Right", "Score < x"),
                   stringsAsFactors = FALSE)
  
  expect_true(check_ds_sc_strat(ds, sc))
  
})



test_that("function returns false when errors are present", {
  
  ds <- data.frame(USUBJID = c(1,2,2),
                   DSDECOD = c("RANDOMIZATION", "RANDOMIZED", "Randomized"),
                   DSSTDTC = c("2021-01-01", "2021-01-02", "2021-02-01"))
  sc <- data.frame(USUBJID  = c(1,1,1),
                   SCCAT = rep("STRATIFICATION", 3),
                   SCTESTCD = c("STRAT 1", "STRAT 2", "STRAT 3"),
                   SCTEST   = c("Factor 1", "Factor 2", "Factor 3"),
                   SCORRES  = c("US", "Left", NA),
                   stringsAsFactors = FALSE)
  
  expect_false(check_ds_sc_strat(ds, sc))
  
})



test_that("function returns false when errors are present", {
  
  
  ds <- data.frame(USUBJID = c(1,2),
                   DSDECOD = c("Open Label", "Open Label"),
                   DSSTDTC = c("2021-01-01", "2021-01-02"))
  sc <- data.frame(USUBJID  = c(1,1,1,2,2,2),
                   SCCAT = rep("No STRATIFICATION", 6),
                   SCTESTCD = c("STRAT 1", "STRAT 2", "STRAT 3", "STRAT 1", "STRAT 2", "STRAT 3"),
                   SCTEST   = c("Factor 1", "Factor 2", "Factor 3",
                                "Factor 1", "Factor 2", "Factor 3"),
                   SCORRES  = c("US", "Left", NA, "RoW", "Right", "Score < x"),
                   stringsAsFactors = FALSE)
  
  expect_false(check_ds_sc_strat(ds, sc))
  
})