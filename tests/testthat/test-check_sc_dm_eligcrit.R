test_that("Function returns true when no errors are present", {
  
  dm <- data.frame(USUBJID = c(1,2))
  sc <- data.frame(USUBJID  = c(1,1,1,2,2,2),
                   SCTEST   = c("Eye Meeting Eligibility Criteria",
                                "Focus of Study-Specific Interest",
                                " ",
                                "Eye Meeting Eligibility Criteria",
                                "Focus of Study-Specific Interest", " "),
                   SCTESTCD = c("ELIGEYE", "FOCID", "", "ELIGEYE", "FOCID", ""),
                   SCCAT    = c("STUDY EYE SELECTION",
                                "STUDY EYE SELECTION",
                                "",
                                "STUDY EYE SELECTION",
                                "STUDY EYE SELECTION",
                                ""),
                   SCORRES  = c("OS", "OS", "", "", "OU", ""),
                   SCDTC    = rep("2021-01-01", 6),
                   stringsAsFactors = FALSE)
  
  sc$SCORRES[4] = "OS"
  expect_true(check_sc_dm_eligcrit(SC=sc, DM=dm))
})


test_that("Function returns false when errors present", {
  
  dm <- data.frame(USUBJID = c(1,2))
  sc <- data.frame(USUBJID  = c(1,1,1,2,2,2),
                   SCTEST   = c("Eye Meeting Eligibility Criteria",
                                "Focus of Study-Specific Interest",
                                " ",
                                "Eye Meeting Eligibility Criteria",
                                "Focus of Study-Specific Interest", " "),
                   SCTESTCD = c("ELIGEYE", "FOCID", "", "ELIGEYE", "FOCID", ""),
                   SCCAT    = c("STUDY EYE SELECTION",
                                "STUDY EYE SELECTION",
                                "",
                                "STUDY EYE SELECTION",
                                "STUDY EYE SELECTION",
                                ""),
                   SCORRES  = c("OS", "OS", "", "", "OU", ""),
                   SCDTC    = rep("2021-01-01", 6),
                   stringsAsFactors = FALSE)
  
  #missing values
  dm <- data.frame(USUBJID = c(1,2,3,4))
  sc$SCORRES[4] = "OS"
  
  
  expect_false(check_sc_dm_eligcrit(SC=sc, DM=dm))
  
})


test_that("Function returns false when expected column not present",{
  
  dm <- data.frame(USUBJID = c(1,2))
  sc <- data.frame(USUBJID  = c(1,1,1,2,2,2),
                   SCTEST   = c("Eye Meeting Eligibility Criteria",
                                "Focus of Study-Specific Interest",
                                " ",
                                "Eye Meeting Eligibility Criteria",
                                "Focus of Study-Specific Interest", " "),
                   SCTESTCD = c("ELIGEYE", "FOCID", "", "ELIGEYE", "FOCID", ""),
                   SCCAT    = c("STUDY EYE SELECTION",
                                "STUDY EYE SELECTION",
                                "",
                                "STUDY EYE SELECTION",
                                "STUDY EYE SELECTION",
                                ""),
                   SCORRES  = c("OS", "OS", "", "", "OU", ""),
                   SCDTC    = rep("2021-01-01", 6),
                   stringsAsFactors = FALSE)
  
  
  sc$SCORRES <- NULL
  expect_false(check_sc_dm_eligcrit(SC=sc, DM=dm))
  
})

                    