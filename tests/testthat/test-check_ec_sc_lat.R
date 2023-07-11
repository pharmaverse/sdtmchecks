test_that("Function returns true when no errors are present", {
  
  
  sc <- data.frame(USUBJID  = c(1,1,1,2,2,2),
                   SCTEST   = c("Eye Meeting Eligibility Criteria",
                                "Focus of Study-Specific Interest",
                                " ",
                                "Eye Meeting Eligibility Criteria",
                                "Focus of Study-Specific Interest",
                                " "),
                   SCTESTCD = c("ELIGEYE", "FOCID", "", "ELIGEYE", "FOCID", ""),
                   SCCAT    = c("STUDY EYE SELECTION", "STUDY EYE SELECTION", "",
                                "STUDY EYE SELECTION", "STUDY EYE SELECTION", ""),
                   SCORRES  = c("LEFT", "OS", "", "RIGHT", "OD", ""),
                   SCDTC    = "2021-01-01",
                   stringsAsFactors = FALSE)
  
  ec <- data.frame(USUBJID = c(1,1,1,1,1,2,2,2,2,2,2),
                   ECCAT   = c("Fellow", "Study", "Study", "Study", "StudY",
                               "Fellow", "Fellow", "STUDY", "STUDY", "STUDY", ""),
                   ECMOOD  = rep("Performed", 11),
                   ECLOC   = rep("Eye", 11),
                   ECLAT   = c("LEFT", "Left", "left", "LEFT", "LEFT", "RIGHT",
                               "right", "right", "RIGHT", "RIGHT", "right"),
                   ECSTDY  = c(1, 28, 56, 84, 112, 1, 28, 56, 84, 112, 140),
                   VISIT   = c("Week 1", "Week 4", "Week 8", "Week 12", "Week 16",
                               "Week 1", "Week 4", "Week 8", "Week 12", "Week 16", "Week 20"),
                   ECSTDTC = c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01",
                               "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01",
                               "2021-06-01"),
                   ECOCCUR = "Y",
                   ECROUTE = "INTRAVITREAL",
                   stringsAsFactors=FALSE)
  
  expect_true(check_ec_sc_lat(SC=sc, EC=ec))
})

test_that("Function returns false when errors are present", {
  
  sc <- data.frame(USUBJID  = c(1,1,1,2,2,2),
                   SCTEST   = c("Eye Meeting Eligibility Criteria",
                                "Focus of Study-Specific Interest",
                                " ",
                                "Eye Meeting Eligibility Criteria",
                                "Focus of Study-Specific Interest",
                                " "),
                   SCTESTCD = c("ELIGEYE", "FOCID", "", "ELIGEYE", "FOCID", ""),
                   SCCAT    = c("STUDY EYE SELECTION", "STUDY EYE SELECTION", "",
                                "STUDY EYE SELECTION", "STUDY EYE SELECTION", ""),
                   SCORRES  = c("LEFT", "OS", "", "RIGHT", "OD", ""),
                   SCDTC    = "2021-01-01",
                   stringsAsFactors = FALSE)
  
  ec <- data.frame(USUBJID = c(1,1,1,1,1,2,2,2,2,2,2),
                   ECCAT   = c("Fellow", "Study", "Study", "Study", "StudY",
                               "Fellow", "Fellow", "STUDY", "STUDY", "STUDY", ""),
                   ECMOOD  = rep("Performed", 11),
                   ECLOC   = rep("Eye", 11),
                   ECLAT   = c("LEFT", "Left", "left", "LEFT", "RIGHT", "RIGHT",
                               "right", "right", "RIGHT", "RIGHT", "left"),
                   ECSTDY  = c(1, 28, 56, 84, 112, 1, 28, 56, 84, 112, 140),
                   VISIT   = c("Week 1", "Week 4", "Week 8", "Week 12", "Week 16",
                               "Week 1", "Week 4", "Week 8", "Week 12", "Week 16", "Week 20"),
                   ECSTDTC = c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01",
                               "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01",
                               "2021-06-01"),
                   ECOCCUR = "Y",
                   ECROUTE = "OPHTHALMIC",
                   stringsAsFactors=FALSE)
  
  expect_false(check_ec_sc_lat(SC=sc, EC=ec))
})


test_that("Function returns false when errors are present", {
  
  sc <- data.frame(USUBJID  = c(1,1,1,2,2,2,3),
                   SCTEST   = c("Eye Meeting Eligibility Criteria",
                                "Focus of Study-Specific Interest",
                                " ",
                                "Eye Meeting Eligibility Criteria",
                                "Focus of Study-Specific Interest",
                                " ",
                                "Focus of Study-Specific Interest"),
                   SCTESTCD = c("ELIGEYE", "FOCID", "", "ELIGEYE", "FOCID", "", "FOCID"),
                   SCCAT    = c("STUDY EYE SELECTION", "STUDY EYE SELECTION", "",
                                "STUDY EYE SELECTION",
                                "STUDY EYE SELECTION", "", "STUDY EYE SELECTION"),
                   SCORRES  = c("LEFT", "OS", "", "RIGHT", "OD", "", "RIGHT"),
                   SCDTC    = "2021-01-01",
                   stringsAsFactors = FALSE)
  
  ec <- data.frame(USUBJID = c(1,1,1,1,1,2,2,2,2,2,2),
                   ECMOOD  = "Performed",
                   ECLOC   = "Eye",
                   ECLAT   = c("LEFT", "Left", "left", "LEFT", "RIGHT", "RIGHT",
                               "right", "right", "RIGHT", "RIGHT", "left"),
                   ECSTDY  = c(1, 28, 56, 84, 112, 1, 28, 56, 84, 112, 140),
                   VISIT   = c("Week 1", "Week 4", "Week 8", "Week 12", "Week 16",
                               "Week 1", "Week 4", "Week 8", "Week 12", "Week 16", "Week 20"),
                   ECSTDTC = c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01",
                               "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01",
                               "2021-06-01"),
                   ECOCCUR = c("Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "N"),
                   stringsAsFactors=FALSE)
  
  
  expect_false(check_ec_sc_lat(SC=sc, EC=ec))
})
