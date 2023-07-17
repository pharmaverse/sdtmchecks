test_that("Returns true when no errors present", {
  
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
                   SCDTC    = rep("2021-01-01", 6),
                   stringsAsFactors = FALSE)
  
  oe <- data.frame(USUBJID = c(1,1,1,1,1,2,2,2,2,2,2),
                   OECAT  = rep("SAFETY ASSESSMENT OF LOW VISION", 11),
                   OELOC   = rep("Eye", 11),
                   OELAT   = c("LEFT", "Left", "left", "LEFT", "LEFT", 
                               "RIGHT", "right", "right", "RIGHT", "RIGHT", "right"),
                   OEDY  = c(1, 28, 56, 84, 112, 1, 28, 56, 84, 112, 140),
                   VISIT   = c("Week 1", "Week 4", "Week 8", "Week 12", "Week 16", 
                               "Week 1", "Week 4", "Week 8", "Week 12", "Week 16", "Week 20"),
                   OEDTC = c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", 
                             "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", 
                             "2021-06-01"),
                   OETEST = c("A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "F"),
                   stringsAsFactors=FALSE)
  
  expect_true(check_oe_sc_lat_count_fingers(SC=sc, OE=oe))
  
})

test_that("Returns false when errors present - 1", {
  
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
                   SCDTC    = rep("2021-01-01", 6),
                   stringsAsFactors = FALSE)
  
  oe <- data.frame(USUBJID = c(1,1,1,1,1,2,2,2,2,2,2),
                   OECAT  = rep("SAFETY ASSESSMENT OF LOW VISION", 11),
                   OELOC   = rep("Eye", 11),
                   OELAT   = c("LEFT", "Left", "left", "LEFT", "right", "RIGHT", 
                               "right", "right", "RIGHT", "RIGHT", "left"),
                   OEDY  = c(1, 28, 56, 84, 112, 1, 28, 56, 84, 112, 140),
                   VISIT   = c("Week 1", "Week 4", "Week 8", "Week 12", "Week 16", 
                               "Week 1", "Week 4", "Week 8", "Week 12", "Week 16", 
                               "Week 20"),
                   OEDTC = c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", 
                             "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", 
                             "2021-06-01"),
                   OETEST = c("A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "F"),
                   stringsAsFactors=FALSE)
  
  expect_false(check_oe_sc_lat_count_fingers(SC=sc, OE=oe))
  
})

test_that("Returns false when errors present - 2", {
  
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
                                "STUDY EYE SELECTION", "STUDY EYE SELECTION", 
                                "", "STUDY EYE SELECTION"),
                   SCORRES  = c("LEFT", "OS", "", "RIGHT", "OD", "", "OS"),
                   SCDTC    = "2021-01-01",
                   stringsAsFactors = FALSE)
  
  oe <- data.frame(USUBJID = c(1,1,1,1,1,2,2,2,2,2,2),
                   OESTAT  = c("","","","","","","","","","", "not DONE"),
                   OECAT  = "SAFETY ASSESSMENT OF LOW VISION",
                   OELOC   = "Eye",
                   OELAT   = c("LEFT", "Left", "left", "LEFT", "right", "RIGHT", 
                               "right", "right", "RIGHT", "RIGHT", "left"),
                   OEDY  = c(1, 28, 56, 84, 112, 1, 28, 56, 84, 112, 140),
                   VISIT   = c("Week 1", "Week 4", "Week 8", "Week 12", "Week 16", 
                               "Week 1", "Week 4", "Week 8", "Week 12", "Week 16", 
                               "Week 20"),
                   OEDTC = c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", 
                             "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", 
                             "2021-06-01"),
                   OETEST = c("A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "F"),
                   stringsAsFactors=FALSE)
  
  expect_false(check_oe_sc_lat_count_fingers(SC=sc, OE=oe))
  
})

test_that("Returns false when expected column not present - 1", {
  
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
                   SCDTC    = rep("2021-01-01", 6),
                   stringsAsFactors = FALSE)
  
  oe <- data.frame(USUBJID = c(1,1,1,1,1,2,2,2,2,2,2),
                   OECAT  = rep("SAFETY ASSESSMENT OF LOW VISION", 11),
                   OELOC   = rep("Eye", 11),
                   OELAT   = c("LEFT", "Left", "left", "LEFT", "LEFT", 
                               "RIGHT", "right", "right", "RIGHT", "RIGHT", "right"),
                   OEDY  = c(1, 28, 56, 84, 112, 1, 28, 56, 84, 112, 140),
                   VISIT   = c("Week 1", "Week 4", "Week 8", "Week 12", "Week 16", 
                               "Week 1", "Week 4", "Week 8", "Week 12", "Week 16", "Week 20"),
                   OEDTC = c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", 
                             "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", 
                             "2021-06-01"),
                   OETEST = c("A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "F"),
                   stringsAsFactors=FALSE)
  
  sc$SCTEST <- NULL
  
  expect_false(check_oe_sc_lat_count_fingers(SC=sc, OE=oe))
  
})

test_that("Returns false when expected column not present - 2", {
  
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
                   SCDTC    = rep("2021-01-01", 6),
                   stringsAsFactors = FALSE)
  
  oe <- data.frame(USUBJID = c(1,1,1,1,1,2,2,2,2,2,2),
                   OECAT  = rep("SAFETY ASSESSMENT OF LOW VISION", 11),
                   OELOC   = rep("Eye", 11),
                   OELAT   = c("LEFT", "Left", "left", "LEFT", "LEFT", 
                               "RIGHT", "right", "right", "RIGHT", "RIGHT", "right"),
                   OEDY  = c(1, 28, 56, 84, 112, 1, 28, 56, 84, 112, 140),
                   VISIT   = c("Week 1", "Week 4", "Week 8", "Week 12", "Week 16", 
                               "Week 1", "Week 4", "Week 8", "Week 12", "Week 16", "Week 20"),
                   OEDTC = c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", 
                             "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", 
                             "2021-06-01"),
                   OETEST = c("A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "F"),
                   stringsAsFactors=FALSE)
  
  oe$OECAT <- NULL
  
  expect_false(check_oe_sc_lat_count_fingers(SC=sc, OE=oe))
  
})
