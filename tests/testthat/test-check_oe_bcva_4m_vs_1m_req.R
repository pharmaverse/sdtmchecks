test_that("Returns true when no errors present", {
  
  OE_1m_done <- data.frame(
    USUBJID = "1",
    OECAT = "BEST CORRECTED VISUAL ACUITY",
    OETSTDTL = c(rep("TESTING DISTANCE: 4M", 4), rep("TESTING DISTANCE: 1M", 3)),
    OESCAT = c(rep("", 3), "TOTAL", rep("", 2), "TOTAL"),
    OESTAT = rep("", 7),
    OERESCAT = c("ROW 1 - SNELLEN 20/200", 
                 "ROW 2 - SNELLEN 20/160", 
                 "ROW 3 - SNELLEN 20/125", 
                 "",
                 "ROW 1 - SNELLEN 20/200", 
                 "ROW 2 - SNELLEN 20/160", 
                 ""),
    VISIT = "WEEK 1",
    VISITNUM = 5,
    OEDTC = "2020-06-01",
    OEDY = 8,
    OELOC = "EYE",
    OELAT = "LEFT",
    OESTRESN = c(9, 9, 3, 21, 3, 2, NA)
  )
  
  expect_true(check_oe_bcva_4m_vs_1m_req(OE_1m_done))
  
}) 


test_that("Returns false when errors present - 1", {
  
  OE_1m_done <- data.frame(
    USUBJID = "1",
    OECAT = "BEST CORRECTED VISUAL ACUITY",
    OETSTDTL = c(rep("TESTING DISTANCE: 4M", 4), rep("TESTING DISTANCE: 1M", 3)),
    OESCAT = c(rep("", 3), "TOTAL", rep("", 2), "TOTAL"),
    OESTAT = rep("", 7),
    OERESCAT = c("ROW 1 - SNELLEN 20/200", 
                 "ROW 2 - SNELLEN 20/160", 
                 "ROW 3 - SNELLEN 20/125", 
                 "",
                 "ROW 1 - SNELLEN 20/200", 
                 "ROW 2 - SNELLEN 20/160", 
                 ""),
    VISIT = "WEEK 1",
    VISITNUM = 5,
    OEDTC = "2020-06-01",
    OEDY = 8,
    OELOC = "EYE",
    OELAT = "LEFT",
    OESTRESN = c(9, 9, 3, 21, 3, 2, 5)
  )
  
  expect_false(check_oe_bcva_4m_vs_1m_req(OE_1m_done))
  
}) 

test_that("Returns false when errors present - 2", {
  
  OE_1m_not_done <- data.frame(
    USUBJID = "1",
    OECAT = "BEST CORRECTED VISUAL ACUITY",
    OETSTDTL = "TESTING DISTANCE: 4M",
    OESCAT = c(rep("", 3), "TOTAL"),
    OESTAT = "",
    OERESCAT = c("ROW 1 - SNELLEN 20/200", 
                 "ROW 2 - SNELLEN 20/160", 
                 "ROW 3 - SNELLEN 20/125", 
                 ""),
    VISIT = "WEEK 1",
    VISITNUM = 5,
    OEDTC = "2020-06-01",
    OEDY = 8,
    OELOC = "EYE",
    OELAT = "LEFT",
    OESTRESN = c(5, 5, 2, 12)
  )
  
  expect_false(check_oe_bcva_4m_vs_1m_req(OE_1m_not_done))
  
})

test_that("Returns false when expected column not present", {
  
  OE_1m_done <- data.frame(
    USUBJID = "1",
    OECAT = "BEST CORRECTED VISUAL ACUITY",
    OETSTDTL = c(rep("TESTING DISTANCE: 4M", 4), rep("TESTING DISTANCE: 1M", 3)),
    OESCAT = c(rep("", 3), "TOTAL", rep("", 2), "TOTAL"),
    OESTAT = rep("", 7),
    OERESCAT = c("ROW 1 - SNELLEN 20/200", 
                 "ROW 2 - SNELLEN 20/160", 
                 "ROW 3 - SNELLEN 20/125", 
                 "",
                 "ROW 1 - SNELLEN 20/200", 
                 "ROW 2 - SNELLEN 20/160", 
                 ""),
    VISIT = "WEEK 1",
    VISITNUM = 5,
    OEDTC = "2020-06-01",
    OEDY = 8,
    OELOC = "EYE",
    OELAT = "LEFT",
    OESTRESN = c(9, 9, 3, 21, 3, 2, NA)
  )
  
  OE_1m_done$USUBJID <- NULL
  
  expect_false(check_oe_bcva_4m_vs_1m_req(OE_1m_done))
  
})
