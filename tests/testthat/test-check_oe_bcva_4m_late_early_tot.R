
test_that("Returns true when no errors present", {
  
  OE <- data.frame(
    USUBJID = "1",
    OESPID = "FORMNAME-R:2/L:2XXXX",
    OECAT = "BEST CORRECTED VISUAL ACUITY",
    OETSTDTL = "TESTING DISTANCE: 4M",
    OESCAT = c(rep("", 6), "TOTAL"),
    OESTAT = "",
    OERESCAT = c("ROW 1 - SNELLEN 20/200", 
                 "ROW 2 - SNELLEN 20/160", 
                 "ROW 4 - SNELLEN 20/100",
                 "ROW 3 - SNELLEN 20/125", 
                 "ROW 5 - SNELLEN 20/80", 
                 "ROW 6 - SNELLEN 20/63",
                 ""),
    VISIT = "WEEK 1",
    VISITNUM = 5,
    OEDTC = "2020-06-01",
    OEDY = 8,
    OELOC = "EYE",
    OELAT = "LEFT",
    OESTRESN = c(5, 5, 5, 4, 3, 0, 22)
  )
  
  expect_true(check_oe_bcva_4m_late_early_tot(OE))
  
})

test_that("Returns false when errors present - 1", {
  
  OE_too_late <- data.frame(
    USUBJID = "1",
    OESPID = "FORMNAME-R:2/L:2XXXX",
    OECAT = "BEST CORRECTED VISUAL ACUITY",
    OETSTDTL = "TESTING DISTANCE: 4M",
    OESCAT = c(rep("", 6), "TOTAL"),
    OESTAT = "",
    OERESCAT = c("ROW 1 - SNELLEN 20/200", 
                 "ROW 2 - SNELLEN 20/160", 
                 "ROW 4 - SNELLEN 20/100",
                 "ROW 3 - SNELLEN 20/125", 
                 "ROW 5 - SNELLEN 20/80", 
                 "ROW 6 - SNELLEN 20/63", 
                 ""),
    VISIT = "WEEK 1",
    VISITNUM = 5,
    OEDTC = "2020-06-01",
    OEDY = 8,
    OELOC = "EYE",
    OELAT = "LEFT",
    OESTRESN = c(5, 5, 5, 4, 3, 2, 24)
  )
  
  expect_false(check_oe_bcva_4m_late_early_tot(OE_too_late))
  
})


test_that("Returns false when errors present - 2", {
  
  OE_too_early <- data.frame(
    USUBJID = "1",
    OESPID = "FORMNAME-R:2/L:2XXXX",
    OECAT = "BEST CORRECTED VISUAL ACUITY",
    OETSTDTL = "TESTING DISTANCE: 4M",
    OESCAT = c(rep("", 6), "TOTAL"),
    OESTAT = "",
    OERESCAT = c("ROW 1 - SNELLEN 20/200", 
                 "ROW 2 - SNELLEN 20/160", 
                 "ROW 4 - SNELLEN 20/100",
                 "ROW 3 - SNELLEN 20/125", 
                 "ROW 5 - SNELLEN 20/80",
                 "ROW 6 - SNELLEN 20/63",
                 ""),
    VISIT = "WEEK 1",
    VISITNUM = 5,
    OEDTC = "2020-06-01",
    OEDY = 8,
    OELOC = "EYE",
    OELAT = "LEFT",
    OESTRESN = c(5, 5, 5, 4, 4, 5, 28)
  )
  
  expect_false(check_oe_bcva_4m_late_early_tot(OE_too_early))
  
})

test_that("Returns false when errors present - 3", {
  
  OE_total_incorrect <- data.frame(
    USUBJID = "1",
    OESPID = "FORMNAME-R:2/L:2XXXX",
    OECAT = "BEST CORRECTED VISUAL ACUITY",
    OETSTDTL = "TESTING DISTANCE: 4M",
    OESCAT = c(rep("", 6), "TOTAL"),
    OESTAT = "",
    OERESCAT = c("ROW 1 - SNELLEN 20/200", 
                 "ROW 2 - SNELLEN 20/160", 
                 "ROW 4 - SNELLEN 20/100",
                 "ROW 3 - SNELLEN 20/125", 
                 "ROW 5 - SNELLEN 20/80", 
                 "ROW 6 - SNELLEN 20/63", 
                 ""),
    VISIT = "WEEK 1",
    VISITNUM = 5,
    OEDTC = "2020-06-01",
    OEDY = 8,
    OELOC = "EYE",
    OELAT = "LEFT",
    OESTRESN = c(5, 5, 5, 4, 4, 2, 28)
  )
  
  expect_false(check_oe_bcva_4m_late_early_tot(OE_total_incorrect))
  
})



test_that("Returns false when expected column not present", {
  
  OE <- data.frame(
    USUBJID = "1",
    OESPID = "FORMNAME-R:2/L:2XXXX",
    OECAT = "BEST CORRECTED VISUAL ACUITY",
    OETSTDTL = "TESTING DISTANCE: 4M",
    OESCAT = c(rep("", 6), "TOTAL"),
    OESTAT = "",
    OERESCAT = c("ROW 1 - SNELLEN 20/200", 
                 "ROW 2 - SNELLEN 20/160", 
                 "ROW 4 - SNELLEN 20/100",
                 "ROW 3 - SNELLEN 20/125", 
                 "ROW 5 - SNELLEN 20/80", 
                 "ROW 6 - SNELLEN 20/63",
                 ""),
    VISIT = "WEEK 1",
    VISITNUM = 5,
    OEDTC = "2020-06-01",
    OEDY = 8,
    OELOC = "EYE",
    OELAT = "LEFT",
    OESTRESN = c(5, 5, 5, 4, 3, 0, 22)
  )
  
  OE$USUBJID <- NULL
  
  expect_false(check_oe_bcva_4m_late_early_tot(OE))
  
})
