test_that("Returns true when no errors present - 1", {
  
  #Using New Standard, PASS Case
  
  OE <- data.frame(
    USUBJID = 1,
    OETESTCD = c("NUMLCOR", "NUMLCOR", "LCORCON", "VACSCORE"),
    OECAT   = rep("BEST CORRECTED VISUAL ACUITY", 4),
    OESCAT   = c("NORMAL LIGHTING SCORE", "NORMAL LIGHTING SCORE","", ""),
    OETSTDTL  = c("TESTING DISTANCE: 4M", "TESTING DISTANCE: 1M", "", ""),
    OESTRESN = c(22, 0, 30, 52),
    OESTAT= rep("", 4),
    OELOC   = rep("EYE", 4),
    OELAT   = rep("LEFT", 4),
    VISIT   = rep("SCREENING", 4),
    VISITNUM   = rep(99, 4),
    OEDTC = rep("2021-05-19", 4),
    OEDY  = rep(1, 4),
    stringsAsFactors = FALSE)
  
  expect_true(check_oe_bcva_tot_mismatch(OE))
  
})

test_that("Returns true when no errors present - 2", {
  
  #FAIL Case without optional variable, OESTAT
  
  OE <- data.frame(
    USUBJID = 1,
    OETESTCD = c("NUMLCOR", "NUMLCOR", "LCORCON", "VACSCORE"),
    OECAT   = rep("BEST CORRECTED VISUAL ACUITY", 4),
    OESCAT   = c("NORMAL LIGHTING SCORE", "NORMAL LIGHTING SCORE","", ""),
    OETSTDTL  = c("TESTING DISTANCE: 4M", "TESTING DISTANCE: 1M", "", ""),
    OESTRESN = c(22, 0, 30, 52),
    OESTAT= rep("", 4),
    OELOC   = rep("EYE", 4),
    OELAT   = rep("LEFT", 4),
    VISIT   = rep("SCREENING", 4),
    VISITNUM   = rep(99, 4),
    OEDTC = rep("2021-05-19", 4),
    OEDY  = rep(1, 4),
    stringsAsFactors = FALSE)
  
  OE$OESTAT <- NULL
  
  expect_true(check_oe_bcva_tot_mismatch(OE))
  
})


test_that("Returns false when errors present - 1", {
  
  #Using Old Standard, FAIL Case (4m <=19, so 4m + 1m to match with Rave Total)
  OE <- data.frame(
    USUBJID = 1,
    OETESTCD = c("NUMLCOR", "NUMLCOR", "LCORCON", "LOGSCORE"),
    OECAT   = rep("BEST CORRECTED VISUAL ACUITY", 4),
    OESCAT   = c("TOTAL", "TOTAL","", ""),
    OETSTDTL  = c("TESTING DISTANCE: 4M", "TESTING DISTANCE: 1M", "", ""),
    OESTRESN = c(18, 0, 30, 48),
    OESTAT= rep("", 4),
    OELOC   = rep("EYE", 4),
    OELAT   = rep("LEFT", 4),
    VISIT   = rep("SCREENING", 4),
    VISITNUM   = rep(99, 4),
    OEDTC = rep("2021-05-19", 4),
    OEDY  = rep(1, 4),
    stringsAsFactors = FALSE)
  
  expect_false(check_oe_bcva_tot_mismatch(OE))
  
})


test_that("Returns false when errors present - 2", {
  
  #Using New Standard, FAIL Case (Total 4m + 1m (As 4m <=19) not equal to CRF Total Score)
  
  OE <- data.frame(
    USUBJID = 1,
    OETESTCD = c("NUMLCOR", "NUMLCOR", "LCORCON", "VACSCORE"),
    OECAT   = "BEST CORRECTED VISUAL ACUITY",
    OESCAT   = c("NORMAL LIGHTING SCORE", "NORMAL LIGHTING SCORE","", ""),
    OETSTDTL  = c("TESTING DISTANCE: 4M", "TESTING DISTANCE: 1M", "", ""),
    OESTRESN = c(17, 12, 0, 27),
    OESTAT= "",
    OELOC   = "EYE",
    OELAT   = "LEFT",
    VISIT   = "SCREENING",
    VISITNUM   = 99,
    OEDTC = "2021-05-19",
    OEDY  = 1,
    stringsAtors = FALSE)

  expect_false(check_oe_bcva_tot_mismatch(OE))

})

 
test_that("Returns false when errors present - 3", {
  
  OE <- data.frame(
    USUBJID = 1,
    OETESTCD = c("NUMLCOR", "NUMLCOR", "LCORCON", "LOGSCORE"),
    OECAT   = rep("BEST CORRECTED VISUAL ACUITY", 4),
    OESCAT   = c("TOTAL", "TOTAL","", ""),
    OETSTDTL  = c("TESTING DISTANCE: 4M", "TESTING DISTANCE: 1M", "", ""),
    OESTRESN = c(18, 0, 30, 48),
    OESTAT= rep("", 4),
    OELOC   = rep("EYE", 4),
    OELAT   = rep("LEFT", 4),
    VISIT   = rep("SCREENING", 4),
    VISITNUM   = rep(99, 4),
    OEDTC = rep("2021-05-19", 4),
    OEDY  = rep(1, 4),
    stringsAsFactors = FALSE)
  
  OE$OESTAT <- NULL
  
  expect_false(check_oe_bcva_tot_mismatch(OE))
  
})


test_that("Returns false when expected column not present", {
  
  OE <- data.frame(
    USUBJID = 1,
    OETESTCD = c("NUMLCOR", "NUMLCOR", "LCORCON", "VACSCORE"),
    OECAT   = rep("BEST CORRECTED VISUAL ACUITY", 4),
    OESCAT   = c("NORMAL LIGHTING SCORE", "NORMAL LIGHTING SCORE","", ""),
    OETSTDTL  = c("TESTING DISTANCE: 4M", "TESTING DISTANCE: 1M", "", ""),
    OESTRESN = c(22, 0, 30, 52),
    OESTAT= rep("", 4),
    OELOC   = rep("EYE", 4),
    OELAT   = rep("LEFT", 4),
    VISIT   = rep("SCREENING", 4),
    VISITNUM   = rep(99, 4),
    OEDTC = rep("2021-05-19", 4),
    OEDY  = rep(1, 4),
    stringsAsFactors = FALSE)
  
  #missing required variable, OETESTCD
  OE$OETESTCD <- NULL
  
  expect_false(check_oe_bcva_tot_mismatch(OE))
  
})

