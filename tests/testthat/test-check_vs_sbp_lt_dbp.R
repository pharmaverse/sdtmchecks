test_that("Returns true when no errors present", {
  
  vs <- data.frame(
    STUDYID = 1,
    USUBJID = 1,
    VSSPID = c("1","2","1","2"),
    VISIT = 1,
    VSDTC = c("2010-01-01","2010-01-01","2010-01-01","2010-01-01"),
    VSTESTCD = c("SYSBP","SYSBP",
                 "DIABP","DIABP")
    ,
    VSSTRESN = c(120,120,100,80)
  )
  
  expect_true(check_vs_sbp_lt_dbp(VS=vs))
  
})

test_that("Returns false when errors present", {
  
  vs <- data.frame(
    STUDYID = 1,
    USUBJID = 1,
    VSSPID = c("1","2","1","2"),
    VISIT = 1,
    VSDTC = c("2010-01-01","2010-01-01","2010-01-01","2010-01-01"),
    VSTESTCD = c("SYSBP","SYSBP",
                 "DIABP","DIABP")
    ,
    VSSTRESN = c(80,120,100,80)
  )
  
  expect_false(check_vs_sbp_lt_dbp(VS=vs))
  
})

test_that("Returns false when expected column not present", {
  
  vs <- data.frame(
    STUDYID = 1,
    USUBJID = 1,
    VSSPID = c("1","2","1","2"),
    VISIT = 1,
    VSDTC = c("2010-01-01","2010-01-01","2010-01-01","2010-01-01"),
    VSTESTCD = c("SYSBP","SYSBP",
                 "DIABP","DIABP")
    ,
    VSSTRESN = c(80,120,100,80)
  )
  
  vs0 <- subset(vs, select = c(USUBJID, VSSPID, VSSTRESN))
  
  expect_false(check_vs_sbp_lt_dbp(VS=vs0))
  
})


