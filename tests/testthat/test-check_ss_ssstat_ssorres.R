test_that("Returns true when no errors present - 1", {
  
  SS <- data.frame(
    STUDYID = 1,
    USUBJID = c(rep(1,6),rep(2,6)),
    SSSTRESC = c("ALIVE", "DEAD", "ALIVE", "", "", "U"),
    SSORRES = c("ALIVE", "DEAD", "ALIVE", "", "", "U"),
    VISIT = rep(c("SURVIVAL FOLLOW UP 3 MONTHS"),6),
    SSSTAT = rep(c("","NOT DONE"),6),
    SSDTC = "2016-01-01",
    SSSPID = "",
    stringsAsFactors = FALSE
  )
  
  SS <- SS[1,]
  
  expect_true(check_ss_ssstat_ssorres(SS))

  
})

test_that("Returns true when no errors present - 2", {
  
  SS <- data.frame(
    STUDYID = 1,
    USUBJID = c(rep(1,6),rep(2,6)),
    SSSTRESC = c("ALIVE", "DEAD", "ALIVE", "", "", "U"),
    SSORRES = c("ALIVE", "DEAD", "ALIVE", "", "", "U"),
    VISIT = rep(c("SURVIVAL FOLLOW UP 3 MONTHS"),6),
    SSSTAT = rep(c("","NOT DONE"),6),
    SSDTC = "2016-01-01",
    SSSPID = "",
    stringsAsFactors = FALSE
  )
  
  SS$SSSPID="FORMNAME-R:5/L:5XXXX"
  SS <- SS[1,]
  
  expect_true(check_ss_ssstat_ssorres(SS,preproc=roche_derive_rave_row))
  
})

test_that("Returns true when no errors present - 3", {
  
  SS <- data.frame(
    STUDYID = 1,
    USUBJID = c(rep(1,6),rep(2,6)),
    SSSTRESC = c("ALIVE", "DEAD", "ALIVE", "", "", "U"),
    SSORRES = c("ALIVE", "DEAD", "ALIVE", "", "", "U"),
    VISIT = rep(c("SURVIVAL FOLLOW UP 3 MONTHS"),6),
    SSSTAT = rep(c("","", "", "NOT DONE", "NOT DONE", ""),2),
    SSDTC = "2016-01-01",
    SSSPID = "",
    stringsAsFactors = FALSE
  )
  
  SS$SSORRES[2]=NA
  
  expect_true(check_ss_ssstat_ssorres(SS))
})


 
test_that("Returns false when errors present - 1", {
  
  SS <- data.frame(
    STUDYID = 1,
    USUBJID = c(rep(1,6),rep(2,6)),
    SSSTRESC = c("ALIVE", "DEAD", "ALIVE", "", "", ""),
    SSORRES = c("ALIVE", "DEAD", "ALIVE", "", "", ""),
    VISIT = rep(c("SURVIVAL FOLLOW UP 3 MONTHS"),6),
    SSSTAT = rep(c("","NOT DONE"),2),
    SSDTC = "2016-01-01",
    SSSPID = "",
    stringsAsFactors = FALSE
  )
  
  expect_false(check_ss_ssstat_ssorres(SS))
  
})



test_that("Returns false when errors present - 2", {
  
  SS <- data.frame(
    STUDYID = 1,
    USUBJID = c(rep(1,6),rep(2,6)),
    SSSTRESC = c("ALIVE", "DEAD", "ALIVE", "", "", "U"),
    SSORRES = c("ALIVE", "DEAD", "ALIVE", "", "", "U"),
    VISIT = rep(c("SURVIVAL FOLLOW UP 3 MONTHS"),6),
    SSSTAT = rep(c("", "NOT DONE"),6),
    SSDTC = "2016-01-01",
    SSSPID = "",
    stringsAsFactors = FALSE
  )
  
  SS$SSORRES[2]=NA
  
  expect_false(check_ss_ssstat_ssorres(SS))
})

test_that("Returns false when errors present - 3", {
  
  SS <- data.frame(
    STUDYID = 1,
    USUBJID = c(rep(1,6),rep(2,6)),
    SSSTRESC = c("ALIVE", "DEAD", "ALIVE", "", "", "U"),
    SSORRES = c("ALIVE", "DEAD", "ALIVE", "", "", "U"),
    VISIT = rep(c("SURVIVAL FOLLOW UP 3 MONTHS"),6),
    SSSTAT = rep(c("","NOT DONE"),6),
    SSDTC = "2016-01-01",
    SSSPID = "",
    stringsAsFactors = FALSE
  )
  
  SS$SSSPID="FORMNAME-R:5/L:5XXXX"
  
  expect_false(check_ss_ssstat_ssorres(SS,preproc=roche_derive_rave_row))
  
})



test_that("Returns false when errors present - 4", {
  
  SS <- data.frame(
    STUDYID = 1,
    USUBJID = c(rep(1,6),rep(2,6)),
    SSSTRESC = c("ALIVE", "DEAD", "ALIVE", "", "", "U"),
    SSORRES = c("ALIVE", "DEAD", "ALIVE", "", "", "U"),
    VISIT = rep(c("SURVIVAL FOLLOW UP 3 MONTHS"),6),
    SSSTAT = rep(c("","NOT DONE"),6),
    SSDTC = "2016-01-01",
    SSSPID = "",
    stringsAsFactors = FALSE
  )
  
  SS$SSORRES[6]=NA
  SS$SSORRES[8]=""
  SS$SSORRES[12]=NA
  
  expect_false(check_ss_ssstat_ssorres(SS))
})

 
test_that("Returns false when expected column not present", {
  
  SS <- data.frame(
    STUDYID = 1,
    USUBJID = c(rep(1,6),rep(2,6)),
    SSSTRESC = c("ALIVE", "DEAD", "ALIVE", "", "", "U"),
    SSORRES = c("ALIVE", "DEAD", "ALIVE", "", "", "U"),
    VISIT = rep(c("SURVIVAL FOLLOW UP 3 MONTHS"),6),
    SSSTAT = rep(c("","NOT DONE"),6),
    SSDTC = "2016-01-01",
    SSSPID = "",
    stringsAsFactors = FALSE
  )
  
  
  SS$SSORRES=NULL
  
  
  expect_false(check_ss_ssstat_ssorres(SS))
  
})


