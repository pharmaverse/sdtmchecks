
test_that("Returns true when no errors present - 1", {
  TU <- data.frame(
    USUBJID = 1:3,
    TUSTRESC = c("INV001","NEW","NEW"),
    TUDTC = "2017-01-01",
    TUEVAL = "INDEPENDENT ASSESSOR"
  )
  
  RS <- data.frame(
    USUBJID = 1:2,
    RSSTRESC = c("SD","NE"),
    RSTESTCD = 'OVRLRESP',
    RSEVAL = "INDEPENDENT ASSESSOR"
  )
  
  ## pass if by IRF, even if NEW in TU
  expect_true(check_tu_rs_new_lesions(RS,TU))
  
  
})

test_that("Returns true when no errors present - 2", {
  
    TU <- data.frame(
      USUBJID = 1:3,
      TUSTRESC = c("INV001","NEW","NEW"),
      TUDTC = "2017-01-01"
    )
    
    RS <- data.frame(
      USUBJID = 1:3,
      RSSTRESC = c("SD","PD", "PD"),
      RSTESTCD = 'OVRLRESP'
    )
  
    
    ## pass if by IRF, even if NEW in TU
    expect_true(check_tu_rs_new_lesions(RS,TU))
    
    
})


test_that("Returns false when errors present - 1", {
  
  TU <- data.frame(
    USUBJID = 1:3,
    TUSTRESC = c("INV001","NEW","NEW"),
    TUDTC = "2017-01-01"
  )
  
  RS <- data.frame(
    USUBJID = 1:3,
    RSSTRESC = c("SD","NE", "NE"),
    RSTESTCD = 'OVRLRESP'
  )
  
  
  
  # flag USUBJIDs with NEW 
  expect_false(check_tu_rs_new_lesions(RS,TU))
  
})

test_that("Returns false when errors present - 2", {
  
  TU <- data.frame(
    USUBJID = 1:3,
    TUSTRESC = c("INV001","NEW","NEW"),
    TUDTC = "2017-01-01"
  )
  
  RS <- data.frame(
    USUBJID = 1:2,
    RSSTRESC = c("SD","NE"),
    RSTESTCD = 'OVRLRESP'
  )
  
  
  RS$RSSTRESC[2] = "PD"
  
  # flag USUBJID with NEW and without PD
  expect_false(check_tu_rs_new_lesions(RS,TU))
  
})


test_that("Returns false when expected column not present - 1", {
  
  TU <- data.frame(
    USUBJID = 1:3,
    TUSTRESC = c("INV001","NEW","NEW"),
    TUDTC = "2017-01-01"
  )
  
  RS <- data.frame(
    USUBJID = 1:3,
    RSSTRESC = c("SD","NE", "NE")
  )
  
  # required variable is missing 
  expect_false(check_tu_rs_new_lesions(RS,TU))
  
})

test_that("Returns false when expected column not present - 2", {
  
  TU <- data.frame(
    USUBJID = 1:3,
    TUSTRESC = c("INV001","NEW","NEW"),
    TUDTC = "2017-01-01"
  )
  
  RS <- data.frame(
    USUBJID = 1:3,
    RSSTRESC = c("SD","PD", "PD"),
    RSTESTCD = 'OVRLRESP'
  )
  
  
  TU$TUDTC <- NULL
  
  # required dataset missing 
  expect_false(check_tu_rs_new_lesions(RS,TU))
  
})



test_that("Returns false when expected dataset missing", {
  
  TU <- data.frame(
    USUBJID = 1:3,
    TUSTRESC = c("INV001","NEW","NEW"),
    TUDTC = "2017-01-01"
  )
  
  RS <- data.frame(
    USUBJID = 1:2,
    RSSTRESC = c("SD","NE"),
    RSTESTCD = 'OVRLRESP'
  )
  
  
  
  RS <- NULL
  
  # required dataset missing 
  expect_false(check_tu_rs_new_lesions(RS,TU))
})








test_that("Returns false when errors present - 3", {
  
  TU <- data.frame(
   USUBJID = 1:3,
   TUSTRESC = c("INV001","NEW","NEW"),
   TUDTC = "2017-01-01"
  )
  
  RS <- data.frame(
   USUBJID = 1:2,
   RSSTRESC = c("SD","NE"),
   RSTESTCD="OVRLRESP"
  )

  RS$RSSTRESC[2] = "PMD"
  
  
  expect_false(check_tu_rs_new_lesions(RS,TU))
  
})


test_that("Returns true when no errors present - 3", {
  
  TU <- data.frame(
    USUBJID = 1:2,
    TUSTRESC = c("INV001","NEW"),
    TUDTC = "2017-01-01"
  )
  
  RS <- data.frame(
    USUBJID = 1:2,
    RSSTRESC = c("SD","NE"),
    RSTESTCD="OVRLRESP"
  )
  
  RS$RSSTRESC[2] = "PMD"
  
  expect_true(check_tu_rs_new_lesions(RS,TU))
  
})



test_that("Returns true when no errors present - 4", {
  
  TU <- data.frame(
    USUBJID = 1:2,
    TUSTRESC = c("INV001","NEW"),
    TUDTC = "2017-01-01"
  )
  
  RS <- data.frame(
    USUBJID = 1:2,
    RSSTRESC = c("SD","NE"),
    RSTESTCD="OVRLRESP"
  )
  
  RS$RSSTRESC[2] = "PMD"
  
  
  expect_true(check_tu_rs_new_lesions(RS,TU))
  
})

