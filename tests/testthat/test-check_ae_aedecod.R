# test_that("function errors when given a bad input", {
#   expect_error(check_ae_aedecod(list()))
#
# })

test_that("Function returns true when no errors are present", {
  
  AE <- data.frame(
    USUBJID = 1:5,
    AESEQ = 1:5,
    AESTDTC = 1:5,
    AETERM = 1:5,
    AEDECOD = 1:5
  )
  
  
  expect_true(check_ae_aedecod(AE))
})

test_that("Function returns false when errors are present", {
  
  AE <- data.frame(
    USUBJID = 1:5,
    AESEQ = 1:5,
    AESTDTC = 1:5,
    AETERM = 1:5,
    AEDECOD = 1:5
  )
  AE$AEDECOD[1]=NA
  
  expect_false(check_ae_aedecod(AE))
})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {
  
  AE <- data.frame(USUBJID =character(),
                   AESEQ=character(),
                   AESTDTC=character(),
                   AETERM=character(),
                   AEDECOD = character(),
                   stringsAsFactors=FALSE)
  
  expect_true(check_ae_aedecod(AE))
})

# test_that("Function returns false when errors are present for an empty dataframe (zero rows)", {
# 
#   AE <- data.frame(USUBJID =NA,
#                    AESEQ=NA,
#                    AESTDTC=NA,
#                    AETERM="",
#                    AEDECOD ="NA",
#                    stringsAsFactors=FALSE)
# 
#   expect_false(check_ae_aedecod(AE))
# })

test_that("Function returns true when no errors are present for a single input (one row)", {
  
  AE <- data.frame(
    USUBJID = 1,
    AESEQ = 1,
    AESTDTC = 1,
    AETERM = 1,
    AEDECOD = 1
  )
  
  expect_true(check_ae_aedecod(AE))
})

test_that("Function returns false when errors are present for a single input (one row)", {
  
  AE <- data.frame(
    USUBJID = 1,
    AESEQ = 1,
    AESTDTC = 1,
    AETERM = 1,
    AEDECOD = "NA"
  )
  
  expect_false(check_ae_aedecod(AE))
})

test_that("Function returns true when no errors are present for multiple inputs (54,000 rows)", {
  
  USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 60)
  
  AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                   AESEQ = rep(x = seq(1, 60, by=1), times = 9000),
                   AESTDTC = rep(seq(as.Date('2023-01-01'), as.Date('2023-03-01'), by = 1), times = 9000),
                   AETERM = rep(x = c("UPPER RESPIRATORY INFECTION","HEADACHE",
                                      "WORSENING THROMBOCYTOPENIA","LOW POTASSIUM","ANEMIA", "TREMORS"), times = 9000),
                   AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
                                       "Blood Potassium Decreased","Anaemia","Tremor"), times = 9000),
                   stringsAsFactors = FALSE)
  
  expect_true(check_ae_aedecod(AE))
})



test_that("Function returns false when errors are present for a multiple inputs (54,000 rows)", {
  
  USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 60)
  
  AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                   AESEQ = rep(x = seq(1, 60, by=1), times = 900),
                   AESTDTC = rep(seq(as.Date('2023-01-01'), as.Date('2023-03-01'), by = 1), times = 900),
                   AETERM = rep(x = c("UPPER RESPIRATORY INFECTION","HEADACHE",
                                      "WORSENING THROMBOCYTOPENIA","LOW POTASSIUM","ANEMIA", "TREMORS"), times = 9000),
                   AEDECOD = rep(x = c("Upper Respiratory Tract Infection","NA","Thrombocytopenia",
                                       "Blood Potassium Decreased","",NA), times = 9000),
                   stringsAsFactors = FALSE)
  
  expect_false(check_ae_aedecod(AE))
})

test_that("Function returns false when errors are present when XXSPID is available", {
  
  AE <- data.frame(
    STUDY = c(rep("STUDYABC", 6)),
    DOMAIN = c(rep("AE", 6)),
    USUBJID = c(rep("STUDYABC-0000000-12345",6)),
    AESEQ = c(1, 2, 3, 4, 5, 6),
    AESPID = c("AEFORM:0123456-R:1/L:1/AT:INITIALEXTREME",
               "AEFORM:0123456-R:2/L:2/AT:INITIALEXTREME",
               "AEFORM:0123456-R:5/L:5/AT:INITIALEXTREME",
               "AEFORM:0123456-R:7/L:7/AT:INITIALEXTREME",
               "AEFORM:0123456-R:8/L:8/AT:INITIALEXTREME",
               "AEFORM:0123456-R:9/L:9/AT:INITIALEXTREME"),
    AETERM = c("ANEMIA", "ANEMIA", "ANOREXIA", "CONSTIPATION", "DIARRHEA", "FATIGUE"),
    AEDECOD = c("ANAEMIA", "ANAEMIA", "DECREASED APPETITE", "CONSTIPATION", "DIARRHOEA", NA),
    AESTDTC = c("2012-01-01", "2012-02-02", "2013-03-01", "2014-01-01", "2014-01-02", "2014-01-03"),
    stringsAsFactors = FALSE
  )
  
  
  expect_false(check_ae_aedecod(AE))
})


test_that("Function returns false when any of the required variables are missing: USUBJID is missing", {
  
  AE <- data.frame(
    STUDY = c(rep("STUDYABC", 6)),
    DOMAIN = c(rep("AE", 6)),
    AESPID = c("AEFORM:0123456-R:1/L:1/AT:INITIALEXTREME",
               "AEFORM:0123456-R:2/L:2/AT:INITIALEXTREME",
               "AEFORM:0123456-R:5/L:5/AT:INITIALEXTREME",
               "AEFORM:0123456-R:7/L:7/AT:INITIALEXTREME",
               "AEFORM:0123456-R:8/L:8/AT:INITIALEXTREME",
               "AEFORM:0123456-R:9/L:9/AT:INITIALEXTREME"),
    AETERM = c("ANEMIA", "ANEMIA", "ANOREXIA", "CONSTIPATION", "DIARRHEA", "FATIGUE"),
    AEDECOD = c("ANAEMIA", "ANAEMIA", "DECREASED APPETITE", "CONSTIPATION", "DIARRHOEA", NA),
    AESTDTC = c("2012-01-01", "2012-02-02", "2013-03-01", "2014-01-01", "2014-01-02", "2014-01-03"),
    stringsAsFactors = FALSE
  )
  
  expect_false(check_ae_aedecod(AE))
})


test_that("Function returns the failed object in attr(data)", {
  AE <- data.frame(
    USUBJID = 1:5,
    AESTDTC = 1:5,
    AETERM = letters[1:5],
    AEDECOD = 1:5, stringsAsFactors = FALSE
  )
  AE$AEDECOD[1]=NA
  
  check <- check_ae_aedecod(AE)
  
  expect_true(!is.null(attr(check, "data")))
  expect_equal(attr(check, "data"), filter(AE, is.na(AEDECOD)))
  
})