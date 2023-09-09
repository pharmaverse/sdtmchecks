AE <- data.frame(USUBJID = c(1,1,1,2,2,2),
                 AEDTHDTC = c("", "", "2016-01-01", "", "2016-01", "2016-01-01"),
                 AESTDTC = "2016-01-01",
                 AEDECOD = LETTERS[1:6],
                 AETERM = LETTERS[1:6],
                 stringsAsFactors = FALSE)

DS <- data.frame(USUBJID = c(1,1,1,2,2,2),
                 DSSTDTC = "2016-01-01",
                 DSDECOD = c("A", "B", "death", "AC", "BC", "death"),
                 DSTERM = letters[1:6],
                 stringsAsFactors = FALSE)

QS <- data.frame(USUBJID = c(1,1,1,2,2,2),
                 QSDTC   = c("2015-06-30", "2015-09-30", "2015-12-30",
                             "2015-06-30", "2015-09-30", "2015-12-30"),
                 QSCAT   = "A",
                 QSORRES =  LETTERS[1:6],
                 QSSTAT  = "",
                 VISIT  =  c("Week 1", "Week 12", "Week 24", "Week 1", "Week 12", "Week 24"),
                 QSSTRESC = LETTERS[1:6],
                 stringsAsFactors = FALSE)



test_that("Function returns true when no errors are present", {
    
    expect_true(check_qs_qsdtc_after_dd(AE, DS, QS))
})


test_that("Function returns false when QSDTC is after death date for USUBJID", {
     
    QS1 <- QS
    QS1$QSDTC[3:5] <- "2016-01-03"
    
    expect_false(check_qs_qsdtc_after_dd(AE, DS, QS1))
    
})


test_that("Function returns true when no errors are present", {
    
    QS2 <- QS
    QS2$QSSTAT[3] <- "Not Done"
    
    expect_true(check_qs_qsdtc_after_dd(AE, DS, QS2))
})



test_that("Function returns true when no errors are present", {
    
    QS2 <- QS
    QS2$QSSTAT[3] <- "Not Done"
    
    expect_true(check_qs_qsdtc_after_dd(AE, DS, QS2))
})



test_that("Function returns false when key variable not available", {
    
    DS1 <- DS
    DS1$DSSTDTC <- NULL
    
    expect_false(check_qs_qsdtc_after_dd(AE, DS1, QS))
})


test_that("Function returns false when error is present", {
    
    AE1 <- data.frame(USUBJID = 1,
                      AEDTHDTC = "",
                      AESTDTC = c("2015-11-01", "2016-02-01"),
                      AEDECOD = "Rash",
                      AETERM = "RASH",
                      stringsAsFactors = FALSE)

    DS1 <- data.frame(USUBJID = 1,
                      DSSTDTC = "2016-01",
                      DSCAT = c("DISPOSITION EVENT", "OTHER"),
                      DSSCAT = c('STUDY COMPLETION/EARLY DISCONTINUATION', ''),
                      DSDECOD = "DEATH",
                      DSTERM = c("DEATH", "DEATH DUE TO PROGRESSIVE DISEASE"),
                      stringsAsFactors = FALSE)

    QS1 <- data.frame(USUBJID = 1,
                      QSDTC   = c("2015-06-30", "2016-01-15", "2016-01-15"),
                      QSCAT   = rep("EQ-5D-5L"),
                      QSORRES = "1",
                      QSSTAT  = "",
                      VISIT  =  c("Week 1", "Week 12", "Week 12"),
                      QSSTRESC = "1",
                      stringsAsFactors = FALSE)
    
    expect_false(check_qs_qsdtc_after_dd(AE1, DS1, QS1))
})
