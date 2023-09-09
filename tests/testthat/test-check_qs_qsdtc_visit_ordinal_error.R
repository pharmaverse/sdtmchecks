# no case
QS1 <- data.frame(USUBJID = c(rep(101, 5), rep(102, 5)),
                QSCAT = "DLQI",
                QSDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                VISITNUM = rep(1:5,2),
                VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "UNSCheduled!!!","VIsit 5"), 2),
                stringsAsFactors = FALSE)


test_that("Function returns true when no errors are present", {
    
    QS2 = QS1
    
    expect_true(check_qs_qsdtc_visit_ordinal_error(QS1))
})





test_that("Function returns true when no errors are present", {
    QS2 = QS1
    
    QS2$QSCAT = "SKINDEX-29"
    
    QS <- rbind(QS1, QS2)
    
    expect_true(check_qs_qsdtc_visit_ordinal_error(QS))
})




test_that("Function returns false when date out of sequence", {
    QS2 = QS1
    QS2$QSCAT = "SKINDEX-29"
    
    QS <- rbind(QS1, QS2)
    
    # adding cases with earlier date
    QS$QSDTC[QS$USUBJID == 101 & QS$VISIT == "Visit 3"] <- "2017-01-10T08:25"
    QS$QSDTC[QS$USUBJID == 102 & QS$VISIT == "Visit 2"] <- "2017-01-01T06:25"
    
    expect_false(check_qs_qsdtc_visit_ordinal_error(QS))
})




test_that("Function returns false when duplicated date", {
    QS2 = QS1
    QS2$QSCAT = "SKINDEX-29"
    
    QS <- rbind(QS1, QS2)
    
    # adding cases with duplicated date
    QS$QSDTC[QS$USUBJID == 102 & QS$VISIT == "Visit 3"] <- "2017-01-01T06:25"
    
    expect_false(check_qs_qsdtc_visit_ordinal_error(QS))
})

 