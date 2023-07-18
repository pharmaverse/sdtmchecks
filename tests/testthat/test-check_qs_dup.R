QS1 <- data.frame(USUBJID = c(rep(101, 5), rep(102, 5)),
                QSCAT = "DLQI",
                QSDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25",
                 "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
                VISITNUM = rep(1:5,2),
                VISIT = rep(c( "Visit 1", "Visit 2", "Visit 3", "UNSCheduled!!!","VIsit 5"), 2),
                stringsAsFactors = FALSE)


test_that("Function returns true when no errors are present", {
    
    expect_true(check_qs_dup(QS1))
})


test_that("Function returns false when multiple dates, QSDTC, for the same visit", {
    
    # multiple dates for the same visit in QS
    QS2 <- QS1
    QS2$VISIT[QS2$USUBJID == 101] <- "Visit 1"
    
    expect_false(check_qs_dup(QS2))
})



test_that("Function returns true when multiple visit labels for the same date, QSDTC", {
    
    # multiple visit labels for the same date
    QS3 <- QS1
    QS3$QSDTC[QS3$USUBJID == 101] <- "2017-01-01"
    
    expect_true(check_qs_dup(QS3))
})




