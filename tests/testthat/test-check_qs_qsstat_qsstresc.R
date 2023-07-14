

QS <- data.frame(
    STUDYID = 1,
    USUBJID = c(rep(1,6),rep(2,6)),
    QSSTRESC = 1:12,
    VISIT = c(rep(1,3),rep(2,3),rep(1,3),rep(2,3)),
    QSSTAT = rep(c("DONE","NOT DONE"),6),
    QSCAT = rep(c("INDIVIDUAL","OVERALL","BFI"),4),
    QSDTC = "2016-01-01",
    QSTESTCD = "QSALL",
    stringsAsFactors = FALSE
)


test_that("Function returns false when errors are present", {
    
    expect_false(check_qs_qsstat_qsstresc(QS))
})


test_that("Function returns false when errors are present", {
    
    QS1 <- QS
    QS1$QSSTRESC[4]=" "
    QS1$QSSTRESC[6]=NA
    QS1$QSSTRESC[8]="."
    
    expect_false(check_qs_qsstat_qsstresc(QS1))
})



test_that("Function returns false when missing key variable", {
    
    QS2 <- QS
    QS2$QSSTRESC=NULL
    
    expect_false(check_qs_qsstat_qsstresc(QS2))
})

