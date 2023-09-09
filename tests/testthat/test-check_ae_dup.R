test_that("check_ae_dup function is TRUE when no duplicate entries by USUBJID, AETERM, AEDECOD, AESTDTC, AEENDTC, AETOXGR", {
    
    AE <- data.frame(USUBJID = c(1:3), 
                     AESTDTC = c(1:3),
                     AEENDTC = c(1:3), 
                     AEDECOD = letters[c(1:3)],
                     AETERM = letters[c(1:3)], 
                     AETOXGR = c(1:3),
                     stringsAsFactors=FALSE)
    
    expect_true(check_ae_dup(AE))
})

test_that("check_ae_dup function is TRUE when no duplicate entries by USUBJID, AETERM, AEDECOD, AESTDTC, AEENDTC, AESEV", {
    
    AE <- data.frame(USUBJID = c(1:3), 
                     AESTDTC = c(1:3),
                     AEENDTC = c(1:3), 
                     AEDECOD = letters[c(1:3)],
                     AETERM = letters[c(1:3)], 
                     AESEV = c(1:3),
                     stringsAsFactors=FALSE)
    
    expect_true(check_ae_dup(AE))
})

test_that("check_ae_dup function is FALSE when no duplicate entries by USUBJID, AETERM, AEDECOD, AESTDTC, AEENDTC, AETOXGR but AESEV and AETOXGR both populated", {
    
    AE <- data.frame(USUBJID = c(1:3), 
                     AESTDTC = c(1:3),
                     AEENDTC = c(1:3), 
                     AEDECOD = letters[c(1:3)],
                     AETERM = letters[c(1:3)], 
                     AETOXGR = c(1:3),
                     AESEV = c(1:3),
                     stringsAsFactors=FALSE)
    
    expect_false(check_ae_dup(AE))
})

test_that("check_ae_dup function is FALSE when no duplicate entries by USUBJID, AETERM, AEDECOD, AESTDTC, AEENDTC, AETOXGR but required variable missing", {
    
    AE <- data.frame(USUBJID = c(1:3), 
                     AESTDTC = c(1:3),
                     AEENDTC = c(1:3), 
                     AEDECOD = letters[c(1:3)],
                     AETERM = letters[c(1:3)], 
                     AETOXGR = c(1:3),
                     stringsAsFactors=FALSE)
    
    AE$AETOXGR <- NULL
    
    expect_false(check_ae_dup(AE))
})


test_that("check_ae_dup function is FALSE when one pair of duplicate entries by USUBJID, AETERM, AEDECOD, AESTDTC, AEENDTC, AETOXGR", {
    
    AE <- data.frame(USUBJID = c(1,1:3), 
                     AESTDTC = c(1,1:3),
                     AEENDTC = c(1,1:3), 
                     AEDECOD = letters[c(1, 1:3)],
                     AETERM = letters[c(1,1:3)], 
                     AETOXGR = c(1,1:3),
                     stringsAsFactors=FALSE)
    
    expect_false(check_ae_dup(AE))
})


test_that("check_ae_dup function is FALSE when one pair of duplicate entries by USUBJID, AETERM, AEDECOD, AESTDTC, AEENDTC, AETOXGR and both AETOXGR and AESEV populated", {
    
    AE <- data.frame(USUBJID = c(1,1:3), 
                     AESTDTC = c(1,1:3),
                     AEENDTC = c(1,1:3), 
                     AEDECOD = letters[c(1, 1:3)],
                     AETERM = letters[c(1,1:3)], 
                     AETOXGR = c(1,1:3),
                     AESEV = c(1, 1:3),
                     stringsAsFactors=FALSE)
    
    expect_false(check_ae_dup(AE))
})





test_that("check_ae_dup function is FALSE when one pair of duplicate entries by USUBJID, AETERM, AEDECOD, AESTDTC, AEENDTC, AESEV", {
    
    AE <- data.frame(USUBJID = c(1,1:3), 
                     AESTDTC = c(1,1:3),
                     AEENDTC = c(1,1:3), 
                     AEDECOD = letters[c(1, 1:3)],
                     AETERM = letters[c(1,1:3)], 
                     AESEV = c(1,1:3),
                     stringsAsFactors=FALSE)
    
    expect_false(check_ae_dup(AE))
})

test_that("check_ae_dup function is FALSE when multiple pairs of duplicates", {
    
    AE <- data.frame(USUBJID = c(1,1:3, 3), 
                     AESTDTC = c(1,1:3, 3),
                     AEENDTC = c(1,1:3, 3), 
                     AEDECOD = letters[c(1, 1:3, 3)],
                     AETERM = letters[c(1,1:3, 3)], 
                     AETOXGR = c(1,1:3, 3),
                     stringsAsFactors=FALSE)
    
    expect_false(check_ae_dup(AE))
})


test_that("check_ae_dup function is TRUE when no duplicate entries by USUBJID, AETERM, AEDECOD, AESTDTC, AEENDTC, AETOXGR, AEMODIFY", {
    
    AE <- data.frame(USUBJID = c(1:3), 
                     AESTDTC = c(1:3),
                     AEENDTC = c(1:3), 
                     AEDECOD = letters[c(1:3)],
                     AETERM = letters[c(1:3)], 
                     AETOXGR = c(1:3),
                     AEMODIFY = "",
                     stringsAsFactors=FALSE)
    
    expect_true(check_ae_dup(AE))
})

test_that("check_ae_dup function is TRUE when no duplicate entries by USUBJID, AETERM, AEDECOD, AESTDTC, AEENDTC, AESEV, AEMODIFY", {
    
    AE <- data.frame(USUBJID = c(1:3), 
                     AESTDTC = c(1:3),
                     AEENDTC = c(1:3), 
                     AEDECOD = letters[c(1:3)],
                     AETERM = letters[c(1:3)], 
                     AESEV = c(1:3),
                     AEMODIFY = "", 
                     stringsAsFactors=FALSE)
    
    expect_true(check_ae_dup(AE))
})



test_that("check_ae_dup function is FALSE when no duplicate entries by USUBJID, AETERM, AEDECOD, AESTDTC, AEENDTC, AESEV, AEMODIFY but missing key variable", {
    
    AE <- data.frame(USUBJID = c(1:3), 
                     AESTDTC = c(1:3),
                     AEENDTC = c(1:3), 
                     AEDECOD = letters[c(1:3)],
                     AETERM = letters[c(1:3)], 
                     AESEV = c(1:3),
                     AEMODIFY = "", 
                     stringsAsFactors=FALSE)
    
    AE$AEENDTC <- NULL
    
    expect_false(check_ae_dup(AE))
})



test_that("check_ae_dup function is TRUE when no duplicate entries by USUBJID, AETERM, AEDECOD, AESTDTC, AEENDTC, AETOXGR, AEMODIFY", {
    
    AE <- data.frame(USUBJID = c(1,1:3), 
                     AESTDTC = c(1,1:3),
                     AEENDTC = c(1,1:3), 
                     AEDECOD = letters[c(1, 1:3)],
                     AETERM = letters[c(1,1:3)], 
                     AETOXGR = c(1,1:3),
                     AEMODIFY = c(1, NA, NA, NA),
                     stringsAsFactors=FALSE)
    
    expect_true(check_ae_dup(AE))
})



test_that("check_ae_dup function is FALSE when duplicate entries by USUBJID, AETERM, AEDECOD, AESTDTC, AEENDTC, AETOXGR, AEMODIFY", {
    
    AE <- data.frame(USUBJID = c(1,1:3), 
                     AESTDTC = c(1,1:3),
                     AEENDTC = c(1,1:3), 
                     AEDECOD = letters[c(1, 1:3)],
                     AETERM = letters[c(1,1:3)], 
                     AETOXGR = c(1,1:3),
                     AEMODIFY = c(1,1:3), 
                     stringsAsFactors=FALSE)
    
    expect_false(check_ae_dup(AE))
})



test_that("check_ae_dup function is TRUE when no duplicate entries by USUBJID, AETERM, AEDECOD, AESTDTC, AEENDTC, AETOXGR, AEMODIFY -- even if the only difference is in AEENDTC value", {
    
    AE <- data.frame(USUBJID = c(1,1:3), 
                     AESTDTC = c(1,1:3),
                     AEENDTC = c(1,2:4), 
                     AEDECOD = letters[c(1, 1:3)],
                     AETERM = letters[c(1,1:3)], 
                     AETOXGR = c(1,1:3),
                     AEMODIFY = c(1, 1:3),
                     stringsAsFactors=FALSE)
    
    expect_true(check_ae_dup(AE))
})


test_that("check_ae_dup function is TRUE when no duplicate entries by USUBJID, AETERM, AEDECOD, AESTDTC, AEENDTC, AESEV, AEMODIFY -- even if the only difference is in AEENDTC value", {
    
    AE <- data.frame(USUBJID = c(1,1:3), 
                     AESTDTC = c(1,1:3),
                     AEENDTC = c(1,2:4), 
                     AEDECOD = letters[c(1, 1:3)],
                     AETERM = letters[c(1,1:3)], 
                     AESEV = c(1,1:3),
                     AEMODIFY = c(1, 1:3),
                     stringsAsFactors=FALSE)
    
    expect_true(check_ae_dup(AE))
})


test_that("check_ae_dup function works when AELAT included but not populated, other variables have no duplicate", {

    AE <- data.frame(USUBJID = c(1,1:3), AESTDTC = c(1,1:3),
                     AEENDTC = c(1,1:3), AEDECOD = letters[c(1:4)],
                     AETERM = letters[c(1,1:3)], AETOXGR = c(1,1:3),
                     AELAT = NA, stringsAsFactors=FALSE)

    expect_true(check_ae_dup(AE))

})

test_that("check_ae_dup function works when AELAT included but not populated, other variables have duplicates", {

    AE <- data.frame(USUBJID = c(1,1:3), AESTDTC = c(1,1:3),
                     AEENDTC = c(1,1:3), AEDECOD = letters[c(1, 1:3)],
                     AETERM = letters[c(1,1:3)], AETOXGR = c(1,1:3),
                     AELAT = NA, stringsAsFactors=FALSE)

    result <- check_ae_dup(AE)
    result_matrix <- as.matrix(attr(result, "data"))
    expected <- data.frame(
        USUBJID = c(1, 1), AETERM = c("a", "a"), AEDECOD = c("a", "a"), AESTDTC = c(1, 1),
        AEENDTC = c(1, 1), AETOXGR = c(1, 1), AELAT = c(NA, NA)
    )
    expected_matrix = as.matrix(expected)

    expect_identical(result_matrix, expected_matrix)
})

test_that("check_ae_dup function works when AELAT populated, can be used to remove duplicates caused by AELAT", {

    AE <- data.frame(USUBJID = c(1,1:3), AESTDTC = c(1,1:3),
                     AEENDTC = c(1,1:3), AEDECOD = letters[c(1, 1:3)],
                     AETERM = letters[c(1,1:3)], AETOXGR = c(1,1:3),
                     AELAT = c("RIGHT", "LEFT", "RIGHT", "LEFT"), stringsAsFactors=FALSE)

    expect_true(check_ae_dup(AE))
})



