context("test-check_ae_dup")

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
