context("test-check_rs_rscat_rsscat")

test_that("function returns false when rscat is populated but rsscat has missing values",{
    expect_false(
        check_rs_rscat_rsscat(
            data.frame(
                USUBJID = c("id1", "id1", "id2", "id2", "id3"),
                RSCAT = c("A", "A", "B", NA, NA),
                RSSCAT = c("AA", "AA", "BB", "BB","AA"),
                stringasFactors = FALSE
            )
        ))

})

test_that("function returns false when variable rscat is missing",{
    expect_false(
        check_rs_rscat_rsscat(
            data.frame(
                USUBJID = c("id1", "id1", "id2", "id2", "id3"),
                RSSCAT = c("AA", "AA", "BB", "BB","AA"),
                stringasFactors = FALSE
            )
        ))

})

test_that("function returns true when both RSCAT and RSSCAT variables are populated",{
    expect_true(
        check_rs_rscat_rsscat(
            data.frame(
                USUBJID = c("id1", "id1", "id2", "id2", "id3"),
                RSCAT = c("A", "A", "B", "B", "AA"),
                RSSCAT = c("AA", "AA", "BB", "BB","AA"),
                stringasFactors = FALSE
            )
        ))

})

test_that("function returns true when RSCAT is populated but RSSCAT variables are missing",{
    expect_true(
        check_rs_rscat_rsscat(
            data.frame(
                USUBJID = c("id1", "id1", "id2", "id2", "id3"),
                RSCAT = c("A", "A", "B", "B", "AA"),
                RSSCAT = c("AA", "AA", "BB", NA,NA),
                stringasFactors = FALSE
            )
        ))
})
