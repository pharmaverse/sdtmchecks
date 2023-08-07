test_that("function returns false when RSSCAT is populated but RSCAT has NA values for the same records",{
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

test_that("function returns false when RSSCAT is populated but RSCAT has NA values for the same records and dataframe includes RSSPID",{
    expect_false(
        check_rs_rscat_rsscat(
            data.frame(
                USUBJID = c("id1", "id1", "id2", "id2", "id3"),
                RSCAT = c("A", "A", "B", NA, NA),
                RSSCAT = c("AA", "AA", "BB", "BB","AA"),
                RSSPID = c("FORMNAME-R:1/L:5XXXX", "FORMNAME-R:2/L:5XXXX", "FORMNAME1-R:5/L:5XXXX", "FORMNAME2-R:1/L:5XXXX", "FORMNAME4-R:5/L:5XXXX"),
                stringasFactors = FALSE
            )
        ))
    
})



test_that("function returns false when required variable RSCAT is not present",{
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
