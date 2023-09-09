

test_that("Function returns false when errors are present", {
    
    covid_df = data.frame(REFTERM = c("COVID-19",
                                      "CORONAVIRUS POSITIVE")
    )
    
    AE <- data.frame(
        STUDYID = 1,
        USUBJID = c(1,2,3,1,2,3),
        AESTDTC = '2020-05-05',
        AETERM  = c("abc Covid-19", "covid TEST POSITIVE",rep("other AE",4)),
        AEDECOD = c("COVID-19", "CORONAVIRUS POSITIVE", rep("OTHER AE",4)),
        AEACN = c("DRUG WITHDRAWN", rep("DOSE NOT CHANGED",5)),
        stringsAsFactors = FALSE
    )
    
    DS <- data.frame(
        USUBJID = c(1,1,2,3,4),
        DSSPID  = 'XXX-DISCTX-XXX',
        DSCAT   = "DISPOSITION EVENT",
        DSDECOD = "REASON",
        stringsAsFactors = FALSE
    )
    
    
    expect_false(check_ae_aeacn_ds_disctx_covid(AE, DS, covid_df))
})


test_that("Function returns true when no errors are present", {
    
    covid_df = data.frame(REFTERM = c("COVID-19",
                                      "CORONAVIRUS POSITIVE")
    )
    
    AE <- data.frame(
        STUDYID = 1,
        USUBJID = c(1,2,3,1,2,3),
        AESTDTC = '2020-05-05',
        AETERM  = c("abc Covid-19", "covid TEST POSITIVE",rep("other AE",4)),
        AEDECOD = c("COVID-19", "CORONAVIRUS POSITIVE", rep("OTHER AE",4)),
        AEACN = c("DRUG WITHDRAWN", rep("DOSE NOT CHANGED",5)),
        stringsAsFactors = FALSE
    )
    
    DS <- data.frame(
        USUBJID = c(1,1,2,3,4),
        DSSPID  = 'XXX-DISCTX-XXX',
        DSCAT   = "DISPOSITION EVENT",
        DSDECOD = "REASON",
        stringsAsFactors = FALSE
    )
    
    DS[1, "DSDECOD"] <- 'ADVERSE EVENT'
    
    expect_true(check_ae_aeacn_ds_disctx_covid(AE, DS, covid_df))
})



test_that("function returns false when covid terms are not supplied", {
    
    AE <- data.frame(
        STUDYID = 1,
        USUBJID = c(1,2,3,1,2,3),
        AESTDTC = '2020-05-05',
        AETERM  = c("abc Covid-19", "covid TEST POSITIVE",rep("other AE",4)),
        AEDECOD = c("COVID-19", "CORONAVIRUS POSITIVE", rep("OTHER AE",4)),
        AEACN = c("DRUG WITHDRAWN", rep("DOSE NOT CHANGED",5)),
        stringsAsFactors = FALSE
    )
    
    DS <- data.frame(
        USUBJID = c(1,1,2,3,4),
        DSSPID  = 'XXX-DISCTX-XXX',
        DSCAT   = "DISPOSITION EVENT",
        DSDECOD = "REASON",
        stringsAsFactors = FALSE
    )
    
    expect_false(check_ae_aeacn_ds_disctx_covid(AE,DS))
    
})














