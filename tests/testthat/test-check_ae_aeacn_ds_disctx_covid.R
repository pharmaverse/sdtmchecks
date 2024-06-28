

test_that("Function returns false when errors are present", {
    
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
    
    
    expect_false(check_ae_aeacn_ds_disctx_covid(AE, DS))
})


test_that("Function returns true when no errors are present", {
    
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
    
    expect_true(check_ae_aeacn_ds_disctx_covid(AE, DS))
})



test_that("function returns false when covid terms set to NULL", {
    
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
    
    expect_false(check_ae_aeacn_ds_disctx_covid(AE,DS,covid_terms=NULL))
    
})


test_that("function returns false when covid terms not a vector", {
    
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
    
    expect_false(check_ae_aeacn_ds_disctx_covid(AE,DS,covid_terms=data.frame(x="COVID")))
    
})


test_that("function returns false when covid terms are a vector of empty terms", {
    
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
    
    expect_false(check_ae_aeacn_ds_disctx_covid(AE,DS,covid_terms=""))
    
})

test_that("Function returns false when key columns set to null", {
    
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
        #DSSPID  = 'XXX-DISCTX-XXX',
        DSCAT   = "DISPOSITION EVENT",
        DSDECOD = "REASON",
        stringsAsFactors = FALSE
    )
    
    expect_false(check_ae_aeacn_ds_disctx_covid(AE, DS))
})





test_that("Function returns false when key columns set to null", {
    
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
        #DSSPID  = 'XXX-DISCTX-XXX',
        DSCAT   = "DISPOSITION EVENT",
        DSDECOD = "REASON",
        stringsAsFactors = FALSE
    )
    
    expect_false(check_ae_aeacn_ds_disctx_covid(AE, DS))
})






test_that("Function returns false when key columns set to null", {
    
    AE <- data.frame(
        STUDYID = 1,
        USUBJID = c(1,2,3,1,2,3),
        AESTDTC = '2020-05-05',
        AETERM  = c("abc Covid-19", "covid TEST POSITIVE",rep("other AE",4)),
        AEDECOD = c("COVID-19", "CORONAVIRUS POSITIVE", rep("OTHER AE",4)),
        #AEACN = c("DRUG WITHDRAWN", rep("DOSE NOT CHANGED",5)),
        stringsAsFactors = FALSE
    )
    
    DS <- data.frame(
        USUBJID = c(1,1,2,3,4),
        DSSPID  = 'XXX-DISCTX-XXX',
        DSCAT   = "DISPOSITION EVENT",
        DSDECOD = "REASON",
        stringsAsFactors = FALSE
    )
    
    expect_false(check_ae_aeacn_ds_disctx_covid(AE, DS))
})








