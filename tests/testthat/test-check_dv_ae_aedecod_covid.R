test_that("function returns true when no errors are present", {
    
    covid_df = data.frame(REFTERM = c("COVID-19",
                                      "CORONAVIRUS POSITIVE")
    )
    
    AE <- data.frame(
        USUBJID = 1:6,
        AEDECOD = c("covid-19", "covid-19","some AE","some AE","CORONAVIRUS POSITIVE","UNMAPPED")
    )
    
    DV <- data.frame(
        USUBJID = 1:6,
        DVREAS=c("SUSPECTED EPIDEMIC/PANDEMIC INFECTION",
                 "SUSPECTED EPIDEMIC/PANDEMIC INFECTION",
                 NA,
                 NA,
                 "SUSPECTED EPIDEMIC/PANDEMIC INFECTION",
                 NA)
    )    

    expect_true(check_dv_ae_aedecod_covid(AE,DV,covid_df))

})

test_that("function returns false when errors are present", {

    
    covid_df = data.frame(REFTERM = c("COVID-19",
                                      "CORONAVIRUS POSITIVE")
    )
    
    AE <- data.frame(
        USUBJID = 1:6,
        AEDECOD = c("covid-19", "covid-19","some AE","some AE","CORONAVIRUS POSITIVE","UNMAPPED")
    )
    
    DV <- data.frame(
        USUBJID = 1:6,
        DVREAS=c("SUSPECTED EPIDEMIC/PANDEMIC INFECTION",
                 "UNKNOWN",
                 "SUSPECTED EPIDEMIC/PANDEMIC INFECTION",
                 "OTHER",
                 "SUSPECTED EPIDEMIC/PANDEMIC INFECTION",
                 "SUSPECTED EPIDEMIC/PANDEMIC INFECTION")
    )
    
    expect_false(check_dv_ae_aedecod_covid(AE,DV,covid_df))
})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    covid_df = data.frame(REFTERM = character()
    )
    
    AE <- data.frame(
        USUBJID = character(),
        AEDECOD = character()
    )
    
    DV <- data.frame(
        USUBJID = character(),
        DVREAS = character()
    )
        
    expect_true(check_dv_ae_aedecod_covid(AE,DV,covid_df))
})


test_that("Function returns true when no errors are present for a single input (one row)", {

    covid_df = data.frame(REFTERM = c("COVID-19",
                                      "CORONAVIRUS POSITIVE")
    )
    
    AE <- data.frame(
        USUBJID = 1,
        AEDECOD = c("covid-19")
    )
    
    DV <- data.frame(
        USUBJID = 1,
        DVREAS=c("SUSPECTED EPIDEMIC/PANDEMIC INFECTION")
    )
    
    expect_true(check_dv_ae_aedecod_covid(AE,DV,covid_df))
    
})

test_that("Function returns false when errors are present for a single input (one row)", {

    
    covid_df = data.frame(REFTERM = c("COVID-19",
                                      "CORONAVIRUS POSITIVE")
    )
    
    AE <- data.frame(
        USUBJID = 1,
        AEDECOD = c("covid-19")
    )
    
    DV <- data.frame(
        USUBJID = 1,
        DVREAS=c("UNKNOWN")
    )
    
    expect_true(check_dv_ae_aedecod_covid(AE, DV, covid_df))
})


test_that("function returns false when covid terms are not supplied", {
    
    AE <- data.frame(
        USUBJID = 1:6,
        AEDECOD = c("covid-19", "covid-19","some AE","some AE","CORONAVIRUS POSITIVE","UNMAPPED")
    )
    
    DV <- data.frame(
        USUBJID = 1:6,
        DVREAS=c("SUSPECTED EPIDEMIC/PANDEMIC INFECTION",
                 "SUSPECTED EPIDEMIC/PANDEMIC INFECTION",
                 NA,
                 NA,
                 "SUSPECTED EPIDEMIC/PANDEMIC INFECTION",
                 NA)
    )    
    
    expect_false(check_dv_ae_aedecod_covid(AE,DV))
    
})
