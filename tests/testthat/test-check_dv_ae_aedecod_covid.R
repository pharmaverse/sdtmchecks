test_that("function returns true when no errors are present", {
    
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

    expect_true(check_dv_ae_aedecod_covid(AE,DV))

})

test_that("function returns false when errors are present", {
    
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
    
    expect_false(check_dv_ae_aedecod_covid(AE,DV))
})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {
    
    AE <- data.frame(
        USUBJID = character(),
        AEDECOD = character()
    )
    
    DV <- data.frame(
        USUBJID = character(),
        DVREAS = character()
    )
        
    expect_true(check_dv_ae_aedecod_covid(AE,DV))
})


test_that("Function returns true when no errors are present for a single input (one row)", {
    
    AE <- data.frame(
        USUBJID = 1,
        AEDECOD = c("covid-19")
    )
    
    DV <- data.frame(
        USUBJID = 1,
        DVREAS=c("SUSPECTED EPIDEMIC/PANDEMIC INFECTION")
    )
    
    expect_true(check_dv_ae_aedecod_covid(AE,DV))
    
})

test_that("Function returns false when errors are present for a single input (one row)", {

    AE <- data.frame(
        USUBJID = 1,
        AEDECOD = c("covid-19")
    )
    
    DV <- data.frame(
        USUBJID = 1,
        DVREAS=c("UNKNOWN")
    )
    
    expect_true(check_dv_ae_aedecod_covid(AE, DV))
})


test_that("function returns false when covid terms are NULL", {
    
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
    
    expect_false(check_dv_ae_aedecod_covid(AE,DV,covid_terms=NULL))
    
})




test_that("function returns false when covid terms are not a vector", {
    
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
    
    expect_false(check_dv_ae_aedecod_covid(AE,DV,covid_terms=data.frame(x="covid")))
    
})



test_that("function returns false when covid terms are a vector of empty terms", {
    
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
    
    expect_false(check_dv_ae_aedecod_covid(AE,DV,covid_terms=c(NA,NA,NA)))
    
})
