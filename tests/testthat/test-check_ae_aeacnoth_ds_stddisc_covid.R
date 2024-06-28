
test_that("Function returns false when errors are present", {
    
    AE <- data.frame(
        USUBJID = 1:5,
        AEDECOD = c("covid-19", "covid-19", "covid-19","Some AE", "CORONAVIRUS POSITIVE" ),
        AEACNOTH=c("SUBJECT DISCONTINUED FROM STUDY",
                   "NONE",
                   "NONE", 
                   "SUBJECT DISCONTINUED FROM STUDY",
                   "NONE"),
        AEACNOTH1=c("SUBJECT DISCONTINUED FROM STUDY", 
                    "NONE",
                    "SUBJECT DISCONTINUED FROM STUDY",
                    "NONE", 
                    "SUBJECT DISCONTINUED FROM STUDY"),
        AEACNOTH2=c("SUBJECT DISCONTINUED FROM STUDY", 
                    "NONE",
                    "NONE",
                    "SUBJECT DISCONTINUED FROM STUDY",
                    "NONE")
    )
    
    DS <- data.frame(
        USUBJID = 1:3,
        DSSCAT=c("TREATMENT DISCONTINUATION","STUDY DISCONTINUATION","STUDY DISCONTINUATION"),
        DSDECOD="DISCON REASON"
    )
    
    expect_false(check_ae_aeacnoth_ds_stddisc_covid(AE,DS))
})


test_that("Function returns true when no errors are present", {
    
    AE <- data.frame(
        USUBJID = 1:5,
        AEDECOD = c("covid-19", "covid-19", "covid-19","Some AE", "CORONAVIRUS POSITIVE" ),
        AEACNOTH=c("SUBJECT DISCONTINUED FROM STUDY",
                   "NONE",
                   "NONE", 
                   "SUBJECT DISCONTINUED FROM STUDY",
                   "NONE"),
        AEACNOTH1=c("SUBJECT DISCONTINUED FROM STUDY", 
                    "NONE",
                    "SUBJECT DISCONTINUED FROM STUDY",
                    "NONE", 
                    "SUBJECT DISCONTINUED FROM STUDY"),
        AEACNOTH2=c("SUBJECT DISCONTINUED FROM STUDY", 
                    "NONE",
                    "NONE",
                    "SUBJECT DISCONTINUED FROM STUDY",
                    "NONE")
    )
    
    DS <- data.frame(
        USUBJID = 1:5,
        DSSCAT=c("STUDY DISCONTINUATION","STUDY DISCONTINUATION","STUDY DISCONTINUATION", "STUDY DISCONTINUATION", "STUDY DISCONTINUATION"),
        DSDECOD="DISCON REASON"
    )
    
    expect_true(check_ae_aeacnoth_ds_stddisc_covid(AE,DS))
})


test_that("function returns false when covid terms are set to NULL", {
    
    AE <- data.frame(
        USUBJID = 1:5,
        AEDECOD = c("covid-19", "covid-19", "covid-19","Some AE", "CORONAVIRUS POSITIVE" ),
        AEACNOTH=c("SUBJECT DISCONTINUED FROM STUDY",
                   "NONE",
                   "NONE", 
                   "SUBJECT DISCONTINUED FROM STUDY",
                   "NONE"),
        AEACNOTH1=c("SUBJECT DISCONTINUED FROM STUDY", 
                    "NONE",
                    "SUBJECT DISCONTINUED FROM STUDY",
                    "NONE", 
                    "SUBJECT DISCONTINUED FROM STUDY"),
        AEACNOTH2=c("SUBJECT DISCONTINUED FROM STUDY", 
                    "NONE",
                    "NONE",
                    "SUBJECT DISCONTINUED FROM STUDY",
                    "NONE")
    )
    
    DS <- data.frame(
        USUBJID = 1:5,
        DSSCAT=c("STUDY DISCONTINUATION","STUDY DISCONTINUATION","STUDY DISCONTINUATION", "STUDY DISCONTINUATION", "STUDY DISCONTINUATION"),
        DSDECOD="DISCON REASON"
    )  
    
    expect_false(check_ae_aeacnoth_ds_stddisc_covid(AE,DS,covid_terms=NULL))
    
})




test_that("function returns false when covid terms are not a vector", {
    
    AE <- data.frame(
        USUBJID = 1:5,
        AEDECOD = c("covid-19", "covid-19", "covid-19","Some AE", "CORONAVIRUS POSITIVE" ),
        AEACNOTH=c("SUBJECT DISCONTINUED FROM STUDY",
                   "NONE",
                   "NONE", 
                   "SUBJECT DISCONTINUED FROM STUDY",
                   "NONE"),
        AEACNOTH1=c("SUBJECT DISCONTINUED FROM STUDY", 
                    "NONE",
                    "SUBJECT DISCONTINUED FROM STUDY",
                    "NONE", 
                    "SUBJECT DISCONTINUED FROM STUDY"),
        AEACNOTH2=c("SUBJECT DISCONTINUED FROM STUDY", 
                    "NONE",
                    "NONE",
                    "SUBJECT DISCONTINUED FROM STUDY",
                    "NONE")
    )
    
    DS <- data.frame(
        USUBJID = 1:5,
        DSSCAT=c("STUDY DISCONTINUATION","STUDY DISCONTINUATION","STUDY DISCONTINUATION", "STUDY DISCONTINUATION", "STUDY DISCONTINUATION"),
        DSDECOD="DISCON REASON"
    )  
    
    expect_false(check_ae_aeacnoth_ds_stddisc_covid(AE,DS,covid_terms=data.frame(x="covid")))
    
})





test_that("function returns false when covid terms are a vector of empty terms", {
    
    AE <- data.frame(
        USUBJID = 1:5,
        AEDECOD = c("covid-19", "covid-19", "covid-19","Some AE", "CORONAVIRUS POSITIVE" ),
        AEACNOTH=c("SUBJECT DISCONTINUED FROM STUDY",
                   "NONE",
                   "NONE", 
                   "SUBJECT DISCONTINUED FROM STUDY",
                   "NONE"),
        AEACNOTH1=c("SUBJECT DISCONTINUED FROM STUDY", 
                    "NONE",
                    "SUBJECT DISCONTINUED FROM STUDY",
                    "NONE", 
                    "SUBJECT DISCONTINUED FROM STUDY"),
        AEACNOTH2=c("SUBJECT DISCONTINUED FROM STUDY", 
                    "NONE",
                    "NONE",
                    "SUBJECT DISCONTINUED FROM STUDY",
                    "NONE")
    )
    
    DS <- data.frame(
        USUBJID = 1:5,
        DSSCAT=c("STUDY DISCONTINUATION","STUDY DISCONTINUATION","STUDY DISCONTINUATION", "STUDY DISCONTINUATION", "STUDY DISCONTINUATION"),
        DSDECOD="DISCON REASON"
    )  
    
    expect_false(check_ae_aeacnoth_ds_stddisc_covid(AE,DS,covid_terms=c("","",NA)))
    
})






