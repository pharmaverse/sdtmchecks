
test_that("Function returns false when errors are present", {
    
    covid_df = data.frame(REFTERM = c("COVID-19",
                                      "CORONAVIRUS POSITIVE"
    )
    )
    
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
    
    expect_false(check_ae_aeacnoth_ds_stddisc_covid(AE,DS,covid_df = covid_df))
})


test_that("Function returns true when no errors are present", {
    
    covid_df = data.frame(REFTERM = c("COVID-19",
                                      "CORONAVIRUS POSITIVE"
    )
    )
    
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
    
    expect_true(check_ae_aeacnoth_ds_stddisc_covid(AE,DS,covid_df = covid_df))
})






