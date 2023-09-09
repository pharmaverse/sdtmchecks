test_that("function errors when given bad input", {
    #expect_error(check_ae_aetoxgr(list()))
    expect_error(check_ae_aetoxgr(data.frame(USUBJID = 1:4,
                                                         AESEQ = 1:3,
                                                         AESTDTC = 1:3,
                                                         AEDECOD = 1:3,
                                                         AETERM = 1:3,
                                                         AETOXGR = 1:3,
                                                         AESEV = 1:3)))

})

test_that("function returns true when no errors are present", {

    AE <- data.frame(
        USUBJID = 1:3,
        AESEQ = 1:3,
        AETERM = 1:3,
        AESTDTC = 1:3,
        AEDECOD = 1:3,
        # AETOXGR = 1:3,
        AESEV = 1:3
    )

    expect_true(check_ae_aetoxgr(AE))

})

test_that("function returns false when errors are present", {

    AE <- data.frame(
        USUBJID = 1:3,
        AESEQ = 1:3,
        AESTDTC = 1:3,
        AEDECOD = 1:3,
        AETERM = 1:3,
        AETOXGR = 1:3
        #AESEV = 1:3
    )

    AE$AETOXGR[1] <- NA

    expect_false(check_ae_aetoxgr(AE = AE))


})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    AE <- data.frame(
        USUBJID = integer(),
        AESTDTC = integer(),
        AETERM = integer(),
        AEDECOD = integer(),
        AETOXGR = integer()
        #,AESEV = integer()
    )


    expect_true(check_ae_aetoxgr(AE))
})

# test_that("Function returns false when errors are present for an empty dataframe (zero rows)", {
#
#     AE <- data.frame(USUBJID =NA,
#                      AESEQ=NA,
#                      AESTDTC=NA,
#                      AETERM="",
#                      AEDECOD ="NA",
#                      stringsAsFactors=FALSE)
#
#     expect_false(check_ae_aedecod(AE))
# })

test_that("Function returns true when no errors are present for a single input (one row)", {

    AE <- data.frame(
        USUBJID = 1,
        AESEQ = 1,
        AESTDTC = 1,
        AETERM = 1,
        AEDECOD = 1,
        # AETOXGR = 1,
        AESEV = 1
    )

    expect_true(check_ae_aetoxgr(AE))
})



## this returns true -- logic has been updated of the check 
# test_that("Function returns false when errors are present for a single input (one row)", {
# 
#     AE <- data.frame(
#         USUBJID = 1,
#         AESEQ = 1,
#         AESTDTC = 1,
#         AETERM = 1,
#         AEDECOD = 1,
#         AETOXGR = 1,
#         AESEV = NA
#     )
# 
#     expect_false(check_ae_aetoxgr(AE))
# })

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     AESEQ = rep(x = seq(1, 60, by=1), times = 900),
                     # AEDTHDTC = as,character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     # AESDTH = rep("Y", times = 900),
                     AETERM = rep(x = c("UPPER RESPIRATORY INFECTION","HEADACHE",
                                         "WORSENING THROMBOCYTOPENIA","LOW POTASSIUM","ANEMIA", "TREMORS"), times = 9000),
                     AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
                                         "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
                     AESTDTC = as.character(rep(seq(as.Date('2010-01-01'), as.Date('2010-03-01'), by = 1), times = 15)),
                     AETOXGR = c(rep(x = c(1,2,3), times = c(300, 300, 300))),
                     stringsAsFactors = FALSE)

    expect_true(check_ae_aetoxgr(AE))
})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                 AESEQ = 1,
                 # AEDTHDTC = as,character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                 # AESDTH = rep("Y", times = 900),
                 AETERM = rep(x = c("UPPER RESPIRATORY INFECTION","HEADACHE",
                                     "WORSENING THROMBOCYTOPENIA","LOW POTASSIUM","ANEMIA", "TREMORS"), times = 9000),
                 AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
                                     "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
                 AESTDTC = as.character(rep(seq(as.Date('2010-01-01'), as.Date('2010-03-01'), by = 1), times = 15)),
                 #AETOXGR = c(rep(x = c(1,2,3,4,5), times = rep(180, times = 5))),
                 AESEV = c(rep(x = c("N", "Y", ""), times = c(540, 300, 60))),
                 stringsAsFactors = FALSE)

    expect_false(check_ae_aetoxgr(AE))
})


test_that("Function returns false when errors are present", {

    
    
    AE <- data.frame(
        STUDY = c(rep("STUDYABC", 6)),
        DOMAIN = c(rep("AE", 6)),
        USUBJID = c(rep("STUDYABC-0000000-12345",6)),
        AESEQ = c(1, 2, 3, 4, 5, 6),
        AESPID = c("AEFORM:0123456-R:1/L:1/AT:INITIALEXTREME",
                   "AEFORM:0123456-R:2/L:2/AT:INITIALEXTREME",
                   "AEFORM:0123456-R:5/L:5/AT:INITIALEXTREME",
                   "AEFORM:0123456-R:7/L:7/AT:INITIALEXTREME",
                   "AEFORM:0123456-R:8/L:8/AT:INITIALEXTREME",
                   "AEFORM:0123456-R:9/L:9/AT:INITIALEXTREME"),
        AETERM = c("ANEMIA", "ANEMIA", "ANOREXIA", "CONSTIPATION", "DIARRHEA", "FATIGUE"),
        AEDECOD = c("ANAEMIA", "ANAEMIA", "DECREASED APPETITE", "CONSTIPATION", "DIARRHOEA", NA),
        AESTDTC = c("2012-01-01", "2012-02-02", "2013-03-01", "2014-01-01", "2014-01-02", "2014-01-03"),
        AETOXGR = c("2", "2", "3", "1", "2", "1"),
        AESEV = c("N", "Y", "Y", "N", "N", "N"),
        stringsAsFactors = FALSE
    ) 


    # both variables AETOXGR and AESEV have missing values
    AE3 <- AE
    AE3$AETOXGR[4] <- NA
    AE3$AESEV[4] <- NA
    expect_false(check_ae_aetoxgr(AE3))

    # AE is missing both the AETOXGR and AESEV variable
    AE4 <- select(AE, -c("AETOXGR", "AESEV"))
    expect_false(check_ae_aetoxgr(AE4))

    # derive df with RAVE no number from AESPID with missing values in AETOXGR
    AE5 <- select(AE, -AESEV)
    AE5$AETOXGR[3] <- NA
    expect_false(check_ae_aetoxgr(AE5))

    # derive df with missing values in AESEV
    AE6 <- select(AE, -c("AESPID", "AETOXGR"))
    AE6$AESEV[5] <- NA
    expect_false(check_ae_aetoxgr(AE6))
})

# test_that("Function returns the failed object in attr(data)", {
# 
#     AE <- data.frame(
#         USUBJID = 1:3,
#         AESEQ = 1:3,
#         AESTDTC = 1:3,
#         AEDECOD = 1:3,
#         AETOXGR = 1:3,
#         AESEV = 1:3
#     )
# 
#     AE$AETOXGR[1] <- NA
# 
#     check <- check_ae_aetoxgr(AE)
# 
#     #expect_true(!is.null(attr(check, "data")))
#     expect_equal(attr(check, "data"), filter(AE, !is.na(AEDTHDTC) & AESDTH != "Y"))
# 
# })


