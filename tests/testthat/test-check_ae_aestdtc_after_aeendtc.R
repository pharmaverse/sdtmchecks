test_that("function errors when given bad input", {
    #expect_error(check_ae_aestdtc_after_aeendtc(list()))

    # AE <- data.frame(
    #     USUBJID = 1:13,
    #     AETERM = "SOME AE TERM",
    #     AEDECOD = "SOME AE PT",
    #     AESTDTC = c("2017-01-01","2017-01-03","2017-01-01T14:26","2017","2017-02","2017"      ,""    ,
    #                 "2017"   ,"2017-01-01T14:26","2017-01-01T14:26","2017-01-01T14","2017-01-01T14:26:02")
    #     ,
    #     AEENDTC = c("2017-01-01","2017-01-02","2017-01-01T14:25","2015","2017-01","2016-01-01",
    #                 "2000","2017-02","2017-01-01"      ,"2017-01","2017-01-01T13","2017-01-01T14:26:01")
    #     ,
    #     stringsAsFactors=FALSE
    # )

    # AE$AETERM <- NULL

    expect_error(check_ae_aestdtc_after_aeendtc(data.frame(
        USUBJID = 1:13,
        AETERM = "SOME AE TERM",
        AEDECOD = "SOME AE PT",
        AESTDTC = c("2017-01-01","2017-01-03","2017-01-01T14:26","2017","2017-02","2017"      ,""    ,
                    "2017"   ,"2017-01-01T14:26","2017-01-01T14:26","2017-01-01T14","2017-01-01T14:26:02")
        ,
        AEENDTC = c("2017-01-01","2017-01-02","2017-01-01T14:25","2015","2017-01","2016-01-01",
                    "2000","2017-02","2017-01-01"      ,"2017-01","2017-01-01T13","2017-01-01T14:26:01")
        ,
        stringsAsFactors=FALSE
    )))

})

test_that("function returns true when no errors are present", {

    AE <- data.frame(
        USUBJID = 1:12,
        AETERM = "SOME AE TERM",
        AEDECOD = "SOME AE PT",
        AESTDTC = c("2017-01-01","2017-01-03","2017-01-01T14:26","2017","2017-02","2017"      ,""    ,
                    "2017"   ,"2017-01-01T14:26","2017-01-01T14:26","2017-01-01T14","2017-01-01T14:26:02")
        ,
        AEENDTC = c("2017-01-01","2017-01-04","2017-01-01T14:27","2018","2017-03","2018-01-01",
                    "2000","2017-02","2017-01-01"      ,"2017-01","2017-01-01T15","2017-01-01T14:26:01")
        ,
        stringsAsFactors=FALSE
    )

    # AE$AETERM <- NULL

    expect_true(check_ae_aestdtc_after_aeendtc(AE))

})

test_that("function returns false when errors are present", {

    AE <- data.frame(
        USUBJID = 1:12,
        AETERM = "SOME AE TERM",
        AEDECOD = "SOME AE PT",
        AESTDTC = c("2017-01-01","2017-01-03","2017-01-01T14:26","2017","2017-02","2017"      ,""    ,
                    "2017"   ,"2017-01-01T14:26","2017-01-01T14:26","2017-01-01T14","2017-01-01T14:26:02")
        ,
        AEENDTC = c("2017-01-01","2017-01-02","2017-01-01T14:25","2015","2017-01","2016-01-01",
                    "2000","2017-02","2017-01-01"      ,"2017-01","2017-01-01T13","2017-01-01T14:26:01")
        ,
        stringsAsFactors=FALSE
    )

    AE$AETERM <- NULL

    expect_false(check_ae_aestdtc_after_aeendtc(AE))

})

# test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {
# 
#     AE <- data.frame(
#         USUBJID = integer(),
#         AETERM = character(),
#         AEDECOD = character(),
#         AESTDTC = character(),
#         AEENDTC = character(),
#         stringsAsFactors=FALSE
#     )
# 
#     expect_true(check_ae_aestdtc_after_aeendtc(AE))
# 
# })

test_that("Function returns true when no errors are present for a single input (one row)", {

    AE <- data.frame(
        USUBJID = 1,
        AETERM = "SOME AE TERM",
        AEDECOD = "SOME AE PT",
        AESTDTC = c("2017-01-01"),
        AEENDTC = c("2017-01-01"),
        stringsAsFactors=FALSE
    )

    expect_true(check_ae_aestdtc_after_aeendtc(AE))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    AE <- data.frame(
        USUBJID = 1,
        AETERM = "SOME AE TERM",
        AEDECOD = "SOME AE PT",
        AESTDTC = c("2017-01-02"),
        AEENDTC = c("2017-01-01"),
        stringsAsFactors=FALSE
    )

    expect_false(check_ae_aestdtc_after_aeendtc(AE))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     AETERM = rep(x = c("UPPER RESPIRATORY INFECTION","HEADACHE", "WORSENING THROMBOCYTOPENIA","LOW POTASSIUM","ANEMIA", "TREMORS"), times = 150),
                     AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
                                         "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
                     AESTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     AEENDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     stringsAsFactors = FALSE)


    expect_true(check_ae_aestdtc_after_aeendtc(AE))

})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     AETERM = rep(x = c("UPPER RESPIRATORY INFECTION","HEADACHE", "WORSENING THROMBOCYTOPENIA","LOW POTASSIUM","ANEMIA", "TREMORS"), times = 150),
                     AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
                                         "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
                     AESTDTC = as.character(rep(seq(as.Date('2013-01-01'), as.Date('2013-03-01'), by = 1), times = 15)),
                     AEENDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     stringsAsFactors = FALSE)

    expect_false(check_ae_aestdtc_after_aeendtc(AE))

})

test_that("Function returns the failed object in attr(data)", {

    AE <- data.frame(
        USUBJID = 1:12,
        AETERM = "SOME AE TERM",
        AEDECOD = "SOME AE PT",
        AESTDTC = c("2017-01-01","2017-01-03","2017-01-01T14:26","2017","2017-02","2017"      ,""    ,
                    "2017"   ,"2017-01-01T14:26","2017-01-01T14:26","2017-01-01T14","2017-01-01T14:26:02")
        ,
        AEENDTC = c("2017-01-01","2017-01-02","2017-01-01T14:25","2015","2017-01","2016-01-01",
                    "2000","2017-02","2017-01-01"      ,"2017-01","2017-01-01T13","2017-01-01T14:26:01")
        ,
        stringsAsFactors=FALSE
    ) 

    # AE$AETERM <- NULL

    check <- check_ae_aestdtc_after_aeendtc(AE)

    expect_true(!is.null(attr(check, "data")))
    #expect_equal(attr(check, "data"), filter(AE, as.Date(AESTDTC) >= as.Date(AEENDTC)))

})
