test_that("function errors when given bad input", {
    #expect_error(check_ae_death_ds_discon(list()))


    # AE <- data.frame(
    #     STUDYID = rep(1,7),
    #     USUBJID = 1:6,
    #     AEDTHDTC = c(NA,1,NA,NA,NA,NA),
    #     AESDTH = c(NA,NA,"Y",NA,NA,NA),
    #     AEOUT = c(NA,NA,NA,"FATAL",NA,NA),
    #     AETOXGR = c(NA,NA,NA,NA,"5",NA)
    # )
    #
    # DS <- data.frame(
    #     STUDYID = 1,
    #     USUBJID = 1:3,
    #     DSCAT="DISPOSITION EVENT",
    #     DSSCAT=c("STUDY DISCON","STUDY DISCON","STUDY COMPLETION/EARLY DISCONTINUATION", "STUDY COMPLETION/EARLY DISCONTINUATION")
    # )

    expect_error(check_ae_death_ds_discon(data.frame(
        STUDYID = rep(1,7),
        USUBJID = 1:6,
        AEDTHDTC = c(NA,1,NA,NA,NA,NA),
        AESDTH = c(NA,NA,"Y",NA,NA,NA),
        AEOUT = c(NA,NA,NA,"FATAL",NA,NA),
        AETOXGR = c(NA,NA,NA,NA,"5",NA)
    ), data.frame(
        STUDYID = 1,
        USUBJID = 1:3,
        DSCAT="DISPOSITION EVENT",
        DSSCAT=c("STUDY DISCON","STUDY DISCON","STUDY COMPLETION/EARLY DISCONTINUATION", "STUDY COMPLETION/EARLY DISCONTINUATION")
    )
    ))

})

test_that("function returns true when no errors are present", {

    AE <- data.frame(
        STUDYID = rep(1,6),
        USUBJID = 1:6,
        AEDTHDTC = c(NA,1,NA,NA,NA,NA),
        AESDTH = c(NA,NA,"Y",NA,NA,NA),
        AEOUT = c(NA,NA,NA,"FATAL",NA,NA),
        AETOXGR = c(NA,NA,NA,NA,"5",NA)
    )

    DS <- data.frame(
        STUDYID = 1,
        USUBJID = 1:6,
        DSCAT="DISPOSITION EVENT",
        DSSCAT=c("STUDY DISCON","STUDY DISCON","STUDY COMPLETION/EARLY DISCONTINUATION", "STUDY DISCON", "STUDY DISCON", "STUDY DISCON")
    )

    expect_true(check_ae_death_ds_discon(AE, DS))

})

test_that("function returns false when errors are present", {

    AE <- data.frame(
        STUDYID = rep(1,6),
        USUBJID = 1:6,
        AEDTHDTC = c(NA,1,NA,NA,NA,NA),
        AESDTH = c(NA,NA,"Y",NA,NA,NA),
        AEOUT = c(NA,NA,NA,"FATAL",NA,NA),
        AETOXGR = c(NA,NA,NA,NA,"5",NA)
    )

    DS <- data.frame(
        STUDYID = 1,
        USUBJID = 1:3,
        DSCAT="DISPOSITION EVENT",
        DSSCAT=c("STUDY DISCON","STUDY DISCON","STUDY COMPLETION/EARLY DISCONTINUATION")
    )

    expect_false(check_ae_death_ds_discon(AE, DS))


})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    AE <- data.frame(
        STUDYID = integer(),
        USUBJID = integer(),
        AEDTHDTC = integer(),
        AESDTH = character(),
        AEOUT = character(),
        AETOXGR = character(),
        stringsAsFactors = FALSE
    )

    DS <- data.frame(
        STUDYID = integer(),
        USUBJID = integer(),
        DSCAT=character(),
        DSSCAT=character(),
        stringsAsFactors = FALSE
    )

    expect_true(check_ae_death_ds_discon(AE, DS))
})

test_that("Function returns true when no errors are present for a single input (one row)", {

    AE <- data.frame(
        STUDYID = 1,
        USUBJID = 1,
        AEDTHDTC = NA,
        AESDTH = NA,
        AEOUT = NA,
        AETOXGR = NA
    )

    DS <- data.frame(
        STUDYID = 1,
        USUBJID = 1,
        DSCAT="DISPOSITION EVENT",
        DSSCAT=c("STUDY DISCON")
    )

    expect_true(check_ae_death_ds_discon(AE, DS))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    AE <- data.frame(
        STUDYID = 1,
        USUBJID = 1,
        AEDTHDTC = NA,
        AESDTH = NA,
        AEOUT = "FATAL",
        AETOXGR = "5"
    )

    DS <- data.frame(
        STUDYID = 1,
        USUBJID = 1,
        DSCAT="DISPOSITION EVENT",
        DSSCAT=c("")
    )

    expect_false(check_ae_death_ds_discon(AE, DS))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     STUDYID = 1,
                     AEDTHDTC = c(as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 8)), rep(NA, 420)),
                     AESDTH = rep(c("Y", NA), times = c(450, 450)),
                     AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
                                         "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
                     AESTDTC = as.character(rep(seq(as.Date('2010-01-01'), as.Date('2010-03-01'), by = 1), times = 15)),
                     AEOUT = rep(c("FATAL", NA), times = c(480, 420)),
                     AETOXGR = rep(c("5", NA), times = c(480, 420)),
                     stringsAsFactors = FALSE)

    DS <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     STUDYID = 1,
                     DSCAT="DISPOSITION EVENT",
                     DSSCAT=rep(c("STUDY DISCON","STUDY COMPLETION/EARLY DISCONTINUATION", ""), times = c(80, 400, 420)),
                     stringsAsFactors = FALSE
                     )

    expect_true(check_ae_death_ds_discon(AE, DS))
})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)
    USUBJID_ <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 479)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     STUDYID = 1,
                     AEDTHDTC = c(as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 8)), rep(NA, 420)),
                     AESDTH = rep(c("Y", NA), times = c(450, 450)),
                     AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
                                         "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
                     AESTDTC = as.character(rep(seq(as.Date('2010-01-01'), as.Date('2010-03-01'), by = 1), times = 15)),
                     AEOUT = rep(c("FATAL", NA), times = c(480, 420)),
                     AETOXGR = rep(c("5", NA), times = c(480, 420)),
                     stringsAsFactors = FALSE)

    DS <- data.frame(USUBJID = USUBJID_[order(nchar(USUBJID_), USUBJID_)],
                     STUDYID = 1,
                     DSCAT="DISPOSITION EVENT",
                     DSSCAT=rep(c("STUDY DISCON","STUDY COMPLETION/EARLY DISCONTINUATION"), times = c(80, 300)),
                     stringsAsFactors = FALSE
    )


    expect_false(check_ae_death_ds_discon(AE, DS))
})

test_that("Function returns the failed object in attr(data)", {

    AE <- data.frame(
        STUDYID = rep(1,6),
        USUBJID = 1:6,
        AEDTHDTC = c(NA,1,NA,NA,NA,NA),
        AESDTH = c(NA,NA,"Y",NA,NA,NA),
        AEOUT = c(NA,NA,NA,"FATAL",NA,NA),
        AETOXGR = c(NA,NA,NA,NA,"5",NA)
    )

    DS <- data.frame(
        STUDYID = 1,
        USUBJID = 1:3,
        DSCAT="DISPOSITION EVENT",
        DSSCAT=c("STUDY DISCON","STUDY DISCON","STUDY COMPLETION/EARLY DISCONTINUATION")
    )

    check <- check_ae_death_ds_discon(AE, DS)

    expect_true(!is.null(attr(check, "data")))
    # expect_equal(attr(check, "data"), filter(AE, !is.na(AEDTHDTC) & AESDTH != "Y"))

})


