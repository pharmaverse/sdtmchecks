test_that("function errors when given bad input", {
    expect_error(check_dm_ae_ds_death(list()))

    # AE <- data.frame(
    #  STUDYID = 1,
    #  USUBJID = 1:4,
    #  AEDTHDTC = c(NA,1,NA),
    #  AESDTH = c(NA,"Y",NA),
    #  AEOUT = c(NA,"FATAL",NA),
    #  AETOXGR = c(NA,"5",NA)
    # )
    #
    # DS <- data.frame(
    #  STUDYID = 1,
    #  USUBJID = 1:4,
    #  DSDECOD = c(NA,"DEATH",NA),
    #  DSSTDTC = c(NA,"DSDATE",NA)
    # )
    #
    #  DM <- data.frame(
    #  STUDYID = 1,
    #  USUBJID = 1:4,
    #  DTHFL=c(NA,"Y","Y"),
    #  DTHDTC = c(NA,"MYDATE","MYDATE")
    #  )

    expect_error(check_dm_ae_ds_death(data.frame(
        STUDYID = 1,
        USUBJID = 1:4,
        AEDTHDTC = c(NA,1,NA),
        AESDTH = c(NA,"Y",NA),
        AEOUT = c(NA,"FATAL",NA),
        AETOXGR = c(NA,"5",NA)
    ), data.frame(
        STUDYID = 1,
        USUBJID = 1:4,
        DSDECOD = c(NA,"DEATH",NA),
        DSSTDTC = c(NA,"DSDATE",NA)
    ), data.frame(
        STUDYID = 1,
        USUBJID = 1:4,
        DTHFL=c(NA,"Y","Y"),
        DTHDTC = c(NA,"MYDATE","MYDATE")
    )))

})

test_that("function returns true when no errors are present", {

    AE <- data.frame(
     STUDYID = 1,
     USUBJID = 1:3,
     AEDTHDTC = c(NA,1,1),
     AESDTH = c(NA,"Y","Y"),
     AEOUT = c(NA,"FATAL","FATAL"),
     AETOXGR = c(NA,"5","5")
    )

    DS <- data.frame(
     STUDYID = 1,
     USUBJID = 1:3,
     DSDECOD = c(NA,"DEATH","DEATH"),
     DSSTDTC = c(NA,"DSDATE","DSDATE")
    )

     DM <- data.frame(
     STUDYID = 1,
     USUBJID = 1:3,
     DTHFL=c(NA,"Y","Y"),
     DTHDTC = c(NA,"MYDATE","MYDATE")
     )

    expect_true(check_dm_ae_ds_death(DM, DS, AE))

})

test_that("function returns false when errors are present", {

    AE <- data.frame(
     STUDYID = 1,
     USUBJID = 1:3,
     AEDTHDTC = c(NA,1,NA),
     AESDTH = c(NA,"Y",NA),
     AEOUT = c(NA,"FATAL",NA),
     AETOXGR = c(NA,"5",NA)
    )

    DS <- data.frame(
     STUDYID = 1,
     USUBJID = 1:3,
     DSDECOD = c(NA,"DEATH",NA),
     DSSTDTC = c(NA,"DSDATE",NA)
    )

     DM <- data.frame(
     STUDYID = 1,
     USUBJID = 1:3,
     DTHFL=c(NA,"Y","Y"),
     DTHDTC = c(NA,"MYDATE","MYDATE")
     )

    expect_false(check_dm_ae_ds_death(DM, DS, AE))


})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    AE <- data.frame(
     STUDYID = integer(),
     USUBJID = integer(),
     AEDTHDTC = character(),
     AESDTH = character(),
     AEOUT = character(),
     AETOXGR = character()
    )

    DS <- data.frame(
     STUDYID = integer(),
     USUBJID = integer(),
     DSDECOD = character(),
     DSSTDTC = character()
    )

     DM <- data.frame(
     STUDYID = integer(),
     USUBJID = integer(),
     DTHFL=character(),
     DTHDTC = character()
     )

    expect_true(check_dm_ae_ds_death(DM, DS, AE))
})

# test_that("Function returns false when errors are present for an empty dataframe (zero rows)", {
# 
#     DM <- data.frame(
#         STUDYID = 1,
#         USUBJID = 1:3,
#         DTHFL=c(NA,"Y","Y"),
#         DTHDTC = c(NA,"MYDATE","MYDATE")
#     )
#     
#     AE <- data.frame(USUBJID =NA,
#                      AESEQ=NA,
#                      AESTDTC=NA, 
#                      AEDECOD ="NA",
#                      AEDTHDTC = NA,
#                      stringsAsFactors=FALSE)
# 
#     expect_false(check_dm_ae_ds_death(DM,DS,AE))
# })

test_that("Function returns true when no errors are present for a single input (one row)", {

    AE <- data.frame(
        STUDYID = 1,
        USUBJID = 1,
        AEDTHDTC = c(1),
        AESDTH = c("Y"),
        AEOUT = c("FATAL"),
        AETOXGR = c("5")
    )

    DS <- data.frame(
        STUDYID = 1,
        USUBJID = 1,
        DSDECOD = c("DEATH"),
        DSSTDTC = c("DSDATE")
    )

    DM <- data.frame(
        STUDYID = 1,
        USUBJID = 1,
        DTHFL=c("Y"),
        DTHDTC = c("MYDATE")
    )

    expect_true(check_dm_ae_ds_death(DM, DS, AE))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    AE <- data.frame(
        STUDYID = 1,
        USUBJID = 1,
        AEDTHDTC = c(NA),
        AESDTH = c(NA),
        AEOUT = c(NA),
        AETOXGR = c(NA)
    )

    DS <- data.frame(
        STUDYID = 1,
        USUBJID = 1,
        DSDECOD = c(NA),
        DSSTDTC = c(NA)
    )

    DM <- data.frame(
        STUDYID = 1,
        USUBJID = 1,
        DTHFL=c("Y"),
        DTHDTC = c("MYDATE")
    )

    expect_false(check_dm_ae_ds_death(DM, DS, AE))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     STUDYID = 1,
                     AEDTHDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
                                         "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
                     AESDTH = "Y",
                     AEOUT = "FATAL",
                     AETOXGR ="5",
                     stringsAsFactors = FALSE)

    DS <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     STUDYID = 1,
                     DSDECOD = "DEATH",
                     DSSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     stringsAsFactors = FALSE)

    DM <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     STUDYID = 1,
                     DTHFL = "Y",
                     DTHDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     stringAsFactors = FALSE)

    expect_true(check_dm_ae_ds_death(DM, DS, AE))
})

test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     STUDYID = 1,
                     AEDTHDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
                                         "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
                     AESDTH = "Y",
                     AEOUT = "FATAL",
                     AETOXGR ="5",
                     stringsAsFactors = FALSE)

    DS <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     STUDYID = 1,
                     DSDECOD = "DEATH",
                     DSSTDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     stringsAsFactors = FALSE)

    DM <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     STUDYID = 1,
                     DTHFL = "Y",
                     DTHDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
                     stringAsFactors = FALSE)

    AE$AEDTHDTC[1] <- NA
    AE$AEDECOD[1] <- NA
    AE$AESDTH[1] <- NA
    AE$AEOUT[1] <- NA
    AE$AETOXGR[1] <- NA

    DS$DSDECOD[1] <- NA
    DS$DSSTDTC[1] <- NA

    expect_false(check_dm_ae_ds_death(DM, DS, AE))
})

test_that("Function returns the failed object in attr(data)", {

    AE <- data.frame(
        STUDYID = 1,
        USUBJID = 1:3,
        AEDTHDTC = c(NA,1,NA),
        AESDTH = c(NA,"Y",NA),
        AEOUT = c(NA,"FATAL",NA),
        AETOXGR = c(NA,"5",NA)
    )

    DS <- data.frame(
        STUDYID = 1,
        USUBJID = 1:3,
        DSDECOD = c(NA,"DEATH",NA),
        DSSTDTC = c(NA,"DSDATE",NA)
    )

    DM <- data.frame(
        STUDYID = 1,
        USUBJID = 1:3,
        DTHFL=c(NA,"Y","Y"),
        DTHDTC = c(NA,"MYDATE","MYDATE")
    )

    check <- check_dm_ae_ds_death(DM, DS, AE)

    expect_true(!is.null(attr(check, "data")))
    # expect_equal(attr(check, "data"), filter(DM, !is.na(DTHFL) | !is.na(DTHDTC)))

})


