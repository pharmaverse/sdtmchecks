 test_that("function errors when given bad input", {
     
     #expect_error(check_ae_aedthdtc_ds_death(list()))

    AE <- data.frame(
        USUBJID = 1:4,
        AEDTHDTC = c(NA,NA,1,NA),
        stringsAsFactors=FALSE
        )

     DS <- data.frame(
      USUBJID = 1:4,
      DSTERM = c("DEATH DUE TO ADVERSE EVENT",
                 "DEATH DUE TO PROGRESSIVE DISEASE",
                 "DEATH DUE TO ADVERSE EVENT",
                 "DEATH DUE TO ADVERSE EVENT")
                 ,
      DSDECOD = rep("DEATH",4),
      DSSTDTC = 1:4,
      stringsAsFactors=FALSE)

     expect_error(check_ae_aedthdtc_ds_death(data.frame(
         USUBJID = 1:4,
         AEDTHDTC = c(NA,NA,1)
     ), data.frame(
         USUBJID = 1:5,
         DSTERM = c("DEATH DUE TO ADVERSE EVENT","DEATH DUE TO PROGRESSIVE DISEASE",
                    "DEATH DUE TO ADVERSE EVENT","DEATH DUE TO ADVERSE EVENT")
         ,
         DSDECOD = rep("DEATH",4),
         DSSTDTC = 1:4
     )))

 })

test_that("function returns true when no errors are present", {

    AE <- data.frame(
        USUBJID = 1:4,
        AEDTHDTC = 1:4
    )

    DS <- data.frame(
        USUBJID = 1:4,
        DSTERM = c("DEATH DUE TO ADVERSE EVENT","DEATH DUE TO PROGRESSIVE DISEASE",
                   "DEATH DUE TO ADVERSE EVENT","DEATH DUE TO ADVERSE EVENT")
        ,
        DSDECOD = rep("DEATH",4),
        DSSTDTC = 1:4
    )
    expect_true(check_ae_aedthdtc_ds_death(AE, DS))

})

test_that("function returns false when errors are present", {

    AE <- data.frame(
        USUBJID = 1:4,
        AEDTHDTC = c(NA,NA,NA,NA)
    )

    DS <- data.frame(
        USUBJID = 1:4,
        DSTERM = c("DEATH DUE TO ADVERSE EVENT",
                   "DEATH DUE TO PROGRESSIVE DISEASE",
                   "DEATH DUE TO ADVERSE EVENT",
                   "DEATH DUE TO ADVERSE EVENT")
        ,
        DSDECOD = rep("DEATH",4),
        DSSTDTC = 1:4
    ) 
    
    expect_false(check_ae_aedthdtc_ds_death(AE, DS))

})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    AE <- data.frame(USUBJID =integer(),
                     AEDTHDTC=integer())

    DS <- data.frame(USUBJID = integer(),
                     DSTERM = character(),
                     DSDECOD = character(),
                     DSSTDTC = integer(),
                     stringsAsFactors = FALSE)

    expect_true(check_ae_aedthdtc_ds_death(AE, DS))
})

test_that("Function returns false when errors are present for an empty dataframe (zero rows)", {

    AE <- data.frame(USUBJID=NA,
                     AESEQ=NA,
                     AESTDTC=NA,
                     AETERM="",
                     AEDECOD="NA",
                     stringsAsFactors=FALSE)
    DS <- data.frame()

    expect_false(check_ae_aedthdtc_ds_death(AE, DS))
})

test_that("Function returns true when no errors are present for a single input (one row)", {

    AE <- data.frame(
        USUBJID = 1,
        AEDTHDTC = c(1)
    )

    DS <- data.frame(
        USUBJID = 1,
        DSTERM = c("DEATH DUE TO ADVERSE EVENT")
        ,
        DSDECOD = rep("DEATH",1),
        DSSTDTC = 1
    )

    expect_true(check_ae_aedthdtc_ds_death(AE, DS))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    AE <- data.frame(
        USUBJID = 1,
        AEDTHDTC = ""
    )

    DS <- data.frame(
        USUBJID = 1,
        DSTERM = c("DEATH DUE TO ADVERSE EVENT")
        ,
        DSDECOD = rep("DEATH",1),
        DSSTDTC = 1
    )

    expect_false(check_ae_aedthdtc_ds_death(AE, DS))
})

test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     # AESEQ = rep(x = seq(1, 60, by=1), times = 900),
                     AEDTHDTC = rep(seq(as.Date('2023-01-01'), as.Date('2023-03-01'), by = 1), times = 15),
                     stringsAsFactors = FALSE)

    DS <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     DSTERM = rep(c("DEATH DUE TO ADVERSE EVENT","DEATH DUE TO PROGRESSIVE DISEASE",
                                "DEATH DUE TO ADVERSE EVENT","DEATH DUE TO ADVERSE EVENT",
                                "DEATH DUE TO ADVERSE EVENT","DEATH DUE TO PROGRESSIVE DISEASE"), times = 150),
                     DSDECOD = rep("DEATH",900),
                     DSSTDTC = rep(seq(as.Date('2022-01-01'), as.Date('2022-03-01'), by = 1), times = 15),
                     stringsAsFactors = FALSE)

    expect_true(check_ae_aedthdtc_ds_death(AE, DS))
})

test_that("Function returns true when no errors are present for multiple inputs (900 rows)", {

    USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)

    AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     # AESEQ = rep(x = seq(1, 60, by=1), times = 900),
                     AEDTHDTC = rep(seq(as.Date('2023-01-01'), as.Date('2023-03-01'), by = 1), times = 15),
                     stringsAsFactors = FALSE)

    DS <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
                     DSTERM = rep(c("DEATH DUE TO ADVERSE EVENT","DEATH DUE TO PROGRESSIVE DISEASE",
                                    "DEATH DUE TO ADVERSE EVENT","DEATH DUE TO ADVERSE EVENT",
                                    "DEATH DUE TO ADVERSE EVENT","DEATH DUE TO PROGRESSIVE DISEASE"), times = 150),
                     DSDECOD = rep("DEATH",900),
                     DSSTDTC = rep(seq(as.Date('2022-01-01'), as.Date('2022-03-01'), by = 1), times = 15),
                     stringsAsFactors = FALSE)

    expect_true(check_ae_aedthdtc_ds_death(AE, DS))
})

test_that("Function returns the failed object in attr(data)", {

    AE <- data.frame(
        USUBJID = 1,
        AEDTHDTC = ""
    )

    DS <- data.frame(
        USUBJID = 1,
        DSTERM = c("DEATH DUE TO ADVERSE EVENT")
        ,
        DSDECOD = rep("DEATH",1),
        DSSTDTC = 1
    )

    DS$DSTERM = "JKLHKJADVERSE EVENTHKHK"
    DS$DSDECOD = "DEATH"
    AE$AEDTHDTC= ""

    check <- check_ae_aedthdtc_ds_death(AE, DS)

    expect_false(!is.null(attr(check, "data")))
    # expect_equal(attr(check, "data"), filter(AE, !is.na(AEDTHDTC) & AESDTH != "Y"))

})

test_that("function returns dataframe when errors are present", {

    AE <- data.frame(
        USUBJID = 1:4,
        AEDTHDTC = c(NA,NA,1, 1)
    )

    DS <- data.frame(
        USUBJID = 1:4,
        DSTERM = c("DEATH DUE TO ADVERSE EVENT",
                   "DEATH DUE TO PROGRESSIVE DISEASE",
                   "DEATH DUE TO ADVERSE EVENT",
                   "DEATH DUE TO ADVERSE EVENT")
        ,
        DSDECOD = rep("DEATH",4),
        DSSTDTC = 1:4
    )

    DS$DSTERM = "JKLHKJADVERSE EVENTHKHK"
    DS$DSDECOD = "DEATH"
    AE$AEDTHDTC= ""

    check <- check_ae_aedthdtc_ds_death(AE, DS)

    expect_false(is.data.frame(attr(check, "data")))

})
