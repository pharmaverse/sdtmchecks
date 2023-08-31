test_that("Returns true when no errors present - 1", {
  
  DS <- data.frame(
    STUDYID = 1,
    USUBJID = 1:3,
    DSDECOD = c(NA,"DEATH",NA),
    DSSTDTC = c(NA,"DSDATE",NA),
    DSCAT = c('DISPOSITION EVENT', 'DISPOSITION EVENT', 'OTHER'),
    DSSCAT = c('STUDY COMPLETION/EARLY DISCONTINUATION',
               'TREATMENT DISCONTINUATION',
               'STUDY TREATMENT'),
    DSOTH = 1:3,
    DSSPID = "XXX-R:0",
    stringsAsFactors=FALSE
  )
  DS$DSSCAT[2] <- "STUDY COMPLETION/EARLY DISCONTINUATION"

  expect_true(check_ds_dsdecod_death(DS))
  
})

test_that("Returns true when no errors present - 2", {
  DS <- data.frame(
    STUDYID = 1,
    USUBJID = 1:3,
    DSDECOD = c(NA,"DEATH",NA),
    DSSTDTC = c(NA,"DSDATE",NA),
    DSCAT = c('DISPOSITION EVENT', 'DISPOSITION EVENT', 'OTHER'),
    DSSCAT = c('STUDY COMPLETION/EARLY DISCONTINUATION',
               'TREATMENT DISCONTINUATION',
               'STUDY TREATMENT'),
    DSOTH = 1:3,
    DSSPID = "XXX-R:0",
    stringsAsFactors=FALSE
  )
  
  DS$DSSCAT[2] <- "STUDY COMPLETION/EARLY DISCONTINUATION"
  expect_true(check_ds_dsdecod_death(DS,preproc=roche_derive_rave_row))
})

test_that("Returns false when errors present", {
  DS <- data.frame(
    STUDYID = 1,
    USUBJID = 1:3,
    DSDECOD = c(NA,"DEATH",NA),
    DSSTDTC = c(NA,"DSDATE",NA),
    DSCAT = c('DISPOSITION EVENT', 'DISPOSITION EVENT', 'OTHER'),
    DSSCAT = c('STUDY COMPLETION/EARLY DISCONTINUATION',
               'TREATMENT DISCONTINUATION',
               'STUDY TREATMENT'),
    DSOTH = 1:3,
    DSSPID = "XXX-R:0",
    stringsAsFactors=FALSE
  )
  
  expect_false(check_ds_dsdecod_death(DS))
})
 

test_that("Returns false when expected column not present", {
  DS <- data.frame(
    STUDYID = 1,
    USUBJID = 1:3,
    DSDECOD = c(NA,"DEATH",NA),
    DSSTDTC = c(NA,"DSDATE",NA),
    DSCAT = c('DISPOSITION EVENT', 'DISPOSITION EVENT', 'OTHER'),
    DSSCAT = c('STUDY COMPLETION/EARLY DISCONTINUATION',
               'TREATMENT DISCONTINUATION',
               'STUDY TREATMENT'),
    DSOTH = 1:3,
    DSSPID = "XXX-R:0",
    stringsAsFactors=FALSE
  )
  
  DS$DSDECOD = NULL
  expect_false(check_ds_dsdecod_death(DS))
})



