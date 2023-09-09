test_that("Function returns TRUE when study discontinuation occurs after last
          treatment date",{
              
              DS <- data.frame(
                  USUBJID = c(1,2),
                  DSSCAT= rep(c("STUDY COMPLETION/EARLY DISCONTINUATION"),2),
                  DSCAT = rep(c("DISPOSITION EVENT"),2),
                  DSSTDTC = c("2019-12-26", "2019-12-30"),
                  stringsAsFactors=FALSE
              )
              
              EX <- data.frame(
                  USUBJID = c(rep(1,2),rep(2,2)),
                  EXSTDTC = c("2019-12-15", "2019-12-23", "2019-12-20","2019-12-27"),
                  EXENDTC = c("2019-12-20", "2019-12-19", "2019-12-30","2019-12-27"),
                  EXTRT = c(rep("SOME DRUG", 2), rep("PLACEBO", 2)),
                  EXDOSE=c(10,10,0,0),
                  stringsAsFactors=FALSE
              )
              
              expect_true(check_ds_ex_after_discon(DS, EX))
              
          })

test_that("Function returns FALSE when study discontinuation occurs before last
          treatment date despite missing DSSTDTC date(s)",{
              
              DS <- data.frame(
                  USUBJID = c(1,2),
                  DSSCAT= rep(c("STUDY COMPLETION/EARLY DISCONTINUATION"),2),
                  DSCAT = rep(c("DISPOSITION EVENT"),2),
                  DSSTDTC = c(NA, "2019-12-28"),
                  stringsAsFactors=FALSE
              )
              
              EX <- data.frame(
                  USUBJID = c(rep(1,2),rep(2,2)),
                  EXSTDTC = c("2018-12-20", "2019-12-28", "2019-12-26", "2019-12-27"),
                  EXENDTC = c("2019-12-30", "2019-12-30", "2019-12-30", "2019-12-27"),
                  EXTRT = c(rep("SOME DRUG", 2), rep("PLACEBO", 2)),
                  EXDOSE=c(10,10,0,0),
                  stringsAsFactors=FALSE
              )
              
              expect_false(check_ds_ex_after_discon(DS, EX))
              
          })

test_that("Function returns FALSE when study discontinuation occurs before last
          treatment date despite missing exposure date(s)",{
              
              DS <- data.frame(
                  USUBJID = c(1,2),
                  DSSCAT= rep(c("STUDY COMPLETION/EARLY DISCONTINUATION"),2),
                  DSCAT = rep(c("DISPOSITION EVENT"),2),
                  DSSTDTC = c("2019-12-29", "2019-12-28"),
                  stringsAsFactors=FALSE
              )
              
              EX <- data.frame(
                  USUBJID = c(rep(1,2),rep(2,2)),
                  EXSTDTC = c(NA, NA, "2019-12-26","2019-12-27"),
                  EXENDTC = c(NA, NA, "2019-12-30", "2019-12-27"),
                  EXTRT = c(rep("SOME DRUG", 2), rep("PLACEBO", 2)),
                  EXDOSE=c(10,10,0,0),
                  stringsAsFactors=FALSE
              )
              
              expect_false(check_ds_ex_after_discon(DS, EX))
              
          })


test_that("Function returns FALSE when study discontinuation occurs before last
          treatment date",{
              
              DS <- data.frame(
                  USUBJID = c(1,2),
                  DSSCAT= rep(c("STUDY COMPLETION/EARLY DISCONTINUATION"),2),
                  DSCAT = rep(c("DISPOSITION EVENT"),2),
                  DSSTDTC = c("2019-12-29", "2019-12-28"),
                  stringsAsFactors=FALSE
              )
              
              EX <- data.frame(
                  USUBJID = c(rep(1,2),rep(2,2)),
                  EXSTDTC = c("2019-12-15", "2019-12-23", "2019-12-20","2019-12-27"),
                  EXENDTC = c("2019-12-20", "2019-12-28", "2019-12-30","2019-12-27"),
                  EXTRT = c(rep("SOME DRUG", 2), rep("PLACEBO", 2)),
                  EXDOSE=c(10,10,0,0),
                  stringsAsFactors=FALSE
              )
              
              expect_false(check_ds_ex_after_discon(DS, EX))
              
          })

