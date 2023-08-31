test_that("Returns true when no errors present", {
  
  USUBJID<- c(1:10)
  DM=data.frame(USUBJID)
  AE=data.frame(USUBJID)
  
  EX <- data.frame(
    USUBJID = c(1:8, 6, 8, 10, 10, 10, 10),
    EXOCCUR = rep("Y", times=14),
    EXDOSE = rep(c(1,2), times=7),
    EXSTDTC = c(rep("2012-01-01", 10),"2012-02-04","2012-02-04", "", "2012-02-07"),
    EXTRT = "GDC",
    stringsAsFactors=FALSE
  )
  
  DS <- data.frame(
    USUBJID = c(2,8,8),
    DSDECOD = rep("DEATH", times=3),
    DSSTDTC = c("2012-12-01", NA, "2013-07-01"),
    stringsAsFactors=FALSE
  )
  
  expect_true(check_dm_usubjid_ae_usubjid(DM, AE, DS, EX))
  
}) 

test_that("Returns false when errors present - 1", {
  USUBJID<- c(1:10)
  DM=data.frame(USUBJID)
  AE=data.frame(USUBJID)
  AE$USUBJID[3]=NA
  AE$USUBJID[8]=NA
  AE$USUBJID[10]=NA
  
  EX <- data.frame(
    USUBJID = c(1:8, 6, 8, 10, 10, 10, 10),
    EXOCCUR = rep("Y", times=14),
    EXDOSE = rep(c(1,2), times=7),
    EXSTDTC = c(rep("2012-01-01", 10),"2012-02-04","2012-02-04", "", "2012-02-07"),
    EXTRT = "GDC",
    stringsAsFactors=FALSE
  )
  
  DS <- data.frame(
    USUBJID = c(2,8,8),
    DSDECOD = rep("DEATH", times=3),
    DSSTDTC = c("2012-12-01", NA, "2013-07-01"),
    stringsAsFactors=FALSE
  )
  
  expect_false(check_dm_usubjid_ae_usubjid(DM, AE, DS, EX))
  
})

test_that("Returns false when errors present - 2", {
  USUBJID<- c(1:10)
  DM=data.frame(USUBJID)
  AE=data.frame(USUBJID)
  AE$USUBJID[3]=NA
  AE$USUBJID[8]=NA
  AE$USUBJID[10]=NA
  
  EX <- data.frame(
    USUBJID = c(1:8, 6, 8, 10, 10, 10, 10),
    EXOCCUR = rep("Y", times=14),
    EXDOSE = rep(c(1,2), times=7),
    EXSTDTC = c(rep("2012-01-01", 10),"2012-02-04","2012-02-04", "", "2012-02-07"),
    EXTRT = "GDC",
    stringsAsFactors=FALSE
  )
  
  DS <- data.frame(
    USUBJID = c(2,8,8),
    DSDECOD = rep("DEATH", times=3),
    DSSTDTC = c("2012-12-01", NA, "2013-07-01"),
    stringsAsFactors=FALSE
  )
  
  EX$EXOCCUR[3]="N"
  
  expect_false(check_dm_usubjid_ae_usubjid(DM, AE, DS, EX))
})

test_that("Returns false when expected column not present - 1", {
  USUBJID<- c(1:10)
  DM=data.frame(USUBJID)
  AE=data.frame(USUBJID)

  EX <- data.frame(
    USUBJID = c(1:8, 6, 8, 10, 10, 10, 10),
    EXOCCUR = rep("Y", times=14),
    EXDOSE = rep(c(1,2), times=7),
    EXSTDTC = c(rep("2012-01-01", 10),"2012-02-04","2012-02-04", "", "2012-02-07"),
    EXTRT = "GDC",
    stringsAsFactors=FALSE
  )
  
  DS <- data.frame(
    USUBJID = c(2,8,8),
    DSDECOD = rep("DEATH", times=3),
    DSSTDTC = c("2012-12-01", NA, "2013-07-01"),
    stringsAsFactors=FALSE
  )
  
  EX$EXDOSE=NULL
  
  expect_false(check_dm_usubjid_ae_usubjid(DM, AE, DS, EX))
})

test_that("Returns false when expected column not present - 2", {
  USUBJID<- c(1:10)
  DM=data.frame(USUBJID)
  AE=data.frame(USUBJID)
  
  EX <- data.frame(
    USUBJID = c(1:8, 6, 8, 10, 10, 10, 10),
    EXOCCUR = rep("Y", times=14),
    EXDOSE = rep(c(1,2), times=7),
    EXSTDTC = c(rep("2012-01-01", 10),"2012-02-04","2012-02-04", "", "2012-02-07"),
    EXTRT = "GDC",
    stringsAsFactors=FALSE
  )
  
  DS <- data.frame(
    USUBJID = c(2,8,8),
    DSDECOD = rep("DEATH", times=3),
    DSSTDTC = c("2012-12-01", NA, "2013-07-01"),
    stringsAsFactors=FALSE
  )
  
  DS$DSDECOD=NULL
  
  expect_false(check_dm_usubjid_ae_usubjid(DM, AE, DS, EX))
})

test_that("Returns false when expected column not present - 3", {
  USUBJID<- c(1:10)
  DM=data.frame(USUBJID)
  AE=data.frame(USUBJID)
  
  EX <- data.frame(
    USUBJID = c(1:8, 6, 8, 10, 10, 10, 10),
    EXOCCUR = rep("Y", times=14),
    EXDOSE = rep(c(1,2), times=7),
    EXSTDTC = c(rep("2012-01-01", 10),"2012-02-04","2012-02-04", "", "2012-02-07"),
    EXTRT = "GDC",
    stringsAsFactors=FALSE
  )
  
  DS <- data.frame(
    USUBJID = c(2,8,8),
    DSDECOD = rep("DEATH", times=3),
    DSSTDTC = c("2012-12-01", NA, "2013-07-01"),
    stringsAsFactors=FALSE
  )
  
  AE$USUBJID=NULL
  
  expect_false(check_dm_usubjid_ae_usubjid(DM, AE, DS, EX))
})

test_that("Returns false when expected column not present - 4", {
  USUBJID<- c(1:10)
  DM=data.frame(USUBJID)
  AE=data.frame(USUBJID)
  
  EX <- data.frame(
    USUBJID = c(1:8, 6, 8, 10, 10, 10, 10),
    EXOCCUR = rep("Y", times=14),
    EXDOSE = rep(c(1,2), times=7),
    EXSTDTC = c(rep("2012-01-01", 10),"2012-02-04","2012-02-04", "", "2012-02-07"),
    EXTRT = "GDC",
    stringsAsFactors=FALSE
  )
  
  DS <- data.frame(
    USUBJID = c(2,8,8),
    DSDECOD = rep("DEATH", times=3),
    DSSTDTC = c("2012-12-01", NA, "2013-07-01"),
    stringsAsFactors=FALSE
  )
  
  DM$USUBJID=NULL
  
  expect_false(check_dm_usubjid_ae_usubjid(DM, AE, DS, EX))
})

