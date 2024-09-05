test_that("creates list from run_all_checks", {
# Assuming sdtm datasets are in your global environment
# Note we are only specifying AE and DS here so all unrelated checks wont be run

ae <- data.frame(
  STUDYID = 1,
  USUBJID = c(1,2,3,1,2,3),
  AESTDTC = '2020-05-05',
  AETERM  = c("abc Covid-19", "covid TEST POSITIVE",rep("other AE",4)),
  AEDECOD = c("COVID-19", "CORONAVIRUS POSITIVE", rep("OTHER AE",4)),
  AEACN = c("DRUG WITHDRAWN", rep("DOSE NOT CHANGED",5)),
  AESPID = "FORMNAME-R:13/L:13XXXX",
  stringsAsFactors = FALSE
)

ds <- data.frame(
  USUBJID = c(1,1,2,3,4),
  DSSPID  = 'XXX-DISCTX-XXX',
  DSCAT   = "DISPOSITION EVENT",
  DSDECOD = "OTHER REASON",
  DSSEQ = c(1,2,1,1,1),
  stringsAsFactors = FALSE
)

all_rec<-run_all_checks(metads=sdtmchecksmeta, 
                        verbose=FALSE)

expect_true(is.list(all_rec))

})
