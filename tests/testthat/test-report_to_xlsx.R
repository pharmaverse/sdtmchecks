# why is a set as a null? bc we did not actually apply run_all_checks?
# should it be more like checking if the file extension after application is xlsx?
test_that("creates xlsx file", {
# Create Dummy data
ae = data.frame(
  USUBJID = 1:5,
  DOMAIN = c(rep("AE", 5)),
  AESEQ = 1:5,
  AESTDTC = 1:5,
  AETERM = 1:5,
  AEDECOD = 1:5,
  AESPID = c("FORMNAME-R:13/L:13XXXX",
             "FORMNAME-R:16/L:16XXXX",
             "FORMNAME-R:2/L:2XXXX",
             "FORMNAME-R:19/L:19XXXX",
             "FORMNAME-R:5/L:5XXXX"),
  stringsAsFactors = FALSE
)

cm = data.frame(
  USUBJID = 1:5,
  DOMAIN = rep("CM", 5),
  CMTRT = rep("DRUG TERM", 5),
  CMDECOD = rep("CODED DRUG TERM", 5),
  CMSTDTC = 1:5,
  CMENDTC = 1:5,
  CMCAT = "CONCOMITANT MEDICATIONS",
  CMSPID = c("FORMNAME-R:13/L:13XXXX",
             "FORMNAME-R:16/L:16XXXX",
             "FORMNAME-R:2/L:2XXXX",
             "FORMNAME-R:19/L:19XXXX",
             "FORMNAME-R:5/L:5XXXX"),
  stringsAsFactors=FALSE
)

res=run_all_checks(verbose = FALSE)
fileName <- file.path(tempdir(), "check_results.xlsx")
invisible(capture.output(a <- report_to_xlsx(res=res,outfile = fileName)))

expect_true(file.exists(fileName))

})
