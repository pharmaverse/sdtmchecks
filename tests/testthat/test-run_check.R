test_that("creates list from run_check", {

# Assuming sdtm datasets are in your global environment

ae <- data.frame(
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

ae$AEDECOD[1] = NA

# Filter sdtmchecksmeta so that only one check is present
metads <- sdtmchecksmeta[sdtmchecksmeta$check=="check_ae_aedecod",] 

a <- run_check(
  check = metads$check,
  fxn_in = metads$fxn_in,
  xls_title = metads$xls_title,
  pdf_title = metads$pdf_title,
  pdf_subtitle = metads$pdf_subtitle,
  pdf_return = metads$pdf_return,
  verbose = FALSE
)

expect_true(is.list(a))

})
