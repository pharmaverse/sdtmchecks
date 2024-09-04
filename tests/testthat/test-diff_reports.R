test_that("diff reports runs", {
   # Step 1: Simulate an older AE dataset with one missing preferred term

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

   # Step 2: Use the run_all_checks() function to generate list of check results on this "old" data

   # Filter sdtmchecksmeta so that only one check is present
   metads <- sdtmchecksmeta[sdtmchecksmeta$check=="check_ae_aedecod",]
   old <- run_all_checks(metads=metads, verbose = FALSE)

   #Step 3: Simulate a newer, updated AE dataset with another record with a new missing preferred term

    ae <- data.frame(
    USUBJID = 1:6,
    DOMAIN = c(rep("AE", 6)),
    AESEQ = 1:6,
    AESTDTC = 1:6,
    AETERM = 1:6,
    AEDECOD = 1:6,
     AESPID = c("FORMNAME-R:13/L:13XXXX",
                "FORMNAME-R:16/L:16XXXX",
                "FORMNAME-R:2/L:2XXXX",
                "FORMNAME-R:19/L:19XXXX",
                "FORMNAME-R:5/L:5XXXX",
                "FORMNAME-R:1/L:5XXXX"
                ),
    stringsAsFactors = FALSE
   )

   ae$AEDECOD[1] = NA
   ae$AEDECOD[6] = NA

   # Step 4: use the run_all_checks() function to generate list of check results  on this "new" data

   new <- run_all_checks(metads=metads, verbose = FALSE)

   # Step 5: Diff to create a column indicating if the finding is new
   res <- suppressMessages(diff_reports(old_report=old, new_report=new))

   expect_true(is.list(res))

})
