test_that("Returns true if no errors present - 1", {
  
  TR <- data.frame(USUBJID = 1:5,
                   TRTESTCD = c("OTHER", rep("LDIAM", 4)),
                   TRLINKID = 1:5,
                   TRDTC = 1:5,
                   VISIT = LETTERS[1:5],
                   TRORRES = LETTERS[1:5],
                   TRSTRESN = 1:5,
                   TRSTAT = "",
                   TREVAL = "INVESTIGATOR",
                   TRSPID = "FORMNAME-R:19/L:19XXXX",
                   stringsAsFactors = FALSE)
  
  expect_true(check_tr_trstresn_ldiam(TR))
  
})

test_that("Returns true if no errors present - 2", {
  
  TR <- data.frame(USUBJID = 1:5,
                   TRTESTCD = c("OTHER", rep("LDIAM", 4)),
                   TRLINKID = 1:5,
                   TRDTC = 1:5,
                   VISIT = LETTERS[1:5],
                   TRORRES = LETTERS[1:5],
                   TRSTRESN = 1:5,
                   TRSTAT = "",
                   TREVAL = "INVESTIGATOR",
                   TRSPID = "FORMNAME-R:19/L:19XXXX",
                   stringsAsFactors = FALSE)
  
  
  TR1 <- TR
  TR1$TRSTAT <- NULL
  TR1$TREVAL <- NULL
  TR1$TRSPID <- NULL
  
  expect_true(check_tr_trstresn_ldiam(TR1))
  
})

test_that("Returns false if errors present - 1", {
  
  TR <- data.frame(USUBJID = 1:5,
                   TRTESTCD = c("OTHER", rep("LDIAM", 4)),
                   TRLINKID = 1:5,
                   TRDTC = 1:5,
                   VISIT = LETTERS[1:5],
                   TRORRES = LETTERS[1:5],
                   TRSTRESN = 1:5,
                   TRSTAT = "",
                   TREVAL = "INVESTIGATOR",
                   TRSPID = "FORMNAME-R:19/L:19XXXX",
                   stringsAsFactors = FALSE)
  
  
  TR1 <- TR
  TR1$TRSTAT <- NULL
  TR1$TREVAL <- NULL
  TR1$TRSPID <- NULL
  TR1$TRTESTCD <- c(rep("OTHER", 5))
  
 expect_false(check_tr_trstresn_ldiam(TR1))
  
})


test_that("Returns false if errors present - 2", {
  
  
  TR <- data.frame(USUBJID = 1:5,
                   TRTESTCD = c("OTHER", rep("LDIAM", 4)),
                   TRLINKID = 1:5,
                   TRDTC = 1:5,
                   VISIT = LETTERS[1:5],
                   TRORRES = LETTERS[1:5],
                   TRSTRESN = 1:5,
                   TRSTAT = "",
                   TREVAL = "INVESTIGATOR",
                   TRSPID = "FORMNAME-R:19/L:19XXXX",
                   stringsAsFactors = FALSE)
  
  TR$TRTESTCD <- c(rep("OTHER", 5))
  
  expect_false(check_tr_trstresn_ldiam(TR))
  
})


test_that("Returns false if errors present - 3", {
  
  
  TR <- data.frame(USUBJID = 1:5,
                   TRTESTCD = c("OTHER", rep("LDIAM", 4)),
                   TRLINKID = 1:5,
                   TRDTC = 1:5,
                   VISIT = LETTERS[1:5],
                   TRORRES = LETTERS[1:5],
                   TRSTRESN = 1:5,
                   TRSTAT = "",
                   TREVAL = "INVESTIGATOR",
                   TRSPID = "FORMNAME-R:19/L:19XXXX",
                   stringsAsFactors = FALSE)
  
  
  TR2 <- TR
  TR2$TRSTRESN <- c("", "NA", NA, 1, 1)
  
  expect_false(check_tr_trstresn_ldiam(TR2))
  
})

test_that("Returns false if errors present - 4", {
  
  
  TR <- data.frame(USUBJID = 1:5,
                   TRTESTCD = c("OTHER", rep("LDIAM", 4)),
                   TRLINKID = 1:5,
                   TRDTC = 1:5,
                   VISIT = LETTERS[1:5],
                   TRORRES = LETTERS[1:5],
                   TRSTRESN = 1:5,
                   TRSTAT = "",
                   TREVAL = "INVESTIGATOR",
                   TRSPID = "FORMNAME-R:19/L:19XXXX",
                   stringsAsFactors = FALSE)
  
  
  TR2 <- TR
  TR2$TRSTRESN <- c("", "NA", NA, 1, 1)
  
  expect_false(check_tr_trstresn_ldiam(TR2,preproc=roche_derive_rave_row))
  
})

test_that("Returns false if expected column not present", {
  
  TR <- data.frame(USUBJID = 1:5,
                   TRTESTCD = c("OTHER", rep("LDIAM", 4)),
                   TRLINKID = 1:5,
                   TRDTC = 1:5,
                   VISIT = LETTERS[1:5],
                   TRORRES = LETTERS[1:5],
                   TRSTRESN = 1:5,
                   TRSTAT = "",
                   TREVAL = "INVESTIGATOR",
                   TRSPID = "FORMNAME-R:19/L:19XXXX",
                   stringsAsFactors = FALSE)
  
  TR$USUBJID <- NULL
  
  expect_false(check_tr_trstresn_ldiam(TR))
  
})




 

