test_that("Function returns true when no errors are present", {
  
  LB1 <- data.frame(USUBJID = c(rep("101", 5), rep("102", 5)),
                 LBCAT = "Hematology",
                 LBDTC = rep(c(
                 "2017-01-01T08:25",
                 "2017-01-05T09:25",
                 "2017-01-15T10:25",
                 "2017-01-20T08:25",
                 "2017-01-25T08:25"), 2),
                 VISITNUM = rep(1:5,2),
                 VISIT = rep(c(
                 "Visit 1",
                 "Visit 2",
                 "Visit 3",
                 "UNSCheduled!!!",
                 "VIsit 5"), 2),
                 LBSTAT = c(rep("", 9), "NOT DONE"),
                 stringsAsFactors = FALSE)
    
  
  
  expect_true(check_lb_lbdtc_visit_ordinal_error(LB1))
})

test_that("Function returns false when errors are present", {
  
  LB1 <- data.frame(USUBJID = c(rep("101", 5), rep("102", 5)),
                    LBCAT = "Hematology",
                    LBDTC = rep(c(
                      "2017-01-01T08:25",
                      "2017-01-05T09:25",
                      "2017-01-15T10:25",
                      "2017-01-20T08:25",
                      "2017-01-25T08:25"), 2),
                    VISITNUM = rep(1:5,2),
                    VISIT = rep(c(
                      "Visit 1",
                      "Visit 2",
                      "Visit 3",
                      "UNSCheduled!!!",
                      "VIsit 5"), 2),
                    LBSTAT = c(rep("", 9), "NOT DONE"),
                    stringsAsFactors = FALSE)
  
  LB2 = LB1
  LB2$LBCAT = "Virology"
  LB3 <- rbind(LB1, LB2)
  LB3$LBDTC[LB3$USUBJID == 101 & LB3$VISIT == "Visit 3"] <- "2016-01-10T08:25"
  LB3$LBDTC[LB3$USUBJID == 102 & LB3$VISIT == "Visit 2"] <- "2016-01-01T06:25"
  
  expect_false(check_lb_lbdtc_visit_ordinal_error(LB3))
})


test_that("Function returns false when errors are present - 3", {
  
  LB1 <- data.frame(USUBJID = c(rep("101", 5), rep("102", 5)),
                    LBCAT = "Hematology",
                    LBDTC = rep(c(
                      "2017-01-01T08:25",
                      "2017-01-05T09:25",
                      "2017-01-15T10:25",
                      "2017-01-20T08:25",
                      "2017-01-25T08:25"), 2),
                    VISITNUM = rep(1:5,2),
                    VISIT = rep(c(
                      "Visit 1",
                      "Visit 2",
                      "Visit 3",
                      "UNSCheduled!!!",
                      "VIsit 5"), 2),
                    LBSTAT = c(rep("", 9), "NOT DONE"),
                    stringsAsFactors = FALSE)
  
  LB2 = LB1
  LB2$LBCAT = "Virology"
  LB3 <- rbind(LB1, LB2)
  LB3$LBSTAT = "NOT DONE"
  
  expect_false(check_lb_lbdtc_visit_ordinal_error(LB3))
})



test_that("Function returns false when expected column not present", {
  
  LB1 <- data.frame(USUBJID = c(rep("101", 5), rep("102", 5)),
                    LBCAT = "Hematology",
                    LBDTC = rep(c(
                      "2017-01-01T08:25",
                      "2017-01-05T09:25",
                      "2017-01-15T10:25",
                      "2017-01-20T08:25",
                      "2017-01-25T08:25"), 2),
                    VISITNUM = rep(1:5,2),
                    VISIT = rep(c(
                      "Visit 1",
                      "Visit 2",
                      "Visit 3",
                      "UNSCheduled!!!",
                      "VIsit 5"), 2),
                    LBSTAT = c(rep("", 9), "NOT DONE"),
                    stringsAsFactors = FALSE)
  
  LB1$LBDTC=NULL
  
  expect_false(check_lb_lbdtc_visit_ordinal_error(LB1))
})

