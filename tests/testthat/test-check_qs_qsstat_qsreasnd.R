incorrect_parameters_qsstat <- data.frame("BadVar1" = c("NOT DONE", "NOT DONE"), "QSREASND" = c("", ""))

#' QS <- data.frame(USUBJID = c(1,1,1,2,2,2),
#'                  QSDTC   = c("2015-06-30", "2015-09-30", "2015-12-30", "2015-06-30", "2015-09-30", "2015-12-30"),
#'                  QSCAT   = rep("A"),
#'                  VISIT  =  c("Week 1", "Week 12", "Week 24", "Week 1", "Week 12", "Week 24"),
#'                  QSSTAT  = c("Not Done","NOT DONE","not done", rep("",3)),
#'                  QSREASND = c("Reasons",rep("",5)),
#'                  stringsAsFactors = FALSE)


incorrect_parameters_qsreasnd <- data.frame("QSSTAT" = c("NOT DONE", "NOT DONE"),
                                            "BadVar2" = c("", ""),
                                            "USUBJID" = rep(1,2),
                                            "QSDTC" = rep(1,2),
                                            "QSCAT" = rep(1,2))
incorrect_parameters_both <- data.frame("BadVar1" = c("NOT DONE", "NOT DONE"),
                                        "BadVar2" = c("", ""),
                                        "USUBJID" = rep(1,2),
                                        "QSDTC" = rep(1,2),
                                        "QSCAT" = rep(1,2))
incorrect_parameters_empty <- data.frame()
fail_qsstat_entered_with_reason <- data.frame("QSSTAT" = c("NOT DONE", ""),
                                              "QSREASND" = c("", ""),
                                              "USUBJID" = rep(1,2),
                                              "QSDTC" = rep(1,2),
                                              "QSCAT" = rep(1,2))
pass_qsstat_entered_with_reason <- data.frame("QSSTAT" = c("NOT DONE", ""),
                                              "QSREASND" = c("Test was not done", ""),
                                              "USUBJID" = rep(1,2),
                                              "QSDTC" = rep(1,2),
                                              "QSCAT" = rep(1,2))
pass_qsstat_and_qsreasnd_empty <- data.frame("QSSTAT" = c("", ""),
                                             "QSREASND" = c("", ""),
                                             "USUBJID" = rep(1,2),
                                             "QSDTC" = rep(1,2),
                                             "QSCAT" = rep(1,2))


test_that("function fails if correct parameters are not passed", {
    expect_equal(check_qs_qsstat_qsreasnd(incorrect_parameters_qsstat)[1], FALSE)
    expect_equal(check_qs_qsstat_qsreasnd(incorrect_parameters_qsreasnd)[1], FALSE)
    expect_equal(check_qs_qsstat_qsreasnd(incorrect_parameters_both)[1], FALSE)
    expect_equal(check_qs_qsstat_qsreasnd(incorrect_parameters_empty)[1], FALSE)
})

test_that("Function fails when a reason is not entered and status is entered as not done", {
    expect_equal(check_qs_qsstat_qsreasnd(fail_qsstat_entered_with_reason)[1], FALSE)
})

test_that("Function passes When NOT DONE is entered there is a reason entered", {
    expect_equal(check_qs_qsstat_qsreasnd(pass_qsstat_entered_with_reason)[1], TRUE)
})

test_that("Function passes when both qsreasnd and qsstat are empty", {
    expect_equal(check_qs_qsstat_qsreasnd(pass_qsstat_and_qsreasnd_empty)[1], TRUE)
})
