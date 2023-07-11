MI <- data.frame(
    USUBJID = c(1:3),
    DOMAIN = c("MI"),
    MISEQ = c(1),
    MISPEC = c("SLIDE", "TUMOR TISSUE", "BLOCK SLIDE"),
    MITESTCD = c("STNPTN", "IHCSCO", "CELLF"),
    MIDTC = c("", "", ""),
    stringsAsFactors = FALSE
)



MI2 <- data.frame(
    USUBJID = c(1:3),
    DOMAIN = c("MI"),
    MISEQ = c(1),
    MISPEC = c("", "TUMOR TISSUE", NA),
    MITESTCD = c("STNPTN", "IHCSCO", "CELLF"),
    MIDTC = c("2023-01-01", "2023-02-01", "2023-03-01"),
    stringsAsFactors = FALSE
)


test_that("function returns true when no errors are present", {

    expect_true(check_mi_mispec(MI))

})


AE <- data.frame(
    USUBJID = c("ABC-123", "ABC-124", "ABC-125"),
    DOMAIN = c("AE"),
    AESEQ = c(1),
    AETERM = c("COUGH", "FEVER", "NAUSEA"),
    AESTDTC = c("2023-01-01", "2023-02-01", "2023-03-01"),
    stringsAsFactors = FALSE
)




test_that("function returns false when errors are present", {

    expect_false(check_mi_mispec(MI2))

    MI3 <- select(MI, -MISPEC)
    expect_false(check_mi_mispec(MI3))



    expect_false(check_mi_mispec(AE))

})
