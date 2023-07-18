
test_that("Function returns true when no errors are present", {
    
    
    AE <- data.frame(
        STUDYID = 1001,
        USUBJID = c(1,2,3,1,2,3),
        AESTDTC = rep('2020-05-05',6),
        AETERM  = c("abc Covid-19", "covid TEST POSITIVE","CHILLS"),
        AESEQ   = c(1,1,1,2,2,2),
        AEREL   = c("Y", "N", "NA", "N", "N", "Y"),
        AEREL1  = c("Y", "N", "NA", "N", "NA", "Y"),
        AEREL2  = c("Y", "N", "NA", "N", "N", "N"),
        AESPID = "FORMNAME-R:13/L:13XXXX",
        stringsAsFactors = FALSE
    )
    
    expect_true(check_ae_aerel(AE))
})

test_that("Function returns false when errors are present", {
    
    AE1 <- data.frame(
        STUDYID = 1001,
        USUBJID = c(1,2,3,1,2,3),
        AESTDTC = rep('2020-05-05',6),
        AETERM  = c("abc Covid-19", "covid TEST POSITIVE","CHILLS"),
        AESEQ   = c(1,1,1,2,2,2),
        AEREL   = c("Y", "N", "N", "N", "N", "N"),
        AEREL1  = c("Y", "N", "NA", "N", "N", ""),
        AEREL2  = c("Y", "N", " ", "N", "N", " "),
        AESPID = "FORMNAME-R:13/L:13XXXX",
        stringsAsFactors = FALSE
    )
    
    expect_false(check_ae_aerel(AE1))
})


test_that("Function returns false when errors are present", {
    
    AE2 <- data.frame(
        STUDYID = 1001,
        USUBJID = c(1,2,3,1,2,3),
        AESTDTC = rep('2020-05-05',6),
        AETERM  = c("abc Covid-19", "covid TEST POSITIVE","CHILLS"),
        AESEQ   = c(1,1,1,2,2,2),
        AEREL   = c("Y", "N", " ", "N", "N", " "),
        AEREL1  = c("NA", "N", "NA", "Y", "N", " "),
        AEREL2  = c("Y", "N", " ", "N", "N", " "),
        AESPID = "FORMNAME-R:13/L:13XXXX",
        stringsAsFactors = FALSE
    )
    
    expect_false(check_ae_aerel(AE2))

})



test_that("Function returns false when errors are present - with processing ", {
    
    AE2 <- data.frame(
        STUDYID = 1001,
        USUBJID = c(1,2,3,1,2,3),
        AESTDTC = rep('2020-05-05',6),
        AETERM  = c("abc Covid-19", "covid TEST POSITIVE","CHILLS"),
        AESEQ   = c(1,1,1,2,2,2),
        AEREL   = c("Y", "N", " ", "N", "N", " "),
        AEREL1  = c("NA", "N", "NA", "Y", "N", " "),
        AEREL2  = c("Y", "N", " ", "N", "N", " "),
        AESPID = "FORMNAME-R:13/L:13XXXX",
        stringsAsFactors = FALSE
    )
    
    expect_false(check_ae_aerel(AE2, preproc=roche_derive_rave_row))
    
})


test_that("Function returns false when errors are present", {
    
    
    AE3 <- data.frame(
        STUDYID = 1001,
        USUBJID = c(1,2,3,1,2,3),
        AESTDTC = rep('2020-05-05',6),
        AETERM  = c("abc Covid-19", "covid TEST POSITIVE","CHILLS"),
        AESEQ   = c(1,1,1,2,2,2),
        AEREL   = c("Y", " ", " ", "N", " ", "NA"),
        AESPID = "FORMNAME-R:13/L:13XXXX",
        stringsAsFactors = FALSE
    )
    
    expect_false(check_ae_aerel(AE3))
})

test_that("Function returns false when errors are present - with processing", {
    
    
    AE3 <- data.frame(
        STUDYID = 1001,
        USUBJID = c(1,2,3,1,2,3),
        AESTDTC = rep('2020-05-05',6),
        AETERM  = c("abc Covid-19", "covid TEST POSITIVE","CHILLS"),
        AESEQ   = c(1,1,1,2,2,2),
        AEREL   = c("Y", " ", " ", "N", " ", "NA"),
        AESPID = "FORMNAME-R:13/L:13XXXX",
        stringsAsFactors = FALSE
    )
    
    expect_false(check_ae_aerel(AE3,preproc=roche_derive_rave_row))
})

test_that("Function returns false when key columns set to null", {

    AE4 <- data.frame(
        STUDYID = 1001,
        USUBJID = c(1,2,3,4,5,6),
        AESTDTC = rep('2020-05-05',6),
        AETERM  = c("abc Covid-19", "covid TEST POSITIVE","CHILLS"),
        AESEQ   = c(1,2,3,4,5,6),
        AEREL   = c("Y", "Y", "N", "", "Y", "NA"),
        AEREL1  = "",
        AEREL2  = "",
        AESPID = "FORMNAME-R:13/L:13XXXX",
        stringsAsFactors = FALSE
    )

    expect_false(check_ae_aerel(AE4))
})


test_that("Function returns false when key columns set to null - with processing", {
    
    AE4 <- data.frame(
        STUDYID = 1001,
        USUBJID = c(1,2,3,4,5,6),
        AESTDTC = rep('2020-05-05',6),
        AETERM  = c("abc Covid-19", "covid TEST POSITIVE","CHILLS"),
        AESEQ   = c(1,2,3,4,5,6),
        AEREL   = c("Y", "Y", "N", "", "Y", "NA"),
        AEREL1  = "",
        AEREL2  = "",
        AESPID = "FORMNAME-R:13/L:13XXXX",
        stringsAsFactors = FALSE
    )
    
    expect_false(check_ae_aerel(AE4,preproc=roche_derive_rave_row))
})




    









