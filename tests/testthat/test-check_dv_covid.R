test_that("function returns true when no errors are present", {

    DV <- data.frame(
             USUBJID = 1:3,
             DVEPRELI = c("Y","Y","N"),
             DVREAS=c("EPIDEMIC/PANDEMIC INFECTION","EPIDEMIC/PANDEMIC INFECTION",""),
             stringsAsFactors=FALSE
         )
        

    expect_true(check_dv_covid(DV))

})

test_that("function returns false when errors are present", {

    DV <- data.frame(
             USUBJID = 1:3,
             DVEPRELI = c("Y","N","Y"),
             DVREAS=c("EPIDEMIC/PANDEMIC INFECTION","EPIDEMIC/PANDEMIC INFECTION",""),
             stringsAsFactors=FALSE
         )
        
    expect_false(check_dv_covid(DV))
})

test_that("Function returns true when no errors are present for an empty dataframe (zero rows)", {

    DV <- data.frame(
             USUBJID = character(),
             DVEPRELI = character(),
             DVREAS= character(),
             stringsAsFactors=FALSE
         )
        
    expect_true(check_dv_covid(DV))
})


test_that("Function returns true when no errors are present for a single input (one row)", {

    DV <- data.frame(
             USUBJID = 1,
             DVEPRELI = c("Y"),
             DVREAS=c("EPIDEMIC/PANDEMIC INFECTION"),
             stringsAsFactors=FALSE
        )
        expect_true(check_dv_covid(DV))
})

test_that("Function returns false when errors are present for a single input (one row)", {

    DV <- data.frame(
        USUBJID = 1,
        DVEPRELI = NA,
        DVREAS=c("EPIDEMIC/PANDEMIC INFECTION"),
        stringsAsFactors=FALSE
    )
    expect_true(check_dv_covid(DV))
})


# test_that("Function returns true when no errors are present for a multiple inputs (900 rows)", {
# 
#     USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)
# 
#     AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
#                      AESTDTC = as.character(rep(seq(as.Date('2010-01-01'), as.Date('2010-03-01'), by = 1), times = 15)),
#                      AEDTHDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
#                      AESDTH = rep("Y", times = 900),
#                      AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
#                                          "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
#                      AEOUT = "FATAL",
#                      AESDTH = "Y",
#                      AETOXGR = "5",
#                      stringsAsFactors = FALSE)
# 
#     expect_true(check_dv_covid(DV))
# })
# 
# test_that("Function returns false when errors are present for a multiple inputs (900 rows)", {
# 
#     USUBJID <- rep(unlist(lapply(X =list("MYSTUDY-S0001-1"), function(x) paste0(x,seq(from = 100, to = 999)))), times = 1)
# 
#     AE <- data.frame(USUBJID = USUBJID[order(nchar(USUBJID), USUBJID)],
#                      AESTDTC = as.character(rep(seq(as.Date('2010-01-01'), as.Date('2010-03-01'), by = 1), times = 15)),
#                      AEDTHDTC = as.character(rep(seq(as.Date('2011-01-01'), as.Date('2011-03-01'), by = 1), times = 15)),
#                      AESDTH = rep("Y", times = 900),
#                      AEDECOD = rep(x = c("Upper Respiratory Tract Infection","Headache","Thrombocytopenia",
#                                          "Blood Potassium Decreased","Anaemia","Tremor"), times = 150),
#                      AEOUT = "FATAL",
#                      AESDTH = "Y",
#                      AETOXGR = c(rep("5", times = 800), rep("3", times = 100)),
#                      stringsAsFactors = FALSE)
# 
# 
#     expect_false(check_dv_covid(DV))
# })
# 
# test_that("Function returns the failed object in attr(data)", {
# 
#     AE <- data.frame(
#         USUBJID = 1:5,
#         AESTDTC = "01JAN2017",
#         AEDECOD = c("AE1","AE2","AE3","AE4","AE5"),
#         AEOUT = "FATAL",
#         AEDTHDTC = c("01FEB2017",NA,"02FEB2017","03FEB2017",NA),
#         AESDTH = c("Y","Y","N","Y",NA),
#         AETOXGR = c("5","5","5",NA,NA),
#         stringsAsFactors = FALSE
#     )
# 
#     check <- check_dv_covid(DV)
# 
#     expect_true(!is.null(attr(check, "data")))
#     # expect_equal(attr(check, "data"), filter(AE, !is.na(AEDTHDTC) & AESDTH != "Y"))
# 
# })


