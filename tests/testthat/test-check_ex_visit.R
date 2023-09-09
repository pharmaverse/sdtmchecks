EX <- data.frame(
    USUBJID = 1:3,
    EXTRT = 1:3,
    EXSTDTC = 1:3,
    EXOCCUR = "Y",
    VISIT = NA, 
    stringsAsFactors = FALSE
)




test_that("function returns false when errors are present", {
    
    expect_false(check_ex_visit(EX))
    
})



test_that("function returns false when no errors are present and optional variable missing", {
    
    EX1 <- EX
    EX1$EXOCCUR=NULL
    
    
    expect_false(check_ex_visit(EX1))
    
})


test_that("function returns false when mandatory variable missing", {
    
    EX2 <- EX
    EX2$VISIT=NULL
    
    expect_false(check_ex_visit(EX2))
    
})


test_that("function returns true when no errors are present", {
    
    EX3 <- data.frame(
        USUBJID = 1:3,
        EXTRT = 1:3,
        EXSTDTC = 1:3,
        EXOCCUR = "Y",
        VISIT = c("VISIT 1", "VISIT 2", "VISIT 3"), 
        stringsAsFactors = FALSE
    )
    
    
    expect_true(check_ex_visit(EX3))
    
})


test_that("function returns false when only 1 error is present (VISIT = NA)", {
    
    EX4 <- data.frame(
        USUBJID = 1:3,
        EXTRT = 1:3,
        EXSTDTC = 1:3,
        EXOCCUR = "Y",
        VISIT = c("VISIT 1", "VISIT 2", NA), 
        stringsAsFactors = FALSE
    )
    
    
    expect_false(check_ex_visit(EX4))
    
})

test_that("function returns true when VISIT missing for record with EXOCCUR ne Y", {
    
    EX5 <- data.frame(
        USUBJID = 1:3,
        EXTRT = 1:3,
        EXSTDTC = 1:3,
        EXOCCUR = c("Y", "Y", ""),
        VISIT = c("VISIT 1", "VISIT 2", NA), 
        stringsAsFactors = FALSE
    )
    
    
    expect_true(check_ex_visit(EX5))
    
})


test_that("function returns true when VISIT populated as UNSCHEDULED", {
    
    EX6 <- data.frame(
        USUBJID = 1:3,
        EXTRT = 1:3,
        EXSTDTC = 1:3,
        EXOCCUR = c("Y", "Y", ""),
        VISIT = "UNSCHEDULED", 
        stringsAsFactors = FALSE
    )
    
    
    expect_true(check_ex_visit(EX6))
    
})

test_that("function returns true when VISIT populated with literal string 'MISSING'", {
    
    EX7 <- data.frame(
        USUBJID = 1:3,
        EXTRT = 1:3,
        EXSTDTC = 1:3,
        EXOCCUR = c("Y", "Y", "Y"),
        VISIT = "MISSING", 
        stringsAsFactors = FALSE
    )
    
    
    expect_true(check_ex_visit(EX7))
    
})


test_that("function returns false when only 1 error is present (VISIT = '')", {
    
    EX8 <- data.frame(
        USUBJID = 1:3,
        EXTRT = 1:3,
        EXSTDTC = 1:3,
        EXOCCUR = "Y",
        VISIT = c("VISIT 1", "VISIT 2", ""), 
        stringsAsFactors = FALSE
    )
    
    
    expect_false(check_ex_visit(EX8))
    
})
