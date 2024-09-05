test_that("truncation of strings is less than or equal specified length", {
  
# Testing: no truncation

AE <- data.frame(
  USUBJID = 1:5,
  DOMAIN = "AE",
  AESEQ = 1:5,
  AESTDTC = 1:5,
  AETERM = 1:5,
  AEDECOD = 1:5,
  stringsAsFactors = FALSE
)

#truncate_var_strings(AE, var_name = "AETERM", trunc_length = 50)

# Testing: Truncation

AE$AETERM[4] <- "THIS IS A SUPER LONG AE TERM, SO LONG IN FACT THAT ITS OVER 50 CHARACTERS."
AE$AETERM[5] <- "THIS AE TERM IS WAY TOO LONG FOR A NICELY FORMATTED REPORT"

# Storing updated df with truncated strings
a <- truncate_var_strings(AE, var_name = "AETERM", trunc_length = 50)

# Assigning values
var_name = a$AETERM
nchar(var_name)
trunc_length = 50
# Logical vector that checks each column value
b <- trunc_length >= nchar(var_name)


expect_true(all(b))
})

