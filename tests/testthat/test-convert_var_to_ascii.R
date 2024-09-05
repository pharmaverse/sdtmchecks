test_that("converts to ascii", {
  df <- data.frame(
     var = c("test", "teäst"),
     stringsAsFactors = FALSE
     )

  res <- convert_var_to_ascii(df, 'var')

  answer <- data.frame(var = c("test", "test"),
                       stringsAsFactors = FALSE)

  expect_equal(res, answer)

})
