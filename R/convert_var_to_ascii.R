#' Remove non-ASCII characters from reported term in order
#' for Pandoc to create PDF file
#'
#'
#' @param df dataframe
#' @param var variable with non-ASCII characters
#'
#' @return dataframe
#'
#' @export
#'
#' @examples
#'
#' df <- data.frame(
#' var = c("test", "teäst"),
#' stringsAsFactors = FALSE
#' )
#'
#' df <- convert_var_to_ascii(df, 'var')
#'
#' df <- data.frame(
#' usubjid = 1:2,
#' var = c("test", "teästõ"),
#' stringsAsFactors = FALSE
#' )
#'
#' df <- convert_var_to_ascii(df, 'var')

convert_var_to_ascii <- function(df, var){
    Encoding(df[[var]]) <- "latin1"
    df[[var]]  <- iconv(df[[var]], "latin1", "ASCII", sub="")
    return(df)
}
