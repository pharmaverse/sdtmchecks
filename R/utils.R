#' Pass object
#'
#' @export
#'
#' @return a boolean TRUE
pass <- function() {
  TRUE
}


#' Fail object
#'
#' With Message and part of the Data that is responsible for the check to fail
#'
#'
#' @param msg character strings with fail message
#' @param data a data frame to be printed with the fail message
#'
#' @export
#'
#' @return Boolean with msg and data attributes
#'
fail <- function(msg, data = NULL) {
  structure(FALSE, msg = msg, data = data)
}




#' Check if the elements could be of any SAS missing data variant
#'
#' Missing Data from sas7bdat imported datesets could be imported in different
#' ways we currently check for 'NA', NA, '.', and ''.
#'
#' @param x vector with data
#'
#' @return logical vector
#'
#' @export
#'
#' @examples
#' is_sas_na(c(1,2,NA))
#'
#' is_sas_na(c("a", NA, "NA", ""))
is_sas_na <- function(x) {
  x <- trimws(x)
  is.na(x) |
    vapply(x, function(xi) {identical(xi, "") | identical(xi, "NA") | identical(xi, ".")}, logical(1))
}


#' Check if data frame is missing any of the specified variables
#'
#' @param df a data frame
#' @param varnames a vector with variable names
#'
#' @return boolean
#'
#' @export
#'
`%lacks_all%` <- function(df, varnames) {
  all(!(varnames %in% names(df)))
}

#' Check if data frame has at least one variable from a set of specified variables
#'
#' @inheritParams %lacks_all%
#' @return boolean
#'
#' @export
#'
`%lacks_any%` <- function(df, varnames) {
  any(!(varnames %in% names(df)))
}

#' Check data frame for mandatory variables and specify which are missing in
#' message
#'
#' @inheritParams %lacks_all%
#' @return character string saying which variables are missing
#' @export
lacks_msg <- function(df, varnames) {
  data_name <- deparse(substitute(df))

  lacking <- varnames[!(varnames %in% names(df))]

  if (length(lacking) == 0) {
    paste(data_name, "is not missing any variable.")
  } else if (length(lacking) == 1) {
    paste(data_name, "is missing the variable:", lacking)
  } else {
    paste(data_name, "is missing the variables:", paste(lacking, collapse = ", "))
  }
}


#' Check if data frame has all specified variables
#'
#' @inheritParams %lacks_all%
#' @return boolean
#' @export
`%has_all%` <- function(df, varnames) {
  all(varnames %in% names(df))
}

#' Check if data frame has at least one of the specified variables
#' @inheritParams %lacks_all%
#' @return boolean
#' @export
`%has_any%` <- function(df, varnames) {
  any(varnames %in% names(df))
}
