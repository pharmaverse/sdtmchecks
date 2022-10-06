#' sdtmchecks: A package containing checks for common SDTM issues
#'
#'
#' @docType package
#' @name sdtmchecks
#' @description Package containing checks for common SDTM issues.
#' This package contains functions to identify common data issues in SDTM data.  
#' These checks are intended to be generalizable, actionable, and meaningful for analysis.  
#'
NULL


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



# Add day of "01" to dates that are in the format of "yyyy-mm"
impute_day01 <- function(dates) {

  ifelse(nchar(dates) ==7, paste0(dates, "-01"), dates)

}













#' @title Check if start dates are duplicated or earlier than prior visit date
#'
#' @description this is a core function for checking if start dates are
#' duplicated or earlier than last visit's (possible datetime data entry error),
#' can be used in separate checks for each domain
#'
#' @param dts dataset, e.g. EX
#' @param vars variables in a form c("USUBJID", "EXTRT", "VISITNUM", "VISIT", "EXSTDTC")
#' @param groupby variables used for grouping and visit.order derivation
#' @param dtc the date variable
#' @param ... variables used for ordering before visit.order derivation
#'
#' @export
#'
#' @return dataframe with records of duplicated or earlier than last visit date
#'
#' @author James Zhang
#'
dtc_dupl_early <- function(dts, vars, groupby, dtc, ...) {
  # dots are for ordering variables
  ### Subset to only records without missing DTC
  mydf <- dts[!is_sas_na(dts[[dtc]]) & !is_sas_na(dts[["VISIT"]]) & !is_sas_na(dts[["VISITNUM"]]) & substr(dts[["VISIT"]], 1, 5) != "UNSCH", vars]

  ### Subset no duplicated records
  mydf1 <- mydf[!duplicated(mydf[, vars]), ]

  ### Sort by
  ord <- paste0("order(", paste0("mydf1[['", list(...), "']]", collapse = ', '), ")")
  mydf2 <- mydf1[eval(parse(text = ord)), ]

  ### Add Vis_order
  splitter <- mydf2[groupby]
  mydf2l <- lapply(split(mydf2, splitter, drop = TRUE), function(x) {
    row.names(x) <- NULL
    # if 1 record then no need for lagging
    if (identical(nrow(x), as.integer(1))) {
      cbind(x, last.vis.dtc = NA, last.vis = NA, visit.order = 1, stringsAsFactors = FALSE)
      # if 2 records then just lag using the first record
    } else if (identical(nrow(x), as.integer(2))) {
      cbind(x, last.vis.dtc = c(NA, x[1, dtc]), last.vis = c(NA, x[1, "VISIT"]), visit.order = seq(1, nrow(x)), stringsAsFactors = FALSE)
      # if more than 2 records then lag and create as many records as in original
    } else {
      cbind(x, last.vis.dtc = c(NA, x[2:nrow(x) - 1, dtc]), last.vis = c(NA, x[2:nrow(x) - 1, "VISIT"]), visit.order = seq(1, nrow(x)), stringsAsFactors = FALSE)
    }
  })

  # need to stack all chunks together
  mydf2 <- Reduce(rbind, mydf2l)

  mydf2$check.flag <- ifelse(mydf2$visit.order != 1 & mydf2$last.vis.dtc == mydf2[[dtc]], "Duplicated",
                             ifelse(mydf2$visit.order != 1 & mydf2$last.vis.dtc > mydf2[[dtc]], "Datetime earlier than last Visit", NA))
  mydf2
}




#' Function to check if month is missing while year and day are non-missing
#' (i.e. would be in the format of "yyyy---dd")
#'
#' @param date date vector (character) in the format 2020-01-20
#'
#' @return vector
#' @export
missing_month <- function(date) { substr(date, 5, 7) == "---" }








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
#' convert_var_to_ascii(df, 'var')
#'
#' df <- data.frame(
#' usubjid = 1:2,
#' var = c("test", "teästõ"),
#' stringsAsFactors = FALSE
#' )
#'
#' convert_var_to_ascii(df, 'var')

convert_var_to_ascii <- function(df, var){
  Encoding(df[[var]]) <- "latin1"
  df[[var]]  <- iconv(df[[var]], "latin1", "ASCII", sub="")
  return(df)
}







#' @title Utility function to truncate data in var_name
#'
#' @description This function will truncate the strings in variables according to the length specified
#' @param dt dataset e.g. AE
#'
#' @param var_name variable name e.g. AETERM
#'
#' @param trunc_length  e.g. length the string will be truncated to e.g. 50
#'
#' @return dataset with truncated variable
#' @export
#'
#' @author Stella Banjo(HackR 2021)
#'
#' @examples
#'
#' # Testing: no truncation
#'
#' AE <- data.frame(
#'  USUBJID = 1:5,
#'  DOMAIN = "AE",
#'  AESEQ = 1:5,
#'  AESTDTC = 1:5,
#'  AETERM = 1:5,
#'  AEDECOD = 1:5,
#'  stringsAsFactors = FALSE
#' )
#'
#' truncate_var_strings(AE, var_name = "AETERM", trunc_length = 50)
#'
#' # Testing: Truncation
#'
#' AE$AETERM[4] <- "THIS IS A SUPER LONG AE TERM, SO LONG IN FACT THAT ITS OVER 50 CHARACTERS."
#' AE$AETERM[5] <- "THIS AE TERM IS WAY TOO LONG FOR A NICELY FORMATTED REPORT"
#' 
#' truncate_var_strings(AE, var_name = "AETERM", trunc_length = 50)
#'


truncate_var_strings <- function(dt, var_name, trunc_length) {
  
  dt <- mutate(dt,
               !!(var_name) := ifelse(nchar(get(var_name)) > trunc_length,
                                      unlist(lapply(get(var_name), function (x) {
                                        
                                        paste0(strwrap(x, width = (trunc_length - 3))[1], "...")
                                      })),
                                      get(var_name)
               )
  )
  
  return(dt)
}



