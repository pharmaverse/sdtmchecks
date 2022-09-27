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




#' @title Utility function to obtain Rave row
#'
#' @description This function derives the Rave row number from XXSPID
#'
#' @param dts dataset e.g. AE
#' @param domains domains you wish to identify a SPID variable from
#'
#' @return dataset with rave row number
#'
#' @export
#'
#' @author Stella Banjo (HackR 2021)
#'
#' @examples
#'
#' AE <- data.frame(
#'   STUDY = c(rep("1", 6)),
#'   DOMAIN = c(rep("AE", 6)),
#'   USUBJID = c(rep("PT1", 6)),
#'   AESEQ = c(1, 2, 3, 4, 5, 6),
#'   AETERM = rep("AE Raw Term",6),
#'   AEDECOD = rep("AE Preferred Term",6),
#'   AESPID = c("FORMNAME-R:13/L:13XXXX",
#'              "FORMNAME-R:16/L:16XXXX",
#'              "FORMNAME-R:2/L:2XXXX",
#'              "FORMNAME-R:19/L:19XXXX",
#'              "FORMNAME-R:5/L:5XXXX",
#'              "FORMNAME-R:20/L:20XXXX"),
#'   AESTDTC = c(rep("2020-01-01", 6))
#' )
#'
#' roche_derive_rave_row(AE)
#'


roche_derive_rave_row <- function(dts,domains=c("ae","cm","ds")) {

  myvec <- paste0(toupper(unlist(domains)), "SPID")

  thevar=intersect(names(dts), myvec) #get --SPID variable of interest

  if(length(thevar)==1) { #Only create RAVE column if there is a --SPID variable

    # Extract RAVE row number, finding the last occurrence of -R:
    RAVE_ROW <- ifelse(grepl("-R:", dts[[thevar]]),
                       sub("/.*-R:", "#", dts[[thevar]]),
                       "")
    RAVE_ROW <- sub("/.*", "", RAVE_ROW)

    # Extract RAVE form name
    RAVE_FORM <- ifelse(startsWith(as.character(dts[[thevar]]), "/F:")
                        & grepl("-D:", dts[[thevar]]),
                        sub("-D:.*", "", substring(dts[[thevar]], 4)),
                        "")

    dts[["RAVE"]] <- paste0(RAVE_FORM, RAVE_ROW)
    attr(dts[["RAVE"]], "label") <- "Rave Form"

    return(dts)

  }else(
    return(dts)
  )
}
