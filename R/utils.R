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


roche_derive_rave_row <- function(dts,domains=c("ae","cm","ds","pr")) {

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

