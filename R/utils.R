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
#'
#' @return a boolean TRUE
#' @export
#' @keywords internal
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
#'
#' @return Boolean with msg and data attributes
#' @export
#' @keywords internal
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
#' @keywords internal
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
#' @export
#' @keywords internal
#'
`%lacks_all%` <- function(df, varnames) {
  all(!(varnames %in% names(df)))
}

#' Check if data frame has at least one variable from a set of specified variables
#'
#' @inheritParams %lacks_all%
#' @return boolean
#' @export
#' @keywords internal
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
#' @keywords internal

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
#' @keywords internal

`%has_all%` <- function(df, varnames) {
  all(varnames %in% names(df))
}

#' Check if data frame has at least one of the specified variables
#' @inheritParams %lacks_all%
#' @return boolean
#' @export
#' @keywords internal

`%has_any%` <- function(df, varnames) {
  any(varnames %in% names(df))
}



#'
#' Add day of "01" to dates that are in the format of "yyyy-mm"
#' @return string
#' @export
#' @keywords internal

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
#'
#' @return dataframe with records of duplicated or earlier than last visit date
#'
#' @author James Zhang
#' @export
#' @keywords internal
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
#' @keywords internal

missing_month <- function(date) { substr(date, 5, 7) == "---" }








#' Remove non-ASCII characters from reported term in order
#' for Pandoc to create PDF file
#'
#'
#' @param df dataframe
#' @param var variable with non-ASCII characters
#'
#' @return dataframe
#' @export
#' @keywords internal
#'
#' @examples
#'
#' df <- data.frame(
#' var = c("test", "te??st"),
#' stringsAsFactors = FALSE
#' )
#'
#' convert_var_to_ascii(df, 'var')
#'
#' df <- data.frame(
#' usubjid = 1:2,
#' var = c("test", "te??st??"),
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
#'
#' @author Stella Banjo(HackR 2021)
#' @export
#' @keywords internal
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


#' Remove non-ASCII characters from reported term in order
#' for Pandoc to create PDF file
#'
#'
#' @param df dataframe
#' @param var variable with non-ASCII characters
#'
#' @return dataframe
#' @export
#' @keywords internal
#'
#' @examples
#'
#' df <- data.frame(
#' var = c("test", "te??st"),
#' stringsAsFactors = FALSE
#' )
#'
#' df <- convert_var_to_ascii(df, 'var')
#'
#' df <- data.frame(
#' usubjid = 1:2,
#' var = c("test", "te??st??"),
#' stringsAsFactors = FALSE
#' )
#'
#' df <- convert_var_to_ascii(df, 'var')

convert_var_to_ascii <- function(df, var){
  Encoding(df[[var]]) <- "latin1"
  df[[var]]  <- iconv(df[[var]], "latin1", "ASCII", sub="")
  return(df)
}









#' Save report as an xlsx file
#'
#'
#' @param res results list created by run_all_checks
#' @param outfile file path/name to write to
#' 
#' @import openxlsx
#' @importFrom utils packageDescription
#' @importFrom tidyselect any_of
#'
#' @return xlsx file
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' 
#' ae = haven::read_sas("path/to/ae.sas7bdat")
#' cm = haven::read_sas("path/to/cm.sas7bdat")
#' dm = haven::read_sas("path/to/dm.sas7bdat")
#' 
#' all_rec=run_all_checks(verbose = TRUE)
#' 
#' report_to_xlsx(res=all_rec,outfile="check_results.xlsx")
#' 
#' 
#' }
#' 
#' 
report_to_xlsx = function(res,outfile){

# prepare summary page
# pull columns (xls_title, pdf_title, nrec, notes) from the list and create a summary data frame
summary_cols<-lapply(res,'[', c("xls_title","pdf_title","nrec","notes","pdf_subtitle"))
summary_data_0<-as.data.frame(do.call(rbind,summary_cols)) 
summary_data = summary_data_0 %>% 
  mutate(version="") %>% select(-any_of("pdf_subtitle"))
summary_data[,"nrec"]<-as.numeric(summary_data[,"nrec"])
summary_data[1,"version"]<-nickname

# assign column names
colnames(summary_data)<-c("Data check (Tab name)",
                          "Description", 
                          "N of Failed records", 
                          "Notes",
                          paste0("sdtmchecks v.",packageDescription("sdtmchecks")[["Version"]])
)

# create workbook
wb<-createWorkbook()

# add some formatting to summary page
addWorksheet(wb, "Summary results")

setColWidths(wb, "Summary results", cols=1, widths=30)
setColWidths(wb, "Summary results", cols=2, widths=65)
setColWidths(wb, "Summary results", cols=3, widths=20)
setColWidths(wb, "Summary results", cols=4, widths=35)
setColWidths(wb, "Summary results", cols=5, widths=25)

addFilter(wb, "Summary results", cols=1:ncol(summary_data), rows=1 )

# write summary data on the 1st page of XLS file
writeData(wb, "Summary results", as.data.frame(summary_data), startRow = 1, startCol = 1, headerStyle=createStyle(textDecoration = "bold"))

# Highlight the rows with problematic queries ( i.e. have non-missing comments at column D)
redStyle<-createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
orangeStyle<-createStyle(fontColour = "#000000", bgFill = "#fac966")
boldnickname<-createStyle(textDecoration = "bold")

conditionalFormatting(wb, "Summary results", cols=1:4 ,  rows=1:nrow(summary_data)+1, rule='$D2!=" "', style=redStyle)
conditionalFormatting(wb, "Summary results", cols=2:4 ,  rows=1:nrow(summary_data)+1, rule='$C2>0', style=orangeStyle)
conditionalFormatting(wb, "Summary results", cols=1 ,  rows=1:nrow(summary_data)+1, rule='$C2>0', style=orangeStyle)
conditionalFormatting(wb, "Summary results", cols=5 ,  rows=2, rule='$E2!=""', style=boldnickname)

# Add comments with PDF subtitles to summary results page
for(i in 1:nrow(summary_data_0)){
  
  writeComment(wb, "Summary results", col=2, row=i+1,
               comment=createComment(
                 unlist(summary_data_0[i,"pdf_subtitle"]),
                 author = "sdtmchecks",
                 visible = FALSE,
                 width = 2,
                 height = 4
               ))
  
}

#loop through the data checks results and write them into separated sheet in xls file.
for (i in 1:length(res)){
  
  # do not create xls sheet for data checks with 0 results
  if(res[[i]]$nrec != 0) {
    
    addWorksheet(wb, res[[i]]$xls_title)
    
    
    # Begin writing individual xls tab at row 2.
    # Row=1 will be used to create a HYPERLINK back to 'Summary results' sheet.
    # writeData(wb, res[[i]]$xls_title, as.data.frame(res[[i]]$data), startRow = 2, startCol = 1)
    writeData(wb, res[[i]]$xls_title, as.data.frame(res[[i]]$data), startRow = 1, startCol = 1)
    
    # create a HYPERLINK between a row on 'Summary results' sheet and individual tab
    # need to have i+1 because the 1st row on 'Summary results' sheet has column names
    
    # writeFormula(wb, sheet="Summary results", startRow=i+1, startCol=1,
    #              x=makeHyperlinkString(sheet=res[[i]]$xls_title, row=1, col=1, text=res[[i]]$xls_title ))
    
    writeData(wb, sheet="Summary results", startRow=i+1, startCol=1,
              x=res[[i]]$xls_title )
    
    
    # create a HYPERLINK between an individual tab and the row on 'Summary results' sheet
    # need to have i+1 because the 1st row on 'Summary results' sheet has column names
    # writeFormula(wb, sheet=res[[i]]$xls_title, startRow=1,
    #              x=makeHyperlinkString(sheet="Summary results", row=i+1, col=5, text="Link to Summary Tab" ))
    
    # Add comments with PDF sub titles to each individual page
    writeComment(wb, res[[i]]$xls_title, col=1, row=1,
                 comment=createComment(
                   unlist(summary_data_0[i,"pdf_subtitle"]),
                   author = "sdtmchecks",
                   visible = FALSE,
                   width = 2,
                   height = 4
                 ))
    
    
  } # end of if
} # end of loop

saveWorkbook(wb, file = outfile, overwrite = TRUE)
return(invisible())

}






#' @title Create .R file with sdtmchecks function calls
#'
#' @description Function that uses sdtmchecksmeta as input and creates .R file with function calls
#'
#' @param metads sdtmchecksmeta file
#' @param file filename and/or path to save to
#'
#' @return R script with user specified sdtmchecks based on sdtmchecksmeta file
#'
#' @export
#'
#' @importFrom dplyr %>% mutate row_number
#'
#' @author Monarch Shah
#'
#' @examples
#' 
#' \dontrun{
#'
#' create_R_script(file = "run_the_checks.R")
#' 
#' # Only include selected checks
#' mymetads = sdtmchecksmeta %>% 
#' filter(category == "ALL" & priority == "High")
#' 
#' create_R_script(metads = mymetads, file = "run_the_checks.R")
#' 
#' #Roche specific function calls
#' mymetads = sdtmchecksmeta %>% 
#' mutate(fxn_in=fxn_in_roche)
#' 
#' create_R_script(metads = mymetads, file = "run_the_checks.R")
#' 
#' }

create_R_script <- function(metads=sdtmchecksmeta, file="sdtmchecks_run_all.R") {
  
  filterchecks <- metads %>%
    mutate(
      n=row_number(),
      check_args = paste0(check, '(', fxn_in, ')',ifelse(n!=nrow(metads),",",""))
    )
  
  write_this <-
    c(
      "# load packages",
      "library(sdtmchecks)",
      "library(dplyr)",
      "",
      "# Read your SDTM Domains, e.g.:",
      "# dm = haven::read_sas('path/to/sdtms/dm.sas7bdat')",
      "# ae = haven::read_sas('path/to/sdtms/ae.sas7bdat')",
      "",
      "# Run selected checks",
      "res = list(",
      paste("  ",eval(filterchecks$check_args)),
      ")",
      "",
      "# Write results to an excel file",
      "report_to_xlsx(res=res,outfile='check_results.xlsx')"
    )
  
  fileConn <- file(file)
  cat("sdtmchecks calls R script written here:", file)
  writeLines(write_this, fileConn)
  close(fileConn)
  
  return(invisible())
  
}

