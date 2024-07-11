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
#' With fail message and dataframe portion responsible for the check failure result
#'
#'
#' @param msg character string with fail message
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
#' @description This is a core function for checking if start dates are
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
#' @keywords internal utils_rpt
#' 
#' @family utils_rpt
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
#'
#' @author Stella Banjo(HackR 2021)
#' @export
#' @keywords internal utils_rpt
#' @family utils_rpt
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






#' @title Save report as an xlsx file
#'
#' @param res results list created by run_all_checks
#' @param outfile file path/name to write to
#' @param extrastring optionally display extra info alongside version info, e.g. diff info
#' 
#' @import openxlsx
#' @importFrom utils packageDescription
#' @importFrom tidyselect any_of
#'
#' @return xlsx file
#' @export
#' 
#' @family ex_rpt
#' @keywords internal ex_rpt
#'
#' @examples
#' 
#' # Create Dummy data
#' 
#' ae = data.frame(
#'  USUBJID = 1:5,
#'  DOMAIN = c(rep("AE", 5)),
#'  AESEQ = 1:5,
#'  AESTDTC = 1:5,
#'  AETERM = 1:5,
#'  AEDECOD = 1:5,
#'   AESPID = c("FORMNAME-R:13/L:13XXXX",
#'              "FORMNAME-R:16/L:16XXXX",
#'              "FORMNAME-R:2/L:2XXXX",
#'              "FORMNAME-R:19/L:19XXXX",
#'              "FORMNAME-R:5/L:5XXXX"),
#'  stringsAsFactors = FALSE
#' )
#' 
#' cm = data.frame(
#'  USUBJID = 1:5,
#'  DOMAIN = rep("CM", 5),
#'  CMTRT = rep("DRUG TERM", 5),
#'  CMDECOD = rep("CODED DRUG TERM", 5),
#'  CMSTDTC = 1:5,
#'  CMENDTC = 1:5,
#'  CMCAT = "CONCOMITANT MEDICATIONS",
#'  CMSPID = c("FORMNAME-R:13/L:13XXXX",
#'              "FORMNAME-R:16/L:16XXXX",
#'              "FORMNAME-R:2/L:2XXXX",
#'              "FORMNAME-R:19/L:19XXXX",
#'              "FORMNAME-R:5/L:5XXXX"),
#'  stringsAsFactors=FALSE
#' )
#' 
#' res=run_all_checks(verbose = FALSE)
#' fileName <- file.path(tempdir(), "check_results.xlsx")
#' report_to_xlsx(res=res,outfile=fileName)
#' 


report_to_xlsx = function(res,outfile,extrastring=""){
  
  # prepare summary page
  # pull columns (xls_title, pdf_title, nrec, notes) from the list and create a summary data frame
  summary_cols<-lapply(res,'[', c("xls_title","pdf_title","nrec","notes","pdf_subtitle"))
  summary_data_0<-as.data.frame(do.call(rbind,summary_cols)) 
  summary_data = summary_data_0 %>% 
    mutate(version="") %>% select(-any_of("pdf_subtitle"))
  summary_data[,"nrec"]<-as.numeric(summary_data[,"nrec"])
  summary_data[1,"version"]<-nickname
  summary_data[2,"version"]<-extrastring
  
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
  conditionalFormatting(wb, "Summary results", cols=5 ,  rows=1:3, rule='!=""', style=boldnickname)
  
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
#' @param verbose Print information to console
#'
#' @return R script with user specified sdtmchecks based on sdtmchecksmeta file
#'
#' @export
#'
#' @importFrom dplyr %>% mutate row_number
#'
#' @author Monarch Shah
#' 
#' @keywords utils_rpt
#' 
#' @family utils_rpt
#'
#' @examples
#' 
#' 
#' # All checks are output to a file
#' fileName <- file.path(tempdir(), "run_all_checks.R")
#' create_R_script(file = fileName)
#' 
#' # Only include selected checks
#' fileName <- file.path(tempdir(), "run_some_checks.R")
#' mymetads = sdtmchecksmeta[sdtmchecksmeta$category == "ALL" & sdtmchecksmeta$priority == "High",]
#' create_R_script(metads = mymetads, file = fileName)
#' 
#' # Roche specific function calls
#' fileName <- file.path(tempdir(), "run_all_checks_roche.R")
#' mymetads = sdtmchecksmeta
#' mymetads$fxn_in=mymetads$fxn_in_roche
#' create_R_script(metads = mymetads, file = fileName)
#' 

create_R_script <- function(metads=sdtmchecksmeta, file="sdtmchecks_run_all.R",verbose=TRUE) {
  
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
  if(verbose){cat("sdtmchecks calls R script written here:", file)}
  writeLines(write_this, fileConn)
  close(fileConn)
  
  return(invisible())
  
}






#' @title Create a sdtmchecks list object with column indicating whether the issue was previously seen
#'
#' @description This report will identify flagged records from an sdtmchecks report
#' that are "new" and those that are "old" for a study. This will help quickly target 
#' newly emergent issues that may require a new query or investigation while indicating 
#' issues that were encountered from a prior report and may have already been queried.
#' 
#' This `diff_reports()` function requires a newer and older set of results from 
#' `sdtmchecks::run_all_checks()`, which will generate a list of check results. 
#' An added column "Status" is created with values of "NEW" and "OLD" 
#' in the list of check results, flagging whether a given record that is present 
#' in the new result (ie `new_report`) is also present in the old result (ie `old_report`).
#' It makes a difference which report is defined as "new" and "old". 
#' This code only keeps results flagged in the new report and drops 
#' old results not in the new report because they were presumably resolved.
#'
#' @param old_report an older sdtmchecks list object as created by `run_all_checks`
#' @param new_report a newer sdtmchecks list object as created by `run_all_checks`
#'
#' @return list of sdtmchecks results based on new_report with Status indicator
#' 
#' @importFrom dplyr %>% left_join mutate
#' @export
#' 
#' @examples 
#' 
#' # Step 1: Simulate an older AE dataset with one missing preferred term
#' 
#'  ae <- data.frame(
#'  USUBJID = 1:5,
#'  DOMAIN = c(rep("AE", 5)),
#'  AESEQ = 1:5,
#'  AESTDTC = 1:5,
#'  AETERM = 1:5,
#'  AEDECOD = 1:5,
#'   AESPID = c("FORMNAME-R:13/L:13XXXX",
#'              "FORMNAME-R:16/L:16XXXX",
#'              "FORMNAME-R:2/L:2XXXX",
#'              "FORMNAME-R:19/L:19XXXX",
#'              "FORMNAME-R:5/L:5XXXX"),
#'  stringsAsFactors = FALSE
#' )
#' 
#' ae$AEDECOD[1] = NA
#' 
#' # Step 2: Use the run_all_checks() function to generate list of check results on this "old" data
#'  
#' # Filter sdtmchecksmeta so that only one check is present
#' metads <- sdtmchecksmeta[sdtmchecksmeta$check=="check_ae_aedecod",] 
#' old <- run_all_checks(metads=metads)
#' 
#' 
#' 
#' #Step 3: Simulate a newer, updated AE dataset with another record with a new missing preferred term
#' 
#'  ae <- data.frame(
#'  USUBJID = 1:6,
#'  DOMAIN = c(rep("AE", 6)),
#'  AESEQ = 1:6,
#'  AESTDTC = 1:6,
#'  AETERM = 1:6,
#'  AEDECOD = 1:6,
#'   AESPID = c("FORMNAME-R:13/L:13XXXX",
#'              "FORMNAME-R:16/L:16XXXX",
#'              "FORMNAME-R:2/L:2XXXX",
#'              "FORMNAME-R:19/L:19XXXX",
#'              "FORMNAME-R:5/L:5XXXX",
#'              "FORMNAME-R:1/L:5XXXX"
#'              ),
#'  stringsAsFactors = FALSE
#' )
#' 
#' ae$AEDECOD[1] = NA
#' ae$AEDECOD[6] = NA
#' 
#' # Step 4: use the run_all_checks() function to generate list of check results  on this "new" data
#' 
#' new <- run_all_checks(metads=metads)
#' 
#' # Step 5: Diff to create a column indicating if the finding is new
#' res <- diff_reports(old_report=old, new_report=new)
#' 
#' ## optionally output results as spreadsheet with sdtmchecks::report_to_xlsx()
#' # report_to_xlsx(res, outfile=paste0("saved_reports/sdtmchecks_diff_",Sys.Date(),".xlsx"))
#' 
#'
#' @keywords ex_rpt
#' @family ex_rpt
#' 

diff_reports=function(old_report,new_report){
  
  # it makes a difference which report is defined as "new_report" and "old_report"
  # this code only keeps results flagged in the new report
  # it ignore old results not in new report (because they were resolved)
  
  if(!is.list(old_report)|!is.list(new_report)){
    
    stop("Inputs are expected to be lists as created by sdtmchecks::run_all_checks")
    
  }else{
    
  
  ###
  # First: subset to only results with flagged issues in the new report
  ###
  
  new_issues=sapply(names(new_report),function(check_name){
    if("data" %in% names(new_report[[check_name]])){#if the check has a "data" attributes
      if(nrow(new_report[[check_name]]$data)>0){ #TRUE if data has any records
        TRUE
      }else{ #FALSE if data exists but no records
        FALSE
      }
    }else{ #FALSE if no data attributes
      FALSE
    }
  },USE.NAMES = TRUE)
  
  new_issues=names(new_issues[new_issues==TRUE]) #filter to just flagged records
  new_report=new_report[new_issues] #subset new report to just flagged records
  
  ### -------------------------
  # Second: Do the diff 
  #
  #    i.e., Compare the flagged records in the new vs. old report.
  #          A new column "Status" will be added to all results of the 
  #          "new_report" based on the flagged record comparison.
  #          The new column will have either "NEW" or "OLD" populated.
  ### -------------------------
  
  res=sapply(new_issues,function(check_name){
    
    if(!(check_name %in% names(old_report))){ #if check not in old report then these issues are new
      
      res_new=new_report[[check_name]]
      res_new$data$Status="NEW"
      res_new
      
    }else if(nrow(old_report[[check_name]]$data)==0){ 
      #if check in the old report but old report didn't have any issues then these issues are new
      
      res_new=new_report[[check_name]]
      res_new$data$Status="NEW"
      res_new
      
    }else{ #else both old and new report have some issues flagged, so we diff them
      
      res_new=new_report[[check_name]]
      res_old=old_report[[check_name]]
      res_old$data$Status="OLD"
      
      res_new$data=res_new$data %>%
        left_join(res_old$data,relationship = "many-to-many") %>% #behold the magic of dplyr automatically identifying columns to join on
        mutate(Status=ifelse(is.na(Status),"NEW",Status))
      
      res_new
    }
    
  },USE.NAMES = TRUE,simplify=FALSE)
  
  return(res)
  
  }
  
}

