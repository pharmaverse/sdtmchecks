#' @title Run all data checks in sdtmchecks package using parallel processing
#'
#' @description This function runs all checks in the sdtmchecks package.  
#' It expects SDTM domains saved as dataframe objects in your global environment.  
#' These dataframes should have lowercase names, e.g., dm.
#'
#' @param type  Type of data checks, i.e., c("ALL", "ONC", "COV", "PRO", "OPHTH"). 
#' NULL runs all type.
#'
#' @param priority Priority level of data checks, i.e., c("High", "Medium", "Low"). 
#' NULL runs all priority levels.
#'
#' @param verbose Whether to display messages while running
#'
#' @param metads Metadata to use to execute the checks. The default is the 
#' sdtmchecksmeta dataframe available in the package. This object could 
#' easily be customized, subset, etc.
#'
#' @param ncores Number of cores for parallel processing, with default set 
#' to 1 (sequential)
#'
#' @details To look up documentation for the data checks in package, please use 
#' command ??sdtmchecks
#'
#' @return list with results from individual data check functions
#'
#' @importFrom parallel mcmapply
#' 
#' @family ex_rpt
#' 
#' @keywords ex_rpt
#'
#' @export
#'
#' @examples
#'
#' # Assuming sdtm datasets are in your global environment
#' # Note we are only specifying AE and DS here so all unrelated checks wont be run
#' 
#' ae <- data.frame(
#'     STUDYID = 1,
#'     USUBJID = c(1,2,3,1,2,3),
#'     AESTDTC = '2020-05-05',
#'     AETERM  = c("abc Covid-19", "covid TEST POSITIVE",rep("other AE",4)),
#'     AEDECOD = c("COVID-19", "CORONAVIRUS POSITIVE", rep("OTHER AE",4)),
#'     AEACN = c("DRUG WITHDRAWN", rep("DOSE NOT CHANGED",5)),
#'     AESPID = "FORMNAME-R:13/L:13XXXX",
#'     stringsAsFactors = FALSE
#' )
#'
#' ds <- data.frame(
#'  USUBJID = c(1,1,2,3,4),
#'  DSSPID  = 'XXX-DISCTX-XXX',
#'  DSCAT   = "DISPOSITION EVENT",
#'  DSDECOD = "OTHER REASON",
#'  DSSEQ = c(1,2,1,1,1),
#'  stringsAsFactors = FALSE
#' )
#' 
#' all_rec<-run_all_checks(metads=sdtmchecksmeta, 
#'                         verbose=FALSE)
#'                         

run_all_checks <- function(metads = sdtmchecksmeta,
                           priority = c("High", "Medium", "Low"),
                           type = c("ALL", "ONC", "COVID", "PRO", "OPHTH"),
                           verbose = TRUE,
                           ncores = 1) {
  
  if (!is.null(priority) & !all(priority %in% c("High", "Medium", "Low"))) {
    stop("priority argument should only take values 'High','Medium', or 'Low'")
  }
  if (!is.null(type) & !all(type %in% c("ALL", "ONC", "COVID", "PRO", "OPHTH"))) {
    stop("type argument should only take values 'ALL', 'ONC', 'COVID', 'PRO', 'OPHTH'")
  }
  
  # subset meta dataset for specific Type and Priority
  if (!is.null(priority)) {
    metads <-subset(metads,metads$priority %in% priority)
  }
  
  if (!is.null(type) ) {
    metads <-subset(metads, metads$category %in% type)
  }
  
  #Identify missing expected domains to cat out to the user later
  expected.domains = sort(unique(strsplit(gsub(" ", "", paste(unique(metads$domains),
                                                              collapse=",")), ",")[[1]]))
  missing.domains = expected.domains[!(expected.domains %in% ls(envir = .GlobalEnv))]
  
  # Run checks
  
  all_rec <- mcmapply(
    FUN = run_check,
    metads$check, metads$fxn_in, metads$xls_title, metads$pdf_title, metads$pdf_subtitle, metads$pdf_return, 
    verbose,
    SIMPLIFY = FALSE,  # forces mcmapply to return a list
    USE.NAMES = TRUE,  # generates list names
    mc.cores = ncores
  )
  
  if (verbose == TRUE & length(missing.domains) > 0) {
    cat(
      paste0(
        "\nDomain not found: ",
        missing.domains,
        ". Checks requiring this domain were not run."
      )
    )
  }
  
  cat("\n")
  
  # return the final list
  return(all_rec)
  
} # end of function