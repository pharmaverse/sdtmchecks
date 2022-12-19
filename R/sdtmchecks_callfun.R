#' @title Create a function that creates .R file with sdtmchecks function calls
#'
#' @description Function that uses sdtmchecksmeta as input and creates .R file with function calls
#'
#' @param sdtmcheckmeta sdtmchecksmeta file
#' @param outfileloc location where user want to save the .R file
#'
#' @return R script with user specified sdtmchecks based on sdtmchecksmeta file
#'
#' @export
#'
#' @importFrom dplyr %>% mutate
#'
#' @author Monarch Shah
#'
#' @examples
#'
#' sdtmchecks_callfun(sdtmcheckmeta = sdtmchecksmeta, outfileloc = paste0("~/sdtmchecks/") )
#'
#' sdtmchecks_callfun(sdtmcheckmeta = sdtmchecksmeta %>% filter(category == "ALL") %>% filter(priority == "High"), outfileloc = paste0("~/sdtmchecks/") )

sdtmchecks_callfun <- function(sdtmcheckmeta, outfileloc) {
  
  filterchecks <- sdtmcheckmeta %>%
    mutate(check_args = paste0(check, '(', fxn_in, ')'))
  
  write_this <-
    c(
      "# load packages",
      "library(sdtmchecks)",
      "library(dplyr)",
      "",
      "# Read your SDTM Domains",
      "# haven::read_sas('path/to/sdtms/ae.sas7bdat')",
      "",
      "# sdtmchecks",
      eval(filterchecks$check_args)
    )
  
  fileConn <- file(paste0(outfileloc, "sdtmchecks_calls.R"))
  
  cat("sdtmchecks calls R script is located", outfileloc,  "as", "sdtmchecks_calls.R")
  
  writeLines(write_this, fileConn)
  
  close(fileConn)
  
}
