
#' @title Utility function to obtain Rave form name and row number 
#'
#' @description This function derives the Rave form name and row number from xxSPID. 
#' The xxSPID string may yield unexpected results for outsourced studies. Log forms 
#' will show the row number as #n. Non-log forms may show #0 after the form name.
#'
#' @param dts SDTM dataframe - e.g., AE
#' @param domains domains you wish to identify a xxSPID variable from
#'
#' @return dataframe with Rave row number
#'
#'
#' @author Stella Banjo (HackR 2021)
#' @export
#' @keywords internal
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
#'   AESTDTC = c(rep("2020-01-01", 6)),
#'   stringsAsFactors = FALSE
#' )
#'
#' roche_derive_rave_row(AE)
#'



roche_derive_rave_row <- function(dts,domains=c("ae","ce","cm","ds","lb","mh","pr","rs","ss","tr","tu")) {

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


