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
