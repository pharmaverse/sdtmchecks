
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


roche_derive_rave_row <- function(dts,domains=c("ae","ce","cm","ds","lb","pr","rs","ss","tr","tu")) {
  
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






#' Check MedDRA version in TS and return dataframe with covid terms according to this version
#'
#' @description This function reads in a dataset with covid terms according to MedDRA version found in TS
#'              This function assumes you have a folder called "data" that contains .Rdata files with file names indexed
#'              by medDRA version, e.g. covid230.Rdata, covid231.Rdata, covid240.Rdata,etc.  It also assumes a pan
#'              MedDRA version data/covid.Rdata file which it will use if the specific version cant be found.
#' @return dataframe
#' @param TS Trial Summary SDTM dataset (optional) with variables TSPARMCD, TSVAL
#' @export
#' @keywords internal
#' @examples
#'
#'\dontrun{
#' #First COVID dataset is from v23.0, so use the latest
#' ts1 <- data.frame(TSPARMCD = "AEDICT", TSVAL = c("MedDRA 22.0"))
#' roche_covid_df(TS=ts1)
#'
#' #COVID datset chosen regardless of the 'v' precending version number
#' ts2 <- data.frame(TSPARMCD = "AEDICT", TSVAL = c("MedDRA v23.0"))
#' roche_covid_df(TS=ts2)
#' 
#' ts2a <- data.frame(TSPARMCD = "AEDICT", TSVAL = c("MedDRA 23.0"))
#' roche_covid_df(TS=ts2a)
#'
#' #Missing version default to use the latest available
#' ts3 <- data.frame(TSPARMCD = "AEDICT", TSVAL = "")
#' roche_covid_df(TS=ts3)
#'
#' #Future version default to use the latest available
#' ts4 <- data.frame(TSPARMCD = "AEDICT", TSVAL = c("MedDRA v40.1"))
#' roche_covid_df(TS=ts4)
#' }
#' 


roche_covid_df <- function(TS=NULL){
  
  # Check if there is ts domain in data pack
  if (!is.null(TS)) {
    
    # Keep only TSVAL and TSVALn for TSPARAMCD = "AEDICT"
    tsval <- subset(TS, TS$TSPARMCD=="AEDICT", "TSVAL")
    
    # Check if there is TSPARAMCD = "AEDICT" in ts domain
    if (nrow(tsval) > 0) {
      versions <- as.vector(tsval[[1]])
      
      condition <- ifelse(length(versions) == 1, TRUE, all(versions[1] == versions))
      # Check if there are more than one record with TSPARAMCD = "AEDICT" and if there are equal versions of MedDRA in TSVAL values
      if (condition) {
        # Check if all TSVAL values are missing
        if (is_sas_na(versions[1])) {
          covid_df_name <- 'covid'
        } else {
          version <- versions[1]
          v_n <- as.numeric(unlist(regmatches(version, gregexpr("[0-9]+", version))))
          version_n <- as.numeric(paste0(v_n[1], v_n[2]))
          
          # Available MedDRA versions with COVID baskets
          covid_dt <- list.files(path = "data/", pattern = "covid")
          covid_n <- as.numeric(unlist(regmatches(covid_dt, gregexpr("[0-9]+", covid_dt))))
          
          # Check if MedDRA version is one of available list
          if (version_n %in% covid_n) {
            covid_df_name <- paste0('covid',version_n)
          } else {
            covid_df_name <- 'covid'
          }
        }
      }
    } else {
      covid_df_name <- 'covid'
    }
  } else {
    covid_df_name <- 'covid'
  }
  
    return(get(covid_df_name))
}
