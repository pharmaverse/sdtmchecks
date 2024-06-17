
#' @title Puts into environment a list of toy datasets for testing. 
#'
#' @description Ad-hoc named list of toy dataframes first used for testing each check function and copied here.  List is structured first by name of check function and then 0, 1 or more dataframes.  Before use, the list must be unnested and assembled as required.

#'
#'
#' @param 
#' @param 
#' @param 
#'
#' @return Nested list of dataframes.
#'
#' @export
#'
#' @importFrom 
#'
#' @author jim rothstein [14JUNE2024]  
#' 
#' @family 
#' 
#' @keywords 
#'
#' @examples
#' toy_datasets = check_toy_datasets()
#'

check_toy_datasets = function() {

  #pass
   AE <- data.frame(
    USUBJID = 1:7,
    AETERM = 1:7,
    AESTDTC = 1:7,
    AEACNOTH = 1:7,
    AEACNOT1 = 1:7,
    AEACNOT2 = 1:7,
    AESPID = "FORMNAME-R:13/L:13XXXX"
   )
  
    #fail
 AE1 =  AE
   AE1$AEACNOTH[1] = ""
   AE1$AEACNOT1[1] = ""
   AE1$AEACNOT2[1] = ""
   AE1$AEACNOTH[2] = "MULTIPLE"
   AE1$AEACNOT1[2] = "DOSE REDUCED"
   AE1$AEACNOT2[2] = "DRUG WITHDRAWN"
   AE1$AEACNOTH[3] = "MULTIPLE"
   AE1$AEACNOT1[3] = "DOSE REDUCED"
   AE1$AEACNOT2[3] = ""
   AE1$AEACNOTH[4] = "MULTIPLE"
   AE1$AEACNOT1[4] = ""
   AE1$AEACNOT2[4] = "DRUG WITHDRAWN"
   AE1$AEACNOTH[5] = "MULTIPLE"
   AE1$AEACNOT1[5] = ""
   AE1$AEACNOT2[5] = ""
   
   
    AE2 = AE1
   AE2$AEACNOTH[1] = NA
   AE2$AEACNOT1[1] = NA
   AE2$AEACNOT2[1] = NA
   AE2$AEACNOT2[3] = NA 
   AE2$AEACNOT1[4] = NA 
   AE2$AEACNOT1[5] = NA
   AE2$AEACNOT2[5] = NA

    ### nest

L = list (
    check_ae_aeacnoth= list(
        pass = list(one=AE),
        fail = list(one=AE1, two=AE2)
        )
    )
}

L = check_toy_datasets()
    
identical(L[[c("check_ae_aeacnoth", "pass","one")]], L$check_ae_aeacnoth$pass$one)
