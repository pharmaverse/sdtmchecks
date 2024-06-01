#' @title Run a single check in sdtmchecks package
#'
#' @description This function runs a single check in the sdtmchecks package. It expects a check name,
#' the function that performs the check and some info for the pdf and Excel files. It also expects a T/F
#' value that determines whether to display messages while running. Excluding verbose, the parameters for
#' this function are usually passed to it by filtering the metads to only contain the row corresponding
#' to the check of interest, and then assigning each parameter as the contents of the eponymous column
#' of metads (see example below). This is because this function is mostly run inside of an mcmapply in
#' the run_all_checks_parallel function, which loops over the checks in the rows of metads.
#'
#' @param check  Check name.
#'
#' @param fxn_in Function performing the check.
#'
#' @param xls_title Excel title.
#'
#' @param pdf_title PDF title.
#'
#' @param pdf_subtitle PDF subtitle.
#'
#' @param pdf_return Text to display in PDF if check does not run.
#'
#' @param verbose Whether to display messages while running
#'
#' @details to look up documentation for the data checks package, please use command ??sdtmchecks
#'
#' @return list with results from the check.
#' 
#' @export
#' 
#' @family ex_rpt
#' 
#' @keywords ex_rpt
#' 
#' @examples
#'
#' 
#' # Assuming sdtm datasets are in your global environment
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
#' # Filter sdtmchecksmeta so that only one check is present
#' metads <- sdtmchecksmeta[sdtmchecksmeta$check=="check_ae_aedecod",] 
#'
#' run_check(
#'   check = metads$check,
#'   fxn_in = metads$fxn_in,
#'   xls_title = metads$xls_title,
#'   pdf_title = metads$pdf_title,
#'   pdf_subtitle = metads$pdf_subtitle,
#'   pdf_return = metads$pdf_return,
#'   verbose = FALSE
#' )

run_check = function(check,
                     fxn_in,
                     xls_title,
                     pdf_title,
                     pdf_subtitle,
                     pdf_return,
                     verbose){
    
    if (verbose == TRUE) { # have to print in this workaround way since cat doesn't work while parallel processing
        system(sprintf('echo "%s"', paste0("Running:", check)))
    }
    
    # run a data check
    result <-
        try(eval(parse(text = paste0(
            check, "(", fxn_in, ")"
        ))), silent = TRUE)
    
    # check if returned data is data frame
    if (!is.null(attributes(result)$data) & !is.data.frame(attributes(result)$data)){
        warning(paste0(
            "\nReturned data are not a data frame for check: ",
            check,
            "."
        ))
    }
    
    # read the message, resulting dataset and other variables
    msg <- as.character(attributes(result)$msg)
    data <- as.data.frame(attributes(result)$data)
    xls_title <- substr(xls_title, start = 1, stop = 31)
    notes <- " "
    
    # determine n of recs in the data check dataset. This will be used for the summary page.
    if (any(grepl("try-error",class(result)))) {
        nrec = 0
        notes <-
            paste0("ATTENTION! Check was not run: ", pdf_return)
    } else if (is.null(attributes(result)$data)) {
        nrec <-
            0 # set to 0 if resulting dataset was not created in case where
        # no bad records found or check failed due to missing variables.
        
        # if check failed due to missing variables then alert user
        if (result != TRUE)
            notes <-
                paste0("ATTENTION! Check was not run: ",
                       attributes(result)$msg)
        
    } else{
        # get number of issues using nrow() for dataframes/tibbles and length for vectors
        nrec <-
            ifelse(any(grepl(
                "data.frame", class(attributes(result)$data)
            )),
            nrow(attributes(result)$data),
            length(attributes(result)$data))
    }
    
    # create a list with items related to individual check
    rec <-
        list(
            xls_title = xls_title,
            pdf_title = pdf_title,
            pdf_subtitle = pdf_subtitle,
            nrec = nrec,
            notes = notes,
            msg = msg,
            data = data
        )
    
    return(rec)
    
}
