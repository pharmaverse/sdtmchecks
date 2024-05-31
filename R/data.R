#' Metadata for sdtmchecks
#'
#' A dataset containing the SDTM checks in the package. The data can be used as input into functions.
#'
#' @format A data frame with a row for each R check in the package:
#' \describe{
#'   \item{check}{R check name, without .R file extension}
#'   \item{category}{Therapeutic area grouping}
#'   \item{priority}{High, Medium, Low}
#'   \item{domains}{SDTM domains used in function}
#'   \item{xls_title}{Excel title for tab}
#'   \item{pdf_title}{PDF title for check}
#'   \item{pdf_subtitle}{PDF subtitle for check, with * at the start of each subtitle line}
#'   \item{pdf_return}{PDF return message when SDTM domain not available}
#'   \item{fxn_in}{explicit string input of domain name(s) into R check function}
#'   \item{fxn_in_roche}{explicit string input of domain name(s) into R check function, Roche specific}
#'   \item{mapping}{Is this related to mapping?  i.e. Not a site issue.}
#'   \item{exist_string}{explicit string input to check existence of SDTM domain(s) before running check}
#'   }
#'
#'
#' @docType data
#' @usage data(sdtmchecksmeta)
#' @keywords datasets
#' @name sdtmchecksmeta
#'
#' @examples
#' data(sdtmchecksmeta)
#' head(sdtmchecksmeta[,1:5])
#'

"sdtmchecksmeta"



#' Nickname for package version
#'
#' A nickname associated with the package version.  Previously lived in the description file.
#' 
#' @docType data
#' @name nickname
#' @keywords internal
#'
#'

"nickname"