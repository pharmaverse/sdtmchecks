#' @title Check for missing values in the MISPEC variable
#'
#' @description This check looks for missing values in the MISPEC variable,
#' which is required. This will be flagged in P21. This may reflect a mapping issue.
#'
#' @param MI Microscopic Findings with variables USUBJID, MISPEC, MITESTCD, MIDTC
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @importFrom  dplyr %>% filter
#'
#' @author Stella Banjo (HackR 2021)
#'
#' @examples
#'
#' MI <- data.frame(
#'   USUBJID = c("1","2", "3"),
#'   DOMAIN = "MI",
#'   MISEQ = c(1, 2, 1),
#'   MISPEC = c("","BLOCK SLIDE",NA),
#'   MITESTCD = "TESTCD1",
#'   MIDTC = "2020-01-01",
#' stringAsFactors = FALSE
#')
#'
#'  check_mi_mispec(MI)
#'
#' ## No errors, MISPEC values present
#' MI2 <- data.frame(
#'  USUBJID = c("1","2", "3"),
#'  DOMAIN = "MI",
#'  MISEQ = 1,
#'  MISPEC = c("SLIDE", "TUMOR TISSUE", "BLOCK SLIDE"),
#'  MITESTCD = "TESTCD1",
#'  MIDTC = "",
#'  stringsAsFactors = FALSE
#' )
#'
#'  check_mi_mispec(MI2)
#'

check_mi_mispec <- function(MI) {

  # Required variables for MI domain
  req_var <- c("USUBJID", "MITESTCD", "MISPEC", "MIDTC")

  ###First check that required variables exist and return a message if they don't
  if (MI %lacks_any% req_var) {

    fail(lacks_msg(MI, req_var))

  } else {

      # Include required variables only
      MI = MI[, req_var]

      # Output any records where MISPEC is not populated
      mydf <- MI %>%
        filter(is_sas_na(MISPEC))

      if (nrow(mydf) > 0) {

        fail(paste0(nrow(mydf), " record(s) with required variable MISPEC not populated. ", sep = ""), mydf)

      } else {

        pass()

      }
    }
}
