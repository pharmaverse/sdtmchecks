#' @title Check duplicate patient records in DM based on USUBJID
#'
#' @description This check looks for duplicate patient demographics records in DM
#'
#' @param DM Demographics SDTM dataset with variable USUBJID
#'
#' @return Boolean value for whether the check passed or failed, with 'msg'
#'   attribute if the check failed
#'
#' @export
#'
#' @importFrom dplyr %>% count filter mutate select group_by ungroup n
#'
#' @author Madeleine Ma, Stella Banjo (HackR 2021)
#'
#' @examples
#'
#' ## duplicates and same patient number across sites for 3-part USUBJID
#' DM <- data.frame(USUBJID = c("GO12345-00000-1000",
#'                              "GO12345-11111-1000",
#'                              "GO12345-00000-1000",
#'                              "GO12345-00000-1001"),
#'       stringsAsFactors = FALSE)
#'
#' check_dm_usubjid_dup(DM)
#'
#'
#' ## no duplicate IDs in the dataframe for 3-part USUBJID
#' DM2 <- data.frame(USUBJID = c("GO12345-00000-1000",
#'                               "GO12345-11111-1001",
#'                               "GO12345-11111-1002"),
#'              stringAsFactors = FALSE)
#'
#' check_dm_usubjid_dup(DM2)
#'
#'
#' ## duplicates for 2-part USUBJID
#' DM3 <- data.frame(USUBJID = c("GO12345-1000",
#'                               "GO12345-1000"),
#'             stringAsFactors = FALSE)
#'
#' check_dm_usubjid_dup(DM3)
#'
#'
#' ##  no duplicate IDs in the dataframe for 2-part USUBJID
#' DM4 <- data.frame(USUBJID = c("GO12345-1000",
#'                               "GO12345-1001",
#'                               "GO12345-1002"),
#'              stringAsFactors = FALSE)
#'
#' check_dm_usubjid_dup(DM4)
#'
#' ##  dataframe with one or two additional variables, if there is variation across other variables
#' DM5 <- data.frame(USUBJID = c("GO12345-1000",
#'                               "GO12345-1000"),
#'                   SEX = c("M", "F"),
#'                   AGE = c(18, 60),
#'          stringAsFactors = FALSE)
#'
#' check_dm_usubjid_dup(DM5)
#'
#' ## dataframe in which USUBJID is not present
#' DM6 <- data.frame(
#'          STUDYID = c("GO12345"),
#'          SEX = c("M"),
#'          AGE = c(72),
#'      stringAsFactors = FALSE)
#'
#' check_dm_usubjid_dup(DM6)
#'

check_dm_usubjid_dup <- function(DM) {

    if (DM %lacks_any% "USUBJID") {

        fail(lacks_msg(DM, "USUBJID"))

    } else {

        DM2 <- select(DM, USUBJID)

        # Identify duplicate USUBJID
        DM2$FLAG <- ifelse(duplicated(DM2$USUBJID) == TRUE, "Duplicate USUBJID", "")

        # Derive patient number for USUBJID not identified as duplicates
        DM2$subject_id <- ifelse(DM2$FLAG == "", gsub(".+\\-(\\d+$)","\\1", DM2$USUBJID), "")

        # Identify duplicate patient numbers across different USUBJIDs
        DM3 <- group_by(DM2, subject_id) %>%
            mutate(n = n()) %>%
            ungroup()


        DM3$FLAG <- ifelse(DM3$FLAG == "" & DM3$n > 1, "Same Patient Number Across Different USUBJID", DM3$FLAG)

        DM4 <- filter(DM3, FLAG != "") %>%
            select(-c(subject_id, n))

        if (nrow(DM4) > 0) {

            fail("Duplicate USUBJID and/or same Patient number across different USUBJIDs", DM4)

        } else {

            pass()
        }
    }
}
