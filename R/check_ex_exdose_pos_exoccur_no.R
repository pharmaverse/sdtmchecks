#' @title Check for EXDOSE>0 When EXOCCUR is not "Y"
#'
#' @description This checks looks for EXDOSE values greater than 0 when
#' EXOCCUR is not "Y". It could be for a specified drug/treatment, or for
#' all drugs/treatments in the dataset.
#'
#' @param EX Exposure SDTM dataset with variables USUBJID, EXTRT, EXSTDTC,
#'   EXOCCUR and EXDOSE
#' @param drug Drug name for EXTRT; used to subset the dataset.
#'   Default value is NULL (i.e. no filtering by drug)
#'
#' @return Boolean value for whether the check passed or failed, with 'msg'
#'   attribute if the test failed
#'
#' @export
#'
#' @importFrom  dplyr %>% filter select
#'
#' @author Sara Bodach
#'
#' @examples
#'
#' EX <- data.frame(
#'  USUBJID = 1:5,
#'  EXSTDTC = rep("2017-01-01",5),
#'  EXTRT   = c(rep("TRT A",2),rep("TRT B",3)),
#'  EXOCCUR = c(".","", "N", "N", "Y"),
#'  EXDOSE  = 0:4,
#'  VISIT = "VISIT 1",
#'  stringsAsFactors = FALSE
#' )
#'
#' check_ex_exdose_pos_exoccur_no(EX)
#'
#' check_ex_exdose_pos_exoccur_no(EX, drug = "TRT A")
#' check_ex_exdose_pos_exoccur_no(EX, drug = "TRT B")
#'
#' EX$EXDOSE = NULL
#'
#' check_ex_exdose_pos_exoccur_no(EX)
#'

check_ex_exdose_pos_exoccur_no <- function(EX, drug=NULL) {

    # Checks if required variables are present
    if (EX %lacks_any% c("USUBJID","EXTRT","EXSTDTC","EXOCCUR","EXDOSE", "VISIT")) {

        fail(lacks_msg(EX, c("USUBJID","EXTRT","EXSTDTC","EXOCCUR","EXDOSE","VISIT")))

        # Checks validity of drug name argument
    } else if (!is.null(drug) && !(drug %in% EX[["EXTRT"]])) {

        fail(msg = "Drug name not found in dataset. ")

    } else {

        # Subsets EX to rows where EXOCCUR is not "Y" but EXDOSE is positive
        df <- EX %>%
            select("USUBJID", "EXTRT", "EXSTDTC", "EXOCCUR", "EXDOSE", "VISIT") %>%
            filter(EX$EXOCCUR != "Y" & EX$EXDOSE > 0)

        if (!is.null(drug)) {

            df <- df %>% filter(df$EXTRT == drug)

        }

        # Returns message depending on if there are records with positive dose
        # but occurrence not marked as "Y"
        if (nrow(df) != 0 && !is.null(drug)) {

            fail(paste0("There are ", length(unique(df$USUBJID)), " patients with ",
                        "positive dose amount (EXDOSE>0) when occurrence (EXOCCUR) for ",
                        drug, " is not 'Y'. "), df)

        } else if (nrow(df) != 0) {

            fail(paste0("There are ", length(unique(df$USUBJID)), " patients with ",
                        "positive dose amount (EXDOSE>0) when occurrence (EXOCCUR)",
                        " is not 'Y'. "), df)

        } else {

            pass()

        }
    }
}
