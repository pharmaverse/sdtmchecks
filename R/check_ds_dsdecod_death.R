#' @title Check for study discontinuation record if death indicated
#'
#' @description If a patient has a record where DS.DSDECOD == DEATH they should
#'   also have a Study Discon Record
#'
#' @param DS Disposition domain with variables USUBJID, DSDECOD, DSSCAT, and
#' optional variables DSCAT, DSSTDTC, DSSPID
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#'
#' @author Sara Bodach and Will Harris
#'
#' @importFrom dplyr distinct %>% select filter
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @examples
#'
#' DS <- data.frame(
#'  STUDYID = 1,
#'  USUBJID = 1:3,
#'  DSDECOD = c(NA,"DEATH",NA),
#'  DSSTDTC = c(NA,"DSDATE",NA),
#'  DSCAT = c('DISPOSITION EVENT', 'DISPOSITION EVENT', 'OTHER'),
#'  DSSCAT = c('STUDY COMPLETION/EARLY DISCONTINUATION',
#'             'TREATMENT DISCONTINUATION',
#'             'STUDY TREATMENT'),
#'  DSOTH = 1:3,
#'  DSSPID = "XXX-R:0",
#'  stringsAsFactors=FALSE
#'  )
#'
#' check_ds_dsdecod_death(DS)
#' check_ds_dsdecod_death(DS,preproc=roche_derive_rave_row)
#'
#' DS$DSSCAT[2] <- "STUDY COMPLETION/EARLY DISCONTINUATION"
#' check_ds_dsdecod_death(DS)
#'
#' DS$DSDECOD = NULL
#' check_ds_dsdecod_death(DS)
#'
#'


check_ds_dsdecod_death <- function(DS,preproc=identity,...){

    if (DS %lacks_any% c("USUBJID", "DSDECOD", "DSSCAT")) {

        fail(lacks_msg(DS, c("USUBJID", "DSDECOD", "DSSCAT")))

    } else {

        #Apply company specific preprocessing function
        DS = preproc(DS,...)
        DS <- DS %>%
            select(any_of(c("USUBJID", "DSDECOD", "DSCAT", "DSSCAT", "DSSTDTC", "RAVE")))

        # Subset DS death records and include only records without a STUDY
        # COMPLETION/DISCONTINUATION form

        # find records with DEATH indicated in DS - note that these are not
        # necessarily unique by USUBJID
        ref <- DS %>%
            filter(grepl("DEATH", toupper(DSDECOD)))

        # find patients that have a STUDY DISCONTINUATION record
        discon <- DS %>%
            filter(
                grepl("STUDY", toupper(DSSCAT)),
                grepl("DISCON", toupper(DSSCAT)),
                !grepl("DRUG", toupper(DSSCAT)),
                !grepl("TREATMENT", toupper(DSSCAT))
            ) %>%
            mutate(DISCFL="Y") %>%
            select(USUBJID, DISCFL)

        discon <- distinct(discon)

        # merge datasets to output only patients without DISCON records in DS
        # but known to have died
        mydf0 <- merge(
                x = ref,
                y = discon,
                by = c("USUBJID"),
                all.x = TRUE,
                all.y = FALSE
            )

        mydf <- mydf0 %>%
            filter(is_sas_na(DISCFL)) %>%
            select(-any_of(c("DISCFL", "DSSCAT")))

        # replace <NA> with blank in DSSCAT
        mydf[is.na(mydf)] <- ""

        rownames(mydf)=NULL

        if (nrow(mydf) == 0) {
            pass()
        } else {
            fail(
                paste(nrow(mydf), "record(s) for",
                      length(unique(mydf$USUBJID)),
                      "unique patient(s) with DSDECOD='DEATH' but no",
                    "record with DSSCAT indicating STUDY DISCONTINUATION. "
                ),
                data = mydf
            )
        }
    }
}

