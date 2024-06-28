
#' @title Check if patient with Death due to AE also has Death record in DS
#'
#' @description Flag if patient has a Death in AE (i.e. AE record with non-missing AE.AEDTHDTC)
#'              but no Death in DS (i.e. record where DS.DSDECOD=DEATH and
#'              DS.DSTERM contains 'DEATH' and does not contain 'PROGRESSIVE DISEASE' or 'DISEASE RELAPSE'
#'              (so we can pick up records where DSTERM in 'DEATH','DEATH DUE TO ...' and exclude
#'              'DEATH DUE TO PROGRESSIVE DISEASE', 'DEATH DUE TO DISEASE RELAPSE')
#'
#' @param AE Adverse Events SDTM dataset with USUBJID, AEDTHDTC, AESPID (optional)
#' @param DS Disposition SDTM dataset with USUBJID, DSDECOD, DSTERM
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% select
#' @importFrom tidyselect any_of
#'
#' @author Edgar Manukyan, N Springfield updated on 14SEP2020
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @examples
#'
#' AE <- data.frame(
#'  USUBJID = 1:5,
#'  AEDTHDTC = c("2018-01-01", "2018-01-02", "2018-01-03","2018-01-04", ""),
#'  AESPID="FORMNAME-R:13/L:13XXXX",
#'  stringsAsFactors = FALSE
#' )
#'
#' DS <- data.frame(
#'  USUBJID = c(1,1,2,3,3,4),
#'  DSTERM=c("DEATH","RANDOM THING","ADVERSE EVENT",
#'           "DEATH DUE TO PROGRESSIVE DISEASE","ADVERSE EVENT",
#'           "DEATH DUE TO ABC"),
#'  DSDECOD=c("DEATH","ADVERSE EVENT","DEATH", "DEATH","OTHER", "DEATH"),
#'  stringsAsFactors=FALSE
#' )
#'
#' check_dd_death_date(AE,DS)
#' check_dd_death_date(AE,DS,preproc=roche_derive_rave_row)
#'

check_dd_death_date <- function(AE, DS, preproc=identity,...){

    ###First check that required variables exist and return a message if they don't

    if (AE %lacks_any% c("USUBJID", "AEDTHDTC")) {

        fail(lacks_msg(AE, c("USUBJID", "AEDTHDTC")))

    } else if (DS %lacks_any% c("USUBJID", "DSDECOD", "DSTERM")) {

        fail(lacks_msg(DS, c("USUBJID", "DSDECOD", "DSTERM")))

    } else {

        #Apply company specific preprocessing function
        AE = preproc(AE,...)

        # Subset AE for recs with non-missing AE.AEDTHDTC
        AE <- AE %>%
            select(any_of(c("USUBJID", "AEDTHDTC", "RAVE")))
        ae0 <- subset(AE, !is_sas_na(AE$AEDTHDTC),)

        # Select DEATH recs in DS, include DSDECOD contains DEATH and DSTERM containing DEATH
        # exclude DSTERM in (DEATH DUE TO PROGRESSIVE DISEASE, DEATH DUE TO DISEASE RELAPSE)
        ds0 <- subset(DS, (grepl("DEATH", toupper(DS$DSDECOD)) &
                               grepl("DEATH", toupper(DS$DSTERM)) &
                               !grepl("PROGRESSIVE DISEASE", toupper(DS$DSTERM)) &
                               !grepl("DISEASE RELAPSE", toupper(DS$DSTERM)) ),
                      ) %>%
            select(any_of(c("USUBJID", "DSTERM", "DSDECOD")))


        # keep patients with AE DEATH who lack DS DEATH record
        mydfprep<- ae0 %>%
            left_join(unique(ds0), by="USUBJID")
        mydf <- subset(mydfprep, is_sas_na(mydfprep$DSDECOD),)

        if (nrow(mydf) == 0) {
            pass()
        } else {

            fail(
                msg = paste(length(unique(mydf$USUBJID)),
                            "patient(s) with a death date in AE but death not reflected properly in DS. "),
                data = unique(mydf)
            )
        }
    }
}

