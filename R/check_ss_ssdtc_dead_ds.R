#' @title Check non-missing DEAD status date in SS and non-missing according DS record with death date
#'        where status date is greater or equal to death date
#'
#' @description This check looks for missing death date in DS dataset
#'              if there is DEAD status date in SS dataset or
#'              if Subject Status Date/Time of Assessments is less than
#'              Start Date/Time of Disposition Event(SS.SSDTC < DS.DSSTDTC)
#'
#' @param SS Subject Status SDTM dataset with variables USUBJID, SSDTC, SSSTRESC, VISIT
#' @param DS Disposition SDTM dataset with variables USUBJID, DSSTDTC, DSDECOD, DSCAT
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr left_join filter %>% rename
#' @importFrom tidyselect any_of
#'
#' @author Vira Vrakina
#'
#' @examples
#'
#' SS <- data.frame(
#'  USUBJID = 1:5,
#'  SSDTC = "2020-01-02",
#'  SSSTRESC  = c("DEAD","DEAD","ALIVE","DEAD","ALIVE"),
#'  VISIT = "FOLLOW-UP",
#'  SSSPID = "FORMNAME-R:13/L:13XXXX"
#' )
#'
#' DS <- data.frame(
#'  USUBJID = 1:5,
#'  DSSTDTC  = c("2020-01-02","2020-01-02", "2020-01-01", "2020-01-03", "2020-01-01"),
#'  DSDECOD = c(rep('DEATH', 5)),
#'  DSSPID = "FORMNAME-R:13/L:13XXXX",
#'  DSCAT   = c("OTHER EVENT", rep("DISPOSITION EVENT", 4))
#' )
#'
#' check_ss_ssdtc_dead_ds(SS, DS)
#' check_ss_ssdtc_dead_ds(SS, DS, preproc=roche_derive_rave_row)
#'
#'
#' SS <- data.frame(
#'  USUBJID = 1:5,
#'  SSDTC = "2020-01-02",
#'  SSSTRESC  = c( rep("DEAD", 5)),
#'  VISIT = "FOLLOW-UP",
#'  SSSPID = "FORMNAME-R:13/L:13XXXX"
#' )
#'
#' DS <- data.frame(
#'  USUBJID = 1:5,
#'  DSSTDTC  = c("2020-01-02","2020-01-02", "2020-01-01", "2020-01-03", "2020-01-01"),
#'  DSDECOD = c(rep('DEATH', 5)),
#'  DSSPID = "FORMNAME-R:13/L:13XXXX",
#'  DSCAT   = c(rep("DISPOSITION EVENT", 5))
#' )
#'
#' check_ss_ssdtc_dead_ds(SS, DS)
#' check_ss_ssdtc_dead_ds(SS, DS, preproc=roche_derive_rave_row)
#'
#'
#' SS <- data.frame(
#'  USUBJID = 1:5,
#'  SSDTC = "2020-01-02",
#'  SSSTRESC  = c(rep("DEAD", 5)),
#'  VISIT = "FOLLOW-UP",
#'  SSSPID = "FORMNAME-R:13/L:13XXXX"
#' )
#'
#' DS <- data.frame(
#'  USUBJID = 1:5,
#'  DSSTDTC  = 2,
#'  DSDECOD = c(rep('DEATH', 5)),
#'  DSSPID = "FORMNAME-R:13/L:13XXXX",
#'  DSCAT   = c(rep("DISPOSITION EVENT", 5))
#' )
#'
#' check_ss_ssdtc_dead_ds(SS, DS)
#'

check_ss_ssdtc_dead_ds <- function(SS, DS, preproc=identity,...) {

    ###First check that required variables exist and return a message if they don't
    if(SS %lacks_any% c("USUBJID","SSDTC","SSSTRESC", "VISIT")){

        fail(lacks_msg(SS, c("USUBJID","SSDTC","SSSTRESC", "VISIT")))

    }else if(DS %lacks_any% c("USUBJID", "DSSTDTC", "DSDECOD", "DSCAT")){

        fail(lacks_msg(DS, c("USUBJID", "DSSTDTC", "DSDECOD", "DSCAT")))

    }else{
        
        #Apply company specific preprocessing function
        DS = preproc(DS,...)
        SS = preproc(SS,...)

        myss <- subset(SS, !is_sas_na(SS$SSDTC) & toupper(SS$SSSTRESC) == 'DEAD') %>% 
            select(any_of(c("USUBJID","SSDTC","SSSTRESC", "VISIT","RAVE")))
        myds <- subset(DS, !is_sas_na(DS$DSSTDTC) & toupper(DS$DSDECOD) == 'DEATH' & toupper(DS$DSCAT) == "DISPOSITION EVENT") %>% 
            select(any_of(c("USUBJID", "DSSTDTC", "DSDECOD", "DSCAT","RAVE")))
        
        if("RAVE" %in% names(myds)){myds = myds %>% rename(DS_RAVE=RAVE)}
        if("RAVE" %in% names(myss)){myss = myss %>% rename(SS_RAVE=RAVE)}
        
        mydf <- myss %>%
            left_join(myds, by="USUBJID") %>%
            filter(is_sas_na(DSSTDTC) | SSDTC < DSSTDTC)

        ###Print to report

        ### Return message if no records
        if(nrow(mydf)==0 ){
            pass()

            ### Return subset dataframe if there are records where SS.SSDTC < DM.DTHDTC
        }else if(nrow(mydf)>0){


            fail(paste(length(unique(mydf$USUBJID)),
                       " patient(s) with death information in SS domain but no death information in DS domain or date with DEAD status in SS dataset is less than death date in DS dataset. ",sep=""),
                       mydf)
        }
    }
}
