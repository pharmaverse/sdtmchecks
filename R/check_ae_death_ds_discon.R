#' @title Check if death in AE then there should be a study discon form
#'
#' @description This checks that if death is indicated in AE via
#' AEDTHDTC/AESDTH/AEOUT (as well as grade 5 AE if AETOXGR exists) then
#' there should be a study discontinuation record indicated by DS.DSSCAT
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AEDTHDTC, AESDTH, AEOUT
#' @param DS Disposition SDTM dataset with variables USUBJID, DSCAT, DSSCAT
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @importFrom dplyr %>% select
#' @importFrom tidyselect any_of
#'
#' @author Sara Bodach
#'
#' @examples
#'
#' AE <- data.frame(
#'  STUDYID = rep(1,6),
#'  USUBJID = 1:6,
#'  AEDTHDTC = c(NA,"2020-01-01",NA,NA,NA,NA),
#'  AESDTH = c(NA,NA,"Y",NA,NA,NA),
#'  AEOUT = c(NA,NA,NA,"FATAL",NA,NA),
#'  AETOXGR = c(NA,NA,NA,NA,"5",NA),
#'  AESPID="FORMNAME-R:2/L:2XXXX"
#' )
#'
#' DS <- data.frame(
#'  STUDYID = 1,
#'  USUBJID = 1:3,
#'  DSCAT="DISPOSITION EVENT",
#'  DSSCAT=c("STUDY DISCON",
#'  "STUDY DISCON",
#'  "STUDY COMPLETION/EARLY DISCONTINUATION")
#' )
#'
#' check_ae_death_ds_discon(AE,DS)
#' check_ae_death_ds_discon(AE,DS,preproc=roche_derive_rave_row)
#'
#' DS$DSSCAT = NULL
#'
#' check_ae_death_ds_discon(AE,DS)
#'

check_ae_death_ds_discon <- function(AE,DS,preproc=identity,...){

    ###First check that required variables exist and return a message if they don't
    if( AE %lacks_any% c("USUBJID","AEDTHDTC","AESDTH","AEOUT")){

        fail(lacks_msg(AE, c("USUBJID","AEDTHDTC","AESDTH","AEOUT")))

    }else if( DS %lacks_any% c("USUBJID","DSSCAT","DSCAT")){

        fail(lacks_msg(DS, c("USUBJID","DSSCAT","DSCAT")))

    } else{

        #Apply company specific preprocessing function
        AE = preproc(AE,...)

        # in ae keep rows where the death date is populated

        if (AE %has_any% "AETOXGR"){
            ae0 <- subset(AE,!is_sas_na(AE$AEDTHDTC) |
                              AE$AESDTH=="Y" |
                              AE$AEOUT=="FATAL" |
                              AE$AETOXGR=="5",) %>%
                select(any_of(c("USUBJID", "AEDTHDTC","AETOXGR","AESDTH","AEOUT","RAVE")))
        }else{
            ae0 <- subset(AE,!is_sas_na(AE$AEDTHDTC) |
                              AE$AESDTH=="Y" |
                              AE$AEOUT=="FATAL",) %>%
                select(any_of(c("USUBJID", "AEDTHDTC", "AESDTH", "AEOUT","RAVE")))
        }

        # find matching patients in DS
        ds0 <- subset(DS, (DS$USUBJID %in% ae0$USUBJID) )
        ds1 <- subset(ds0, (grepl("STUDY DISCON",
                                  toupper(ds0$DSSCAT)) |
                                toupper(ds0$DSSCAT)=='STUDY COMPLETION/EARLY DISCONTINUATION' |
                                toupper(ds0$DSSCAT)=='STUDY EARLY DISCONTINUATION/COMPLETION') &
                          grepl("DISPO", toupper(ds0$DSCAT)),
                      select=c("USUBJID", "DSSCAT", "DSCAT"))


        # check which patients have STUDY DISCON FORM
        mydfprep <- merge(unique(ds1), ae0, c("USUBJID"), all.y=TRUE)

        ## to fix the following line to use the subset function
        mydf <- subset(mydfprep, is_sas_na(mydfprep$DSSCAT))
        rownames(mydf)=NULL

        ###Print to report

        ### Return message if no records with missing STUDY DISCON form (i.e., DS.DSSCAT includes "STUDY DISCON" and DS.DSCAT includes "DISPO")
        if(nrow(mydf)==0){
            pass()
            ### Return subset dataframe if there are records with missing STUDY DISCON
        }else if(nrow(mydf)>0){
            fail(
                msg = paste(length(unique(mydf$USUBJID)),
                            "patient(s) where AE data indicates death but no study discontinuation record in DS. "),
                data = mydf
            )
        }
    }
}
