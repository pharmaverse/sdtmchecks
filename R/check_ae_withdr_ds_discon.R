#' @title Check if an AE leading to drug being withdrawn is reflected in DS
#'
#' @description This checks that if there is an AE with AEACN="DRUG WITHDRAWN" then there should be a treatment discontinuation
#'              record indicated by DS.DSSCAT
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AEACN
#' @param DS Disposition SDTM dataset with variables USUBJID, DSCAT, DSSCAT
#' @param TS Trial Summary SDTM dataset with variables TSPARMCD, TSVAL
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#' @export
#'
#' @author Yuliia Bahatska
#'
#' @examples
#'
#' AE <- data.frame(
#'  USUBJID = 1:6,
#'  AEACN = c("DRUG WITHDRAWN",NA,NA,NA,NA,NA),
#'  AETOXGR = c(NA,NA,NA,NA,"5",NA),
#'  AEDECOD=c("NAUSEA","HEADACHE"),
#'  AESPID = "FORMNAME-R:5/L:5XXXX"
#' )
#' DS <- data.frame(
#'  USUBJID = 1:3,
#'  DSCAT="DISPOSITION EVENT",
#'  DSSCAT="STUDY TREATMENT",
#'  DSDECOD=c("COMPLETED","ADVERSE EVENT","DEATH")
#' )
#'
#'  TS <- data.frame(
#'  TSPARMCD="TRT",
#'  TSVAL="CHECK"
#' )
#'
#' check_ae_withdr_ds_discon(AE,DS,TS)
#' check_ae_withdr_ds_discon(AE,DS,TS,preproc=roche_derive_rave_row)
#'
#' DS$DSSCAT = NULL
#'
#' check_ae_withdr_ds_discon(AE,DS,TS)
#'

check_ae_withdr_ds_discon <- function(AE,DS,TS,preproc=identity,...){

    ###First check that required variables exist and return a message if they don't
    if( AE %lacks_any% c("USUBJID","AEACN")){

        fail(lacks_msg(AE, c("USUBJID","AEACN")))

    }else if( DS %lacks_any% c("USUBJID","DSSCAT","DSCAT","DSDECOD")){

        fail(lacks_msg(DS, c("USUBJID","DSSCAT","DSCAT","DSDECOD")))

    } else if( TS %lacks_any% c("TSPARMCD","TSVAL")){

        fail(lacks_msg(DS, c("TSPARMCD","TSVAL")))

    }else{

        #calculate number of drugs in the study
        agent_num<-filter(TS,(TSPARMCD %in% c("TRT","COMPTRT" ))) %>% nrow()

        #if a study is not single agent the check won't be executed
        if (agent_num != 1)
        {
          fail("This check is only applicable for single agent studies, but based on TS domain this study is not single agent or study type cannot be determined. ")
        }
        #only run for single agent studies
        else if (agent_num==1)
        {

        # in ae keep rows where the drug was withdrawn

          #Apply company specific preprocessing function
          AE = preproc(AE,...)

            ae0 <- subset(AE,AE$AEACN=="DRUG WITHDRAWN",) %>%
              select(any_of(c("USUBJID", "AEACN","AEDECOD","RAVE")))


        # find matching patients in DS
        DS = preproc(DS,...)

        ds0 <- subset(DS, (DS$USUBJID %in% ae0$USUBJID) )
        ds1 <- subset(ds0, (grepl("DISCON", toupper(ds0$DSSCAT)) | toupper(ds0$DSSCAT)=='TREATMENT COMPLETION/EARLY DISCONTINUATION' | toupper(ds0$DSSCAT)=='TREATMENT EARLY DISCONTINUATION/COMPLETION'
                            |(toupper(ds0$DSSCAT)=="STUDY TREATMENT")) & toupper(ds0$DSDECOD)!="COMPLETED" & grepl("DISPO", toupper(ds0$DSCAT)),) %>%
          select(any_of(c("USUBJID", "DSSCAT", "DSCAT","RAVE")))


        # check which patients have TREATMENT DISCON FORM
        mydfprep <- merge(unique(ds1), ae0, "USUBJID", all.y=TRUE)

        ## to fix the following line to use the subset function
        mydf <- subset(mydfprep, is_sas_na(mydfprep$DSSCAT))
        rownames(mydf)=NULL

        ###Print to report

        ### Return message if no records with missing TREATMENT DISCON form (i.e., DS.DSSCAT includes "TREATMENT DISCON" and DS.DSCAT includes "DISPO")
        if(nrow(mydf)==0){
            pass()
            ### Return subset dataframe if there are records with missing TREATMENT DISCON
        }else if(nrow(mydf)>0){
            fail(
                msg = paste("There are",
                            length(unique(mydf$USUBJID)),
                            "patient(s) where AE data treatment discontinuation but no treatment discontinuation record in DS. "),
                data = mydf
            )
        }
    }
  }
}
