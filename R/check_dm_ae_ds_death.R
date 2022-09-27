#' @title Check if death reported in DM then death indicator also present in DS or AE
#'
#' @description This checks that when death is indicated in DM with either of DTHFL or
#'              DTHDTC then there should be death indicated in either AE or DS.
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AEDTHDTC, AESDTH, AEOUT
#' @param DS Disposition SDTM dataset with variables USUBJID, DSDECOD, DSSTDTC
#' @param DM Demographics SDTM dataset with variables USUBJID, DTHFL, DTHDTC
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author Sara Bodach
#'
#' @examples
#'
#' AE <- data.frame(
#'  STUDYID = 1,
#'  USUBJID = 1:3,
#'  AEDTHDTC = c(NA,1,NA),
#'  AESDTH = c(NA,"Y",NA),
#'  AEOUT = c(NA,"FATAL",NA),
#'  AETOXGR = c(NA,"5",NA)
#' )
#'
#' DS <- data.frame(
#'  STUDYID = 1,
#'  USUBJID = 1:3,
#'  DSDECOD = c(NA,"DEATH",NA),
#'  DSSTDTC = c(NA,"DSDATE",NA)
#' )
#'
#' DM <- data.frame(
#'  STUDYID = 1,
#'  USUBJID = 1:3,
#'  DTHFL=c(NA,"Y","Y"),
#'  DTHDTC = c(NA,"DMDATE","DMDATE")
#'  )
#'
#' check_dm_ae_ds_death(DM,DS,AE)
#'
#' DS$DSDECOD = NULL
#'
#' check_dm_ae_ds_death(DM,DS,AE)
#'

check_dm_ae_ds_death <- function(DM,DS,AE){

    ###First check that required variables exist and return a message if they don't
    if(AE %lacks_any% c("USUBJID","AEDTHDTC","AEOUT","AESDTH")){

        fail(lacks_msg(AE, c("USUBJID","AEDTHDTC","AEOUT","AESDTH")))

    }else if(DS %lacks_any% c("USUBJID" ,"DSDECOD","DSSTDTC")){

        fail(lacks_msg(DS, c("USUBJID", "DSDECOD","DSSTDTC")))

    }else if(DM %lacks_any% c("USUBJID", "DTHFL", "DTHDTC")){

        fail(lacks_msg(DM, c("USUBJID", "DTHFL", "DTHDTC")))

    } else{

        # Get death information from AE, DS, DM
        if (AE %has_any% "AETOXGR"){
            aedth <- subset(AE,!(is_sas_na(AE$AEDTHDTC)) | AE$AESDTH=="Y" | AE$AEOUT=="FATAL" | AE$AETOXGR=="5",
                            select=c("USUBJID", "AEDTHDTC", "AETOXGR", "AESDTH", "AEOUT"))
        }else{
            aedth <- subset(AE,!(is_sas_na(AE$AEDTHDTC)) | AE$AESDTH=="Y" | AE$AEOUT=="FATAL",
                            select=c("USUBJID", "AEDTHDTC", "AESDTH", "AEOUT"))
        }

        dsdth <- subset(DS,(DS$DSDECOD=="DEATH"),
                        select=c("USUBJID", "DSSTDTC"))
        dmdth <- subset(DM,!(is_sas_na(DM$DTHDTC)) | DM$DTHFL=="Y",
                        select=c("USUBJID", "DTHDTC", "DTHFL"))

        # Get earliest AE death record
        if(nrow(aedth) > 0
        ){
            aedths <- aedth[order(aedth$USUBJID, aedth$AEDTHDTC), ]
            aedthsl = unique(aedths)
        }else if(nrow(aedth) == 0
        ){
            aedthsl <- aedth
        }

        # Get earliest DS death record
        if(nrow(dsdth) > 0
        ){
            dsdths <- dsdth[order(dsdth$USUBJID, dsdth$DSSTDTC), ]
            dsdthsl = unique(dsdths)
        }else if(nrow(dsdth)==0
        ){
            dsdthsl <- dsdth
        }

        # JOIN AE and DS deaths
        aeds <- merge(x=aedthsl, y=dsdthsl, by="USUBJID", all=TRUE)

        # Get only DM deaths not reported in AE or DS
        mydf <- subset(dmdth, !(dmdth$USUBJID %in% aeds$USUBJID))
        rownames(mydf)=NULL
        ###Print to report

        ### Return message
        if(nrow(mydf)==0){
            pass()

            ### Return subset dataframe with death information reported in DM (and not either DS or AE)
        }else if(nrow(mydf)>0){
            fail(
                msg = paste(length(unique(mydf$USUBJID)),
                            "patient(s) where DM data indicates death but no record indicating death in DS or AE. "),
                data = mydf
            )
        }
    }
}

