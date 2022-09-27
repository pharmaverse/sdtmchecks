#' @title Check patients in the DM dataset who do not have records in the AE dataset
#'
#' @description This check looks for patients in the DM dataset who do not have
#' records in the AE dataset, and obtains first treatment start date and earliest
#' death date for these patients
#'
#' @param DM Demographics SDTM dataset with variable USUBJID
#' @param AE Adverse Events SDTM dataset with variable USUBJID
#' @param EX Exposure SDTM dataset with variables USUBJID, EXDOSE, EXSTDTC,
#'  EXTRT
#' @param DS Disposition SDTM dataset with variables USUBJID, DSSTDTC, DSDECOD
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#'
#' @author Vani Nimbal
#'
#' @examples
#' USUBJID<- c(1:10)
#' DM=data.frame(USUBJID)
#' AE=data.frame(USUBJID)
#' AE$USUBJID[3]=NA
#' AE$USUBJID[8]=NA
#' AE$USUBJID[10]=NA
#'
#' EX <- data.frame(
#' USUBJID = c(1:8, 6, 8, 10, 10, 10, 10),
#' EXOCCUR = rep("Y", times=14),
#' EXDOSE = rep(c(1,2), times=7),
#' EXSTDTC = c(rep("2012-01-01", 10),"2012-02-04","2012-02-04", "", "2012-02-07"),
#' EXTRT = "GDC",
#' stringsAsFactors=FALSE
#' )
#'
#' DS <- data.frame(
#' USUBJID = c(2,8,8),
#' DSDECOD = rep("DEATH", times=3),
#' DSSTDTC = c("2012-12-01", NA, "2013-07-01"),
#' stringsAsFactors=FALSE
#' )
#' check_dm_usubjid_ae_usubjid(DM, AE, DS, EX)
#'
#' EX$EXOCCUR[3]="N"
#'
#' check_dm_usubjid_ae_usubjid(DM, AE, DS, EX)
#'
#' EX$EXOCCUR=NULL
#'
#' check_dm_usubjid_ae_usubjid(DM, AE, DS, EX)
#'

check_dm_usubjid_ae_usubjid <- function(DM, AE, DS, EX){

    if (DM %lacks_any% "USUBJID"){

        fail(lacks_msg(DM, "USUBJID"))

    } else if (AE %lacks_any% "USUBJID"){

        fail(lacks_msg(AE, "USUBJID"))

    } else if (DS %lacks_any% c("USUBJID","DSSTDTC","DSDECOD")) {

        fail(lacks_msg(DS, c("USUBJID","DSSTDTC","DSDECOD")))

    } else if (EX %lacks_any% c("USUBJID","EXSTDTC","EXDOSE","EXTRT")) {

        fail(lacks_msg(EX, c("USUBJID","EXSTDTC","EXDOSE","EXTRT")))

    } else if(DM %has_all% "USUBJID" & AE %has_all% "USUBJID"){

        ### Subset observations where USUBJID exists in DM but not in AE
        dm_no_ae <- DM %>%
                    select("USUBJID") %>%
                    filter(!(USUBJID %in% unique(AE$USUBJID)))
        rownames(dm_no_ae)=NULL

        ### Pass if all USUBJIDs in DM also exist in AE
        if(nrow(dm_no_ae)==0){
            pass()

        ### Return subset dataframe if there are records in DM but not in AE
        }else if(nrow(dm_no_ae)>0){

            ### Obtain first treatment start date for these patients
                if ("EXOCCUR" %in% names(EX)) { EX <- EX %>% filter(EX$EXOCCUR == "Y")}
                myex <- EX %>%
                        filter(!is_sas_na(EXSTDTC)
                               & (EXDOSE>0 | (EXDOSE==0 & grepl("PLACEBO",toupper(EXTRT))))
                               & USUBJID %in% dm_no_ae$USUBJID) %>%
                        select("USUBJID","EXSTDTC")
                ranks_ex <- with(myex, ave(as.numeric(as.Date(myex$EXSTDTC)), myex$USUBJID,
                                        FUN = function(x) rank(x, ties.method="first")))
                myex0 <- myex %>% filter(ranks_ex == 1)
                ex_first<-merge(dm_no_ae, myex0, by="USUBJID", all.x=TRUE)

            # Obtain earliest death date for these patients
                myds <- DS %>%
                        filter(!is_sas_na(DSSTDTC) & DS$DSDECOD=="DEATH" & USUBJID %in% dm_no_ae$USUBJID) %>%
                        select("USUBJID","DSDECOD","DSSTDTC")
                ranks_ds <- with(myds, ave(as.numeric(as.Date(myds$DSSTDTC)), myds$USUBJID,
                                        FUN = function(x) rank(x, ties.method="first")))
                myds0 <- myds %>% filter(ranks_ds == 1)

                dm_ex_ds<-merge(ex_first, myds0, by="USUBJID", all.x=TRUE)

                # replace <NA> with blank
                dm_ex_ds[is.na(dm_ex_ds)] <- ""

            fail(paste0("There is/are ",nrow(dm_ex_ds),
                        " patient(s) in DM without Adverse Events reported. "),
                 dm_ex_ds)
        }}
}
