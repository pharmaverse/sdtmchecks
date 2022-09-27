#' @title Check for patients who had Start/End date of treatment after study discontinuation date
#'
#' @description Check for patients who had Start/End date of treatment after study discontinuation date
#' in the DS and EX domains.
#'
#' @param DS Disposition SDTM dataset with variables USUBJID, DSSCAT, DSCAT and DSSTDTC
#' @param EX Exposure SDTM dataset with variables USUBJID, EXSTDTC, EXENDTC, EXTRT, EXDOSE and EXOCCUR (if available)
#'
#' @return Boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select mutate group_by arrange distinct ungroup n slice case_when
#'
#' @author Saibah Chohan, Ashley Mao, Tina Cho (HackR 2021 Team STA-R)
#'
#' @examples
#'
#' DS <- data.frame(
#'  USUBJID = c(rep(1,2), rep(2,2)),
#'  DSSCAT= rep(c("STUDY COMPLETION/EARLY DISCONTINUATION", "ADVERSE EVENT"),2),
#'  DSCAT = rep(c("DISPOSITION EVENT", "OTHER"),2),
#'  DSSTDTC = c("2019-12-29", "2019-12-20", "2019-12-10", "2019-12-01"),
#'  stringsAsFactors = FALSE
#'  )
#'
#' EX <- data.frame(
#'  USUBJID = c(rep(1,2), rep(2,2)),
#'  EXSTDTC = c("2019-12-20", "2019-12-28", "2019-12-26", "2019-12-27"),
#'  EXENDTC = c("2019-12-10", "2019-12-23", "2019-12-30", "2019-12-27"),
#'  EXTRT = c(rep("SOME DRUG", 2), rep("PLACEBO",2)),
#'  EXDOSE = c(10,10,0,0),
#'  stringsAsFactors = FALSE
#'  )
#'
#' check_ds_ex_after_discon(DS, EX)
#'
#' DS <- data.frame(
#'  USUBJID = c(rep(1,2), rep(2,2)),
#'  DSSCAT= rep(c("STUDY COMPLETION/EARLY DISCONTINUATION", "ADVERSE EVENT"),2),
#'  DSCAT = rep(c("DISPOSITION EVENT", "OTHER"),2),
#'  DSSTDTC = c("2019-12-29", "2019-12-20", "2019-12-10", "2019-12-01"),
#'  stringsAsFactors = FALSE
#'  )
#'
#' EX <- data.frame(
#'  USUBJID = c(rep(1,2), rep(2,2)),
#'  EXSTDTC = c("2019-12-20", "2019-12-28", "2019-12-01", "2019-12-02"),
#'  EXENDTC = c("2019-12-10", "2019-12-23", "2020", "2020"),
#'  EXTRT = c(rep("SOME DRUG", 2), rep("PLACEBO",2)),
#'  EXDOSE = c(10,10,0,0),
#'  stringsAsFactors = FALSE
#'  )
#'
#' check_ds_ex_after_discon(DS, EX)
#'
#'
#'


check_ds_ex_after_discon <- function(DS, EX){

    ###First check that required variables exist and return a message if they don't
    if(DS %lacks_any% c("USUBJID","DSSTDTC", "DSCAT", "DSSCAT")){

        fail(lacks_msg(DS, c("USUBJID","DSSTDTC", "DSCAT", "DSSCAT")))

    } else if (EX %lacks_any% c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC", "EXDOSE")){

        fail(lacks_msg(EX, c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC", "EXDOSE")))

    } else {

        # Get treatment discontinuation date

        DS <- DS %>%
            filter(!is_sas_na(DSSTDTC) &
                       (grepl("STUDY DISCON",toupper(DSSCAT)) |
                                              toupper(DSSCAT)=='STUDY COMPLETION/EARLY DISCONTINUATION' |
                                              toupper(DSSCAT)=='STUDY EARLY DISCONTINUATION/COMPLETION')
                   & grepl("DISPO", toupper(DSCAT))) %>%
            mutate(
                DSSTDTC_imp = as.Date(case_when(
                    nchar(trimws(DSSTDTC))==4 ~ paste0(trimws(DSSTDTC),"-01-01"),
                    nchar(trimws(DSSTDTC))==7 ~ paste0(trimws(DSSTDTC),"-01"),
                    TRUE~DSSTDTC
                ))) %>%
            select(USUBJID, DSSTDTC, DSSCAT, DSSTDTC_imp, DSCAT)

        # Check that they have valid dosage
        if ("EXOCCUR" %in% names(EX)) {
            EX <- EX %>% filter(EXOCCUR == "Y" & (EXDOSE>0 | (EXDOSE==0 & grepl('PLACEBO', toupper(EXTRT)))))

        } else{
            EX <- EX %>% filter(EXDOSE>0 | (EXDOSE==0 & grepl('PLACEBO', toupper(EXTRT))))
        }

        # Get max exposure start date
        # Impute missing month/year with 01 (this will error on the side of false negatives)
        EX_start = EX %>%
            filter(!is_sas_na(EXSTDTC)) %>%
            mutate(
                EXSTDTC_imp = as.Date(case_when(
                    nchar(trimws(EXSTDTC))==4 ~ paste0(trimws(EXSTDTC),"-01-01"),
                    nchar(trimws(EXSTDTC))==7 ~ paste0(trimws(EXSTDTC),"-01"),
                    TRUE~EXSTDTC
                ))) %>%
            arrange(USUBJID,EXSTDTC) %>%
            group_by(USUBJID) %>%
            slice(n()) %>%
            ungroup() %>%
            distinct(USUBJID,EXSTDTC,EXSTDTC_imp)

        # Get max exposure end date
        # Impute missing month/year with 01 (this will error on the side of false negatives)
        EX_end = EX %>%
            filter(!is_sas_na(EXENDTC)) %>%
            mutate(
                EXENDTC_imp = as.Date(case_when(
                    nchar(trimws(EXENDTC))==4 ~ paste0(trimws(EXENDTC),"-01-01"),
                    nchar(trimws(EXENDTC))==7 ~ paste0(trimws(EXENDTC),"-01"),
                    TRUE~EXENDTC
                ))) %>%
            arrange(USUBJID,EXENDTC) %>%
            group_by(USUBJID) %>%
            slice(n()) %>%
            ungroup() %>%
            distinct(USUBJID,EXENDTC,EXENDTC_imp)

        # Get overall max exposure date comparing start and end
        EX_all = rbind((EX_start %>% select(USUBJID,DATE=EXSTDTC_imp))
                       ,
                       (EX_end %>% select(USUBJID,DATE=EXENDTC_imp))
                       ) %>%
            arrange(USUBJID,DATE) %>%
            group_by(USUBJID) %>%
            slice(n()) %>%
            ungroup %>%
            distinct(USUBJID,max_STDEND=DATE)

        # Merge together and do the check for exposure after study discon
        mydf = DS %>%
            left_join(EX_all,by="USUBJID") %>%
            left_join(EX_start,by="USUBJID") %>%
            left_join(EX_end,by="USUBJID") %>%
            filter(difftime(DSSTDTC_imp, max_STDEND)<0) %>%
            select(USUBJID, max_EX_start=EXSTDTC, max_EX_end=EXENDTC, DSSCAT, DSSTDTC)


        ### Return message if patient's latest Start Date/Time of Treatment or
        ### latest End Date/Time of Treatment occurred after study discontinuation

        if(nrow(mydf)==0){
            pass()

        }else if(nrow(mydf)>0){

            fail(paste(length(unique(mydf$USUBJID)),
                       " patient(s) with suspicious Start/End date of treatment occurring after study discontinuation. ",sep=""),
                 mydf)
        }
    }
}

