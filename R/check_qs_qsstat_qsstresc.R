#' @title Check for non-missing QSSTRESC if QSSTAT is NOT DONE
#'
#' @description This check is for studies with PRO outcomes data (i.e., QS domain),
#' check that within a given instrument (e.g., QS.QSCAT='BFI' or QS.QSCAT ='MDASI"),
#' if QS.QSSTAT=NOT DONE and QSTESTCD=QSALL, then there should be no populated
#' responses(QS.QSSTRESC) for a particular visit (QS.VISIT), return a dataframe if otherwise
#'
#' @param QS Questionnaires SDTM dataset with variables USUBJID, QSSTRESC,
#' VISIT, QSSTAT, QSCAT, QSDTC, QSTESTCD
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#'
#' @author Sara Bodach
#'
#' @examples
#'
#' QS <- data.frame(
#' STUDYID = 1,
#' USUBJID = c(rep(1,6),rep(2,6)),
#' QSSTRESC = 1:12,
#' VISIT = c(rep(1,3),rep(2,3),rep(1,3),rep(2,3)),
#' QSSTAT = rep(c("DONE","NOT DONE"),6),
#' QSCAT = rep(c("INDIVIDUAL","OVERALL","BFI"),4),
#' QSDTC = "2016-01-01",
#' QSTESTCD = "QSALL",
#' stringsAsFactors = FALSE
#' )
#'
#' check_qs_qsstat_qsstresc(QS)
#'
#' QS$QSSTRESC[4]=" "
#' QS$QSSTRESC[6]=NA
#' QS$QSSTRESC[8]="."
#' check_qs_qsstat_qsstresc(QS)
#'
#' QS$QSSTRESC=NULL
#' check_qs_qsstat_qsstresc(QS)
#'

check_qs_qsstat_qsstresc <- function(QS){

    ###First check that required variables exist and return a message if they don't
    if( QS %lacks_any% c("USUBJID", "QSSTRESC", "VISIT", "QSSTAT", "QSCAT",
                         "QSDTC", "QSTESTCD") ){

        fail(lacks_msg(QS, c("USUBJID", "QSSTRESC", "VISIT", "QSSTAT", "QSCAT",
                             "QSDTC", "QSTESTCD")))

    } else{

        # in QS keep rows where QSSTAT = NOT DONE and QSTESTCD = QSALL
        qsND <- QS %>%
            filter(QSSTAT=="NOT DONE" & QSTESTCD=="QSALL") %>%
            select(USUBJID, QSSTRESC, VISIT, QSSTAT, QSCAT, QSDTC)
                
        qsANS <- QS %>%
                select(USUBJID, QSSTRESC, VISIT, QSSTAT, QSCAT, QSDTC) %>%
                filter(!is_sas_na(QSSTRESC))

        # find matching patients in qsND
        qsNDsub <- qsND %>%
                   select(USUBJID, VISIT, QSSTAT, QSCAT, QSDTC)
        qsANSsub <- qsANS %>%
                   select(USUBJID, VISIT, QSSTRESC, QSCAT, QSDTC)
        qsPREP <- merge( qsNDsub, qsANSsub, c("USUBJID", "VISIT", "QSCAT", "QSDTC"),
                         all.x=TRUE)

        mydf <- qsPREP %>%
                filter(QSSTAT=="NOT DONE" & !is_sas_na(QSSTRESC)) %>%
                select(USUBJID, VISIT, QSCAT, QSDTC, QSSTAT,QSSTRESC)
        
        mydf = unique(mydf)
        rownames(mydf)=NULL
        
        ###Print to report

        ### Return message if no records with issue in QS
        if(nrow(mydf)==0){
            pass()

            ### Return subset dataframe if there are issues in QS with NOT DONE but results
        } else if(nrow(mydf)>0){
            fail(paste0("There are non-missing QSSTRESC records for the following ",
                        "visits when QSSTAT=NOT DONE and QSTESTCD=QSALL. ",sep=" "),
                 mydf)
        }
    }
}
