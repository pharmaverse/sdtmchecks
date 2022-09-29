#' @title Check for missing MedDRA version in TS
#'
#' @description This check looks for missing MedDRA version;
#' if it's present, also checking it's the current version
#'
#' @param TS Trial Summary SDTM dataset with variables TSPARMCD and TSVAL
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select pull case_when mutate
#'
#' @author Vira Vrakina, Antony Howard (HackR 2021 Team Pentraxin1)
#'
#' @examples
#'
#' TS1 <- data.frame(
#'  STUDYID = 1,
#'  TSPARMCD = "AEDICT",
#'  TSVAL = "MedDRA 22.0",
#'  TSVAL2 = ""
#' )
#'
#' TS2 <- data.frame(
#'  STUDYID = 2,
#'  TSPARMCD = "AEDICT",
#'  TSVAL = "",
#'  TSVAL1 = "meddra v22.0"
#' )
#'
#' TS3 <- data.frame(
#'  STUDYID = 3,
#'  TSPARMCD = "AEDICT",
#'  TSVAL = ""
#' )
#'
#' TS4 <-data.frame(
#'  STUDYID = 4,
#'  TSPARMCD = "CMDICT",
#'  TSVAL = ""
#' )
#'
#' TS5 <- data.frame(
#'     STUDYID = 1,
#'     TSPARMCD = "AEDICT",
#'     TSVAL = "meddra 24.0",
#'     TSVAL2 = ""
#' )
#'
#' TS6 <- data.frame(
#'     STUDYID = 1,
#'     TSPARMCD = "AEDICT",
#'     TSVAL = "    meddra    23.0   ",
#'     TSVAL2 = ""
#' )
#'
#' check_ts_aedict(TS1)
#' check_ts_aedict(TS2)
#' check_ts_aedict(TS3)
#' check_ts_aedict(TS4)
#' check_ts_aedict(TS5)
#' check_ts_aedict(TS6)
#' check_ts_aedict(rbind(TS1,TS1))
#'

check_ts_aedict <- function(TS){
    ###First check that required variables exist and return a message if they don't
    if(TS %lacks_any% c("TSPARMCD", "TSVAL")){

        fail(lacks_msg(TS, c("TSPARMCD", "TSVAL")))

    }else{

        ### identify current MedDRA version

        t <-  Sys.Date()
        y <-  substring(t,1,4)

        may <- seq(as.Date(paste0(y, "-05-01")),as.Date(paste0(y, "-05-07")),by="1 day")
        date1 <-  may[weekdays(may) == "Monday"]

        nov <- seq(as.Date(paste0(y, "-11-01")),as.Date(paste0(y, "-11-07")),by="1 day")
        date2 <- nov[weekdays(nov) == "Monday"]

        # Work out the main MedDRA version of the year; start on version 24.0

        meddra_version <- as.numeric(y) - 2021 + 24

        # if today is before the first Monday of May, MedDRA version is the previous version, eg 23.1
        # if today is after the first Monday of November, MedDRA version is the next version, eg 24.1
        meddra_version_num <- case_when(t < date1 ~ as.character(meddra_version - 0.9),
                                        t >= date1 & t< date2 ~ paste0(meddra_version, ".0"),
                                        t >= date2 ~ as.character(meddra_version + 0.1))

        ### Subset TS to only AEDICT record
        mydf <- subset(TS, TS$TSPARMCD=="AEDICT", c("TSPARMCD","TSVAL"))
        ###Print to report

        ### Pass if MedDRA version is populated
        if (nrow(mydf) == 1) {

            ###Adding calculated MedDRA Version so it is included in the dataframe that is returned if the check fails
            mydf <- mydf %>%
                mutate(Current_MedDRA_version = meddra_version_num)

            meddra=mydf %>% select(TSVAL) %>%
                pull() %>% trimws()
            meddra_short <- substr(meddra,nchar(as.character(meddra))-3,nchar(as.character(meddra)))

            if (is_sas_na(meddra)) {
                fail("No value in TS.TSVAL where TS.TSPARMCD=AEDICT.", mydf)
            }
            else if(meddra_short != meddra_version_num){
                fail(paste("MedDRA version in TS.TSVAL where TS.TSPARMCD=AEDICT is not the latest version as of",Sys.Date()), mydf)
            }
            else {
                if (grepl("MEDDRA", toupper(mydf$TSVAL), ignore.case=TRUE)) {
                    pass()
                } else {
                    fail("The string 'MedDRA' not found in TS.TSVAL where TS.TSPARMCD=AEDICT.", mydf)
                }
            }
            ### Return message if MedDRA version is not populated in TS or multiple records
        }else if (nrow(mydf) < 1){
            fail("No record where TS.TSPARMCD=AEDICT.")
        }else if (nrow(mydf) > 1){
            fail("Multiple records where TS.TSPARMCD=AEDICT.",mydf)
        }
    }
}
