#' @title Check for missing WHODrug version in TS
#'
#' @description This check looks for missing WHODrug version;
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
#' @author Antony Howard (HackR 2021 Team Pentraxin1)
#'
#' @examples
#'
#' TS1 <- data.frame(
#'  STUDYID = 1,
#'  TSPARMCD = "CMDICT",
#'  TSVAL = "WHODRUG GLOBAL B3 MARCH 1, 2021",
#'  TSVAL2 = ""
#' )
#'
#' TS2 <- data.frame(
#'  STUDYID = 2,
#'  TSPARMCD = "CMDICT",
#'  TSVAL = "",
#'  TSVAL1 = "WHODRUG GLOBAL B3 MARCH 1, 2021"
#' )
#'
#' TS3 <- data.frame(
#'  STUDYID = 3,
#'  TSPARMCD = "CMDICT",
#'  TSVAL = ""
#' )
#'
#' TS4 <-data.frame(
#'  STUDYID = 4,
#'  TSPARMCD = "AEDICT",
#'  TSVAL = ""
#' )
#'
#' TS5 <- data.frame(
#'     STUDYID = 5,
#'     TSPARMCD = "CMDICT",
#'     TSVAL = "meddra 24.0",
#'     TSVAL2 = ""
#' )
#'
#' TS6 <- data.frame(
#'  STUDYID = 6,
#'  TSPARMCD = "CMDICT",
#'  TSVAL = "WHODRUG vGLOBAL B3 MARCH 1, 2021",
#'  TSVAL2 = ""
#' )
#'
#' check_ts_cmdict(TS1)
#' check_ts_cmdict(TS2)
#' check_ts_cmdict(TS3)
#' check_ts_cmdict(TS4)
#' check_ts_cmdict(TS5)
#' check_ts_cmdict(TS6)
#' check_ts_cmdict(rbind(TS1,TS1))

check_ts_cmdict <- function(TS){

    ###First check that required variables exist and return a message if they don't
    if(TS %lacks_any% c("TSPARMCD", "TSVAL")){

        fail(lacks_msg(TS, c("TSPARMCD", "TSVAL")))

    }else{

        ### identify current WHODrug version

        t <-  Sys.Date()
        y <-  substring(t,1,4)

        may <- seq(as.Date(paste0(y, "-05-01")),as.Date(paste0(y, "-05-07")),by="1 day")
        date1 <-  may[weekdays(may) == "Monday"]

        nov <- seq(as.Date(paste0(y, "-11-01")),as.Date(paste0(y, "-11-07")),by="1 day")
        date2 <- nov[weekdays(nov) == "Monday"]

        # Work out the main WHODrug version of the year eg March 1, 2021

        # if today is before the first Monday of May, WHODrug version is the previous version, ie SEPTEMBER 1, 2020
        # if today is after the first Monday of November, WHODrug version is the next version, ie SEPTEMBER 1, 2021
        whodrug_ver <- case_when(t < date1 ~ paste0("SEPTEMBER 1, ", as.numeric(y)-1),
                                 t >= date1 & t< date2 ~ paste0("MARCH 1, ", as.numeric(y)),
                                 t>= date2 ~ paste0("SEPTEMBER 1, ", as.numeric(y))
        )

        version <- paste0("WHODRUG GLOBAL B3 ",whodrug_ver)

        ### Subset TS to only CMDICT record
        mydf <- subset(TS, TS$TSPARMCD=="CMDICT", c("TSPARMCD","TSVAL"))
        ###Print to report

        ### Pass if WHODrug version is populated
        if (nrow(mydf) == 1) {

            ###Adding calculated WHODrug Version so it is included in the dataframe that is returned if the check fails
            mydf <- mydf %>%
                mutate(Current_WHODRUG_ver = version)

            whodrug=mydf %>% select(TSVAL) %>%
                pull() %>% trimws()

            if (is_sas_na(whodrug)) {
                fail("No value in TS.TSVAL where TS.TSPARMCD=CMDICT. ", mydf)
            }

            else if(toupper(whodrug) != toupper(version)  ){
                fail(paste("WHODrug version in TS.TSVAL where TS.TSPARMCD=CMDICT is not latest version as of",Sys.Date(),"or not an exact string match."), mydf)
            }
            else {
                if (grepl("WHODRUG", toupper(mydf$TSVAL), ignore.case=TRUE)) {
                    pass()
                } else {
                    fail("The string `WHODRUG` not found in TS.TSVAL where TS.TSPARMCD=CMDICT. ", mydf)
                }
            }
        ### Return message if WHODrug version is not populated in TS
        }else if (nrow(mydf) < 1){
            fail("No record where TS.TSPARMCD=CMDICT. ")
        }else if (nrow(mydf) > 1){
            fail("Multiple records where TS.TSPARMCD=CMDICT. ",mydf)
        }
    }
}

