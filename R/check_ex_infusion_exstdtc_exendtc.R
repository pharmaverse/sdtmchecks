#' @title Check that an infusion drug has same start/end exposure dates, also including missing start/end dates
#'
#' @description This check identifies that an infusion drug has same EXSTDTC and EXENDTC dateparts.
#' If time is available for both dates, also check that end time is after start time.
#' Missing start/end dates are also included.
#'
#' @param EX Exposure SDTM dataset with variables USUBJID,EXTRT,EXSTDTC,EXENDTC,EXROUTE
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @importFrom dplyr %>% filter case_when
#'
#' @export
#'
#' @author Anastasiia Khmelnytska, Stella Banjo(HackR 2021)
#'
#' @examples
#'
#' EX <- data.frame(
#'  STUDYID = 1,
#'  USUBJID = 1:12,
#'  EXTRT = "SOME DRUG",
#'  EXROUTE = "INTRAVENOUS",
#'  EXSTDTC = c("2017-01-01","2017-01-02","2017-01-01T14:36","2015","2017-02","2017"      ,""    ,
#'              "2017"   ,"2017-01-01T14:26","2017-01-01T14:26","2017-01-01T14","2017-01-01T14:26:01")
#'              ,
#'  EXENDTC = c("2017-01-01","2017-01-03","2017-01-01T14:35","2017","2017-01","2016-01-01","2000",
#'              "2017-02","2017-01-01"      ,"2017-01","2017-01-01T13","2017-01-02T14:26:02")
#'              ,
#'  EXOCCUR = "Y",
#'  VISIT = "CYCLE 1 DAY 1",
#'  stringsAsFactors=FALSE
#' )
#'
#'  check_ex_infusion_exstdtc_exendtc(EX)
#'
#'  EX2 <- data.frame(
#'  STUDYID = 1,
#'  USUBJID = 1:4,
#'  EXTRT = "SOME DRUG",
#'  EXROUTE = "INTRAVENOUS",
#'  EXSTDTC = c("2017-01-03", "", "2017-02-01T14:26", ""),
#'  EXENDTC = c("", "2017-02-03", "", "2017-02-02T14:26:02"),
#'  EXOCCUR = "Y",
#'  VISIT = "CYCLE 1 DAY 1",
#'  stringsAsFactors = FALSE
#'  )
#'
#'  check_ex_infusion_exstdtc_exendtc(EX2)
#'
#'  EX3 <- data.frame(
#'      STUDYID = 1,
#'      USUBJID = 1:3,
#'      EXTRT = "SOME DRUG",
#'      EXROUTE = "INTRAVENOUS",
#'      EXSTDTC = c("2017-01-01", "2017-01-01T14:26", "2017-01-01T14:26"),
#'      EXENDTC = c("2017-01-01", "2017-01-01", "2017-01"),
#'      EXOCCUR = "Y",
#'      VISIT = "CYCLE 1 DAY 1",
#'      stringsAsFactors=FALSE
#'  )
#'
#'  check_ex_infusion_exstdtc_exendtc(EX3)
#'


check_ex_infusion_exstdtc_exendtc <- function(EX) {

    ###First check that required variables exist and return a message if they don't
    if(EX %lacks_any% c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC", "EXROUTE" )){

        fail(lacks_msg(EX, c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC", "EXROUTE" )))

    } else {
        if ("EXOCCUR" %in% names(EX)) {
            df <- EX %>% filter(EXOCCUR == "Y")
        } else {
            df <- EX
        }

        ## Get minimum length for when EXSTDTC and EXENDTC are different lengths
        df$startdtc = ifelse(!is_sas_na(df$EXSTDTC),
                             substr(df$EXSTDTC, 1, pmin(nchar(df$EXSTDTC), nchar(df$EXENDTC), na.rm = TRUE)),
                             df$EXSTDTC)

        df$startdtc = ifelse(is_sas_na(df$EXENDTC),
                             substr(df$EXSTDTC, 1, pmin(nchar(df$EXSTDTC), nchar(df$EXSTDTC), na.rm = TRUE)),
                             df$EXSTDTC)

        df$enddtc = ifelse(!is_sas_na(df$EXENDTC),
                           substr(df$EXENDTC, 1, pmin(nchar(df$EXSTDTC), nchar(df$EXENDTC), na.rm = TRUE)),
                           df$EXENDTC)

        df$enddtc = ifelse(is_sas_na(df$EXSTDTC),
                           substr(df$EXENDTC, 1, pmin(nchar(df$EXENDTC), nchar(df$EXENDTC), na.rm = TRUE)),
                           df$EXENDTC)


        df$startdate = substr(df$startdtc, 1, 10)
        df$enddate   = substr(df$enddtc, 1, 10)

        df$starttime = substr(df$startdtc, 12, 19)
        df$endtime   = substr(df$enddtc, 12, 19)


        # convert string to date/time
        df$DT1 = NA
        df$DT1[nchar(df$startdate) == 10] <- as.POSIXct(df$startdate[nchar(df$startdate) == 10], format = "%Y-%m-%d")
        df$DT1[nchar(df$startdate) == 7]  <- as.POSIXct(df$startdate[nchar(df$startdate) == 7], format = "%Y-%M")
        df$DT1[nchar(df$startdate) == 4]  <- as.POSIXct(df$startdate[nchar(df$startdate)== 4], format = "%Y")

        df$DT2 = NA
        df$DT2[nchar(df$enddate) == 10] <- as.POSIXct(df$enddate[nchar(df$enddate) == 10], format = "%Y-%m-%d")
        df$DT2[nchar(df$enddate) == 7]  <- as.POSIXct(df$enddate[nchar(df$enddate) == 7], format = "%Y-%M")
        df$DT2[nchar(df$enddate) == 4]  <- as.POSIXct(df$enddate[nchar(df$enddate) == 4], format = "%Y")

        df$TM1 = NA
        df$TM1[nchar(df$starttime) == 8] <- as.POSIXct(df$starttime[nchar(df$starttime) == 8], format = "%H:%M:%S")
        df$TM1[nchar(df$starttime) == 5] <- as.POSIXct(df$starttime[nchar(df$starttime) == 5], format = "%H:%M")
        df$TM1[nchar(df$starttime) == 2] <- as.POSIXct(df$starttime[nchar(df$starttime)==2], format = "%H")

        df$TM2 = NA
        df$TM2[nchar(df$endtime)==8] <- as.POSIXct(df$endtime[nchar(df$endtime) == 8], format = "%H:%M:%S")
        df$TM2[nchar(df$endtime)==5] <- as.POSIXct(df$endtime[nchar(df$endtime) == 5], format = "%H:%M")
        df$TM2[nchar(df$endtime)==2] <- as.POSIXct(df$endtime[nchar(df$endtime) == 2], format = "%H")

        # Include VISIT and EXOCCUR in display if they exist in the data set
        myvars = c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")

        if (EX %has_any% "VISIT") {

            myvars = c(myvars, "VISIT")

        }

        # Start date/time after end date/time, including missing start/end dates
        # Create issue flag types

        # filter records where route is intravenous
        mydf0 <- subset(df, grepl("INTRAVENOUS", toupper(df$EXROUTE)))

        # create check flags for date checks
        mydf1 <- mydf0
        mydf1$check_stat <- case_when(
            mydf0$DT1 != mydf0$DT2 & is_sas_na(mydf0$TM1) & is_sas_na(mydf0$TM2) ~ "Different Start/End date for Infusion",
            !is_sas_na(mydf0$DT1) & is_sas_na(mydf0$TM1) & is_sas_na(mydf0$EXENDTC) ~ "Missing End date",
            !is_sas_na(mydf0$DT2) & is_sas_na(mydf0$TM2) & is_sas_na(mydf0$EXSTDTC) ~ "Missing Start date",
            TRUE ~ ""
        )

        # filter empty check flags
        mydf1 <- select(mydf1, all_of(myvars), check_stat) %>%
            filter(check_stat != "")


        # create check flags for date and time checks
        mydf2 <- mydf0
        mydf2$check_stat <- case_when(
            mydf0$DT1 == mydf0$DT2 & mydf0$TM1 > mydf0$TM2 ~ "Same Start/End date but Start time after End time",
            mydf0$DT1 != mydf0$DT2 & !is_sas_na(mydf0$TM1) & !is_sas_na(mydf0$TM2) ~ "Different Start/End date for Infusion",
            !is_sas_na(mydf0$DT1) & !is_sas_na(mydf0$TM1) & is_sas_na(mydf0$EXENDTC) ~ "Missing End date/time",
            !is_sas_na(mydf0$DT2) & !is_sas_na(mydf0$TM2) & is_sas_na(mydf0$EXSTDTC) ~ "Missing Start date/time",
            TRUE ~ ""
        )

        # filter empty check flags
        mydf2 <- select(mydf2, all_of(myvars), check_stat) %>%
            filter(check_stat != "")

        rownames(mydf1) = NULL
        rownames(mydf2) = NULL

        ### Return message if no records with issue
        if (nrow(mydf1) == 0 && nrow(mydf2) == 0) {

            pass()

            ### Return subset dataframe if there are issues with start date/time of IV
        } else {

            stackeddf = rbind(mydf1, mydf2)
            stackeddf <- stackeddf %>%
                mutate(check_flag = case_when(
                    check_stat == "Different Start/End date for Infusion" ~ "A",
                    check_stat == "Missing End date" ~ "B",
                    check_stat == "Missing Start date" ~ "C",
                    check_stat == "Same Start/End date but Start time after End time" ~ "D",
                    check_stat == "Different Start/End date for Infusion" ~ "E",
                    check_stat == "Missing End date/time" ~ "F",
                    check_stat == "Missing Start date/time" ~ "G"
                ))

            if (nrow(mydf1) > 0) {
                msg1 = paste(nrow(mydf1), " record(s) with issues on date checks", sep = "")
            }
            if (nrow(mydf2) > 0) {
                msg2 = paste(nrow(mydf2), " record(s) with issues on date/time checks", sep = "")
            }
            if (nrow(mydf1) > 0 && nrow(mydf2) > 0) {
                fmsg = paste("EX has ", msg1, " and ", msg2, ". ", sep = "")
            }
            else if (nrow(mydf1) > 0) {
                fmsg = paste("EX has ", msg1, ". ", sep = "")
            }
            else if (nrow(mydf2) > 0) {
                fmsg = paste("EX has ", msg2, ". ", sep = "")
            }
            fail(fmsg, stackeddf)
        }
    }
}
