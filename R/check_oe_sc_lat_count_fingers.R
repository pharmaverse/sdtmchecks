#' @title Check if Post Treatment Count Fingers in Study Eye laterality does not match with Subject Characteristics Study Eye laterality
#'
#' @description Check If Post Treatment Count Fingers in Study Eye is done on the actual Study eye by comparing laterality from OE domain with SC domain.
#' Check is ignored if Post Treatment Count Fingers is not collected in study as it is quite common for EP studies
#'
#' @param SC Subject Characteristics Dataset for Ophtho Study with variables USUBJID, SCTEST, SCTESTCD, SCCAT, SCORRES, SCDTC
#' @param OE Ophthalmic Examination Dataset for Ophtho Study with variables USUBJID, OECAT, OELAT, VISIT, OEDTC, OETEST, OELOC, OESTAT (if present)
#'
#' @importFrom dplyr %>% filter mutate select
#'
#' @family OPHTH
#'
#' @keywords OPHTH
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @author Monarch Shah (HackR 2021 Team Eye)
#'
#' @examples
#'
#' sc <- data.frame(USUBJID  = c(1,1,1,2,2,2),
#'                  SCTEST   = c("Eye Meeting Eligibility Criteria",
#'                               "Focus of Study-Specific Interest",
#'                               " ",
#'                               "Eye Meeting Eligibility Criteria",
#'                               "Focus of Study-Specific Interest",
#'                               " "),
#'                  SCTESTCD = c("ELIGEYE", "FOCID", "", "ELIGEYE", "FOCID", ""),
#'                  SCCAT    = c("STUDY EYE SELECTION", "STUDY EYE SELECTION", "",
#'                               "STUDY EYE SELECTION", "STUDY EYE SELECTION", ""),
#'                  SCORRES  = c("LEFT", "OS", "", "RIGHT", "OD", ""),
#'                  SCDTC    = rep("2021-01-01", 6),
#'                  stringsAsFactors = FALSE)
#'
#' oe <- data.frame(USUBJID = c(1,1,1,1,1,2,2,2,2,2,2),
#'                  OECAT  = rep("SAFETY ASSESSMENT OF LOW VISION", 11),
#'                  OELOC   = rep("Eye", 11),
#'                  OELAT   = c("LEFT", "Left", "left", "LEFT", "LEFT",
#'                              "RIGHT", "right", "right", "RIGHT", "RIGHT", "right"),
#'                  OEDY  = c(1, 28, 56, 84, 112, 1, 28, 56, 84, 112, 140),
#'                  VISIT   = c("Week 1", "Week 4", "Week 8", "Week 12", "Week 16",
#'                              "Week 1", "Week 4", "Week 8", "Week 12", "Week 16", "Week 20"),
#'                  OEDTC = c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01",
#'                            "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01",
#'                            "2021-06-01"),
#'                  OETEST = c("A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "F"),
#'                  stringsAsFactors=FALSE)
#'
#' check_oe_sc_lat_count_fingers(SC=sc, OE=oe)
#'
#' sc <- data.frame(USUBJID  = c(1,1,1,2,2,2),
#'                  SCTEST   = c("Eye Meeting Eligibility Criteria",
#'                               "Focus of Study-Specific Interest",
#'                               " ",
#'                               "Eye Meeting Eligibility Criteria",
#'                               "Focus of Study-Specific Interest",
#'                               " "),
#'                  SCTESTCD = c("ELIGEYE", "FOCID", "", "ELIGEYE", "FOCID", ""),
#'                  SCCAT    = c("STUDY EYE SELECTION", "STUDY EYE SELECTION", "",
#'                               "STUDY EYE SELECTION", "STUDY EYE SELECTION", ""),
#'                  SCORRES  = c("LEFT", "OS", "", "RIGHT", "OD", ""),
#'                  SCDTC    = rep("2021-01-01", 6),
#'                  stringsAsFactors = FALSE)
#'
#' oe <- data.frame(USUBJID = c(1,1,1,1,1,2,2,2,2,2,2),
#'                  OECAT  = rep("SAFETY ASSESSMENT OF LOW VISION", 11),
#'                  OELOC   = rep("Eye", 11),
#'                  OELAT   = c("LEFT", "Left", "left", "LEFT", "right", "RIGHT",
#'                              "right", "right", "RIGHT", "RIGHT", "left"),
#'                  OEDY  = c(1, 28, 56, 84, 112, 1, 28, 56, 84, 112, 140),
#'                  VISIT   = c("Week 1", "Week 4", "Week 8", "Week 12", "Week 16",
#'                              "Week 1", "Week 4", "Week 8", "Week 12", "Week 16",
#'                              "Week 20"),
#'                  OEDTC = c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01",
#'                            "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01",
#'                            "2021-06-01"),
#'                  OETEST = c("A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "F"),
#'                  stringsAsFactors=FALSE)
#'
#' check_oe_sc_lat_count_fingers(SC=sc, OE=oe)
#'
#' sc <- data.frame(USUBJID  = c(1,1,1,2,2,2,3),
#'                  SCTEST   = c("Eye Meeting Eligibility Criteria",
#'                               "Focus of Study-Specific Interest",
#'                               " ",
#'                               "Eye Meeting Eligibility Criteria",
#'                               "Focus of Study-Specific Interest",
#'                               " ",
#'                               "Focus of Study-Specific Interest"),
#'                  SCTESTCD = c("ELIGEYE", "FOCID", "", "ELIGEYE", "FOCID", "", "FOCID"),
#'                  SCCAT    = c("STUDY EYE SELECTION", "STUDY EYE SELECTION", "",
#'                               "STUDY EYE SELECTION", "STUDY EYE SELECTION",
#'                               "", "STUDY EYE SELECTION"),
#'                  SCORRES  = c("LEFT", "OS", "", "RIGHT", "OD", "", "OS"),
#'                  SCDTC    = "2021-01-01",
#'                  stringsAsFactors = FALSE)
#'
#' oe <- data.frame(USUBJID = c(1,1,1,1,1,2,2,2,2,2,2),
#'                  OESTAT  = c("","","","","","","","","","", "not DONE"),
#'                  OECAT  = "SAFETY ASSESSMENT OF LOW VISION",
#'                  OELOC   = "Eye",
#'                  OELAT   = c("LEFT", "Left", "left", "LEFT", "right", "RIGHT",
#'                              "right", "right", "RIGHT", "RIGHT", "left"),
#'                  OEDY  = c(1, 28, 56, 84, 112, 1, 28, 56, 84, 112, 140),
#'                  VISIT   = c("Week 1", "Week 4", "Week 8", "Week 12", "Week 16",
#'                              "Week 1", "Week 4", "Week 8", "Week 12", "Week 16",
#'                              "Week 20"),
#'                  OEDTC = c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01",
#'                            "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01",
#'                            "2021-06-01"),
#'                  OETEST = c("A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "F"),
#'                  stringsAsFactors=FALSE)
#'
#' check_oe_sc_lat_count_fingers(SC=sc, OE=oe)
#'


check_oe_sc_lat_count_fingers <- function(OE, SC) {

    if (SC %lacks_any% c("USUBJID", "SCTEST", "SCTESTCD", "SCCAT", "SCORRES", "SCDTC")) {

        fail(lacks_msg(SC, c("USUBJID","SCTEST","SCTESTCD","SCCAT","SCORRES", "SCDTC")))

    }

    else if (OE %lacks_any% c("USUBJID", "OECAT", "OELAT", "VISIT", "OEDTC",  "OETEST")) {

        fail(lacks_msg(OE, c("USUBJID",  "OECAT", "OELAT", "VISIT", "OEDTC", "OETEST")))

    }

    else if (OE %>% filter( OECAT == "SAFETY ASSESSMENT OF LOW VISION" & toupper(OELOC) == "EYE" & toupper(OELAT) %in% c("LEFT", "RIGHT")) %>% nrow() == 0){
        pass()
    }


    else {

        # Subset SC data on SC.SCCAT = "STUDY EYE SELECTION", and  and SCTESTCD = "FOCID". SC.SCORRES OS (oculus sinister) means the left
        #       eye and OD (oculus dextrus) means the right eye.
        SC <- SC %>% select(USUBJID, SCTESTCD, SCTEST, SCCAT, SCORRES, SCDTC) %>%
            filter(SCTESTCD %in% c("FOCID")) %>%
            mutate(SC_STUDYEYE = ifelse(SCORRES == "OS", "LEFT",
                                ifelse(SCORRES == "OD", "RIGHT", "SCORRES Value not OS or OD")))

        # Remove OESTAT NOT DONE
        if ((any(names(OE) == "OESTAT")) == TRUE) {

            OE <- OE %>% filter(toupper(OESTAT) != "NOT DONE" )

        }

        #perm_var <- c("OELOC")
        #int_var <- intersect(names(OE), perm_var)

        my_select_var <- c("USUBJID", "OELOC", "OECAT", "OELAT", "VISIT", "OEDTC", "OETEST")


        # Subset OE on OECAT = SAFETY ASSESSMENT OF LOW VISIT, OELOC = EYE, OESTAT NOT EQ "NOT DONE" & OELAT in LEFT or RIGHT
        OE <- OE[, my_select_var]
        OE <- OE %>% filter(toupper(OECAT) == "SAFETY ASSESSMENT OF LOW VISION" & toupper(OELOC) == "EYE" & toupper(OELAT) %in% c("LEFT", "RIGHT") ) # Remove Missing OELAT

        # Merge SC & OE by USUBJID & Create a variable when STUDYEYE do not match with OELAT
        mydf1 <- left_join(SC, OE, by="USUBJID") %>%
            mutate(MISFLAG = ifelse((toupper(SC_STUDYEYE) != toupper(OELAT)) | is.na(toupper(OELAT)), 1, 0))

        mydf = mydf1 %>% filter(MISFLAG == 1) %>% select(-MISFLAG, -SCTESTCD, -SCTEST, -SCCAT, -SCORRES, -OELOC, -OECAT)

        if ((nrow(mydf) > 0 ) == FALSE) {
            pass()
        } else {
            fail(paste0(nrow(mydf), " record(s) with Post Treatment Count Fingers - Study Eye laterality does not match with Subject Characteristics Study Eye laterality. "), mydf )
        }

    }
}
