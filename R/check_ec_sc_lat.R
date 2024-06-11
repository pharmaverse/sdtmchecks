#' @title Check if Study Drug is not administered in the Study Eye
#'
#' @description Check if Study Drug is not administered in the Study Eye.
#'    1.> Subset Exposure dataset (EC) for only ocular Study Drug Administration records,
#'    and pass the check if there are none. If EC.ECCAT variable is available then remove records
#'    containing EC.ECCAT = “FELLOW”. If EC.ECCAT variable is not available then include all records,
#'    assuming drug administration is collected for study eye only.
#'    2.> Subset Subject Characteristics dataset (SC) for only Study Eye Selection
#'    3.> Compare Exposure dataset laterality (EC.ECLAT) with Subject Characteristics dataset laterality
#'    (SC.SCORRES - OS = LEFT, OD = RIGHT) and report if there is any mismatch.
#'
#' @param SC Subject Characteristics Dataset for Ophtha Study with variables
#'           USUBJID, SCTEST, SCTESTCD, SCCAT, SCORRES, SCDTC
#' @param EC Subject Exposure Dataset with variables
#'           USUBJID, ECCAT (if available), ECLOC, ECMOOD, ECLAT, ECSTDY, VISIT, ECSTDTC, ECOCCUR, ECROUTE
#'
#' @importFrom dplyr %>% filter mutate select
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
#'                  SCDTC    = "2021-01-01",
#'                  stringsAsFactors = FALSE)
#'
#' ec <- data.frame(USUBJID = c(1,1,1,1,1,2,2,2,2,2,2),
#'                  ECCAT   = c("Fellow", "Study", "Study", "Study", "StudY",
#'                              "Fellow", "Fellow", "STUDY", "STUDY", "STUDY", ""),
#'                  ECMOOD  = rep("Performed", 11),
#'                  ECLOC   = rep("Eye", 11),
#'                  ECLAT   = c("LEFT", "Left", "left", "LEFT", "LEFT", "RIGHT",
#'                              "right", "right", "RIGHT", "RIGHT", "right"),
#'                  ECSTDY  = c(1, 28, 56, 84, 112, 1, 28, 56, 84, 112, 140),
#'                  VISIT   = c("Week 1", "Week 4", "Week 8", "Week 12", "Week 16",
#'                              "Week 1", "Week 4", "Week 8", "Week 12", "Week 16", "Week 20"),
#'                  ECSTDTC = c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01",
#'                              "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01",
#'                              "2021-06-01"),
#'                  ECOCCUR = "Y",
#'                  ECROUTE = "INTRAVITREAL",
#'                  stringsAsFactors=FALSE)
#'
#' check_ec_sc_lat(SC=sc, EC=ec)
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
#'                  SCDTC    = "2021-01-01",
#'                  stringsAsFactors = FALSE)
#'
#' ec <- data.frame(USUBJID = c(1,1,1,1,1,2,2,2,2,2,2),
#'                  ECCAT   = c("Fellow", "Study", "Study", "Study", "StudY",
#'                              "Fellow", "Fellow", "STUDY", "STUDY", "STUDY", ""),
#'                  ECMOOD  = rep("Performed", 11),
#'                  ECLOC   = rep("Eye", 11),
#'                  ECLAT   = c("LEFT", "Left", "left", "LEFT", "RIGHT", "RIGHT",
#'                              "right", "right", "RIGHT", "RIGHT", "left"),
#'                  ECSTDY  = c(1, 28, 56, 84, 112, 1, 28, 56, 84, 112, 140),
#'                  VISIT   = c("Week 1", "Week 4", "Week 8", "Week 12", "Week 16",
#'                              "Week 1", "Week 4", "Week 8", "Week 12", "Week 16", "Week 20"),
#'                  ECSTDTC = c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01",
#'                              "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01",
#'                              "2021-06-01"),
#'                  ECOCCUR = "Y",
#'                  ECROUTE = "OPHTHALMIC",
#'                  stringsAsFactors=FALSE)
#'
#' check_ec_sc_lat(SC=sc, EC=ec)
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
#'                               "STUDY EYE SELECTION",
#'                               "STUDY EYE SELECTION", "", "STUDY EYE SELECTION"),
#'                  SCORRES  = c("LEFT", "OS", "", "RIGHT", "OD", "", "RIGHT"),
#'                  SCDTC    = "2021-01-01",
#'                  stringsAsFactors = FALSE)
#'
#' ec <- data.frame(USUBJID = c(1,1,1,1,1,2,2,2,2,2,2),
#'                  ECMOOD  = "Performed",
#'                  ECLOC   = "Eye",
#'                  ECLAT   = c("LEFT", "Left", "left", "LEFT", "RIGHT", "RIGHT",
#'                              "right", "right", "RIGHT", "RIGHT", "left"),
#'                  ECSTDY  = c(1, 28, 56, 84, 112, 1, 28, 56, 84, 112, 140),
#'                  VISIT   = c("Week 1", "Week 4", "Week 8", "Week 12", "Week 16",
#'                              "Week 1", "Week 4", "Week 8", "Week 12", "Week 16", "Week 20"),
#'                  ECSTDTC = c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01",
#'                              "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01",
#'                              "2021-06-01"),
#'                  ECOCCUR = c("Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "N"),
#'                  stringsAsFactors=FALSE)
#'
#' check_ec_sc_lat(SC=sc, EC=ec)
#'
#'
#'

check_ec_sc_lat <- function(EC,SC) {

    if (SC %lacks_any% c("USUBJID", "SCTEST", "SCTESTCD", "SCCAT", "SCORRES", "SCDTC")) {

        fail(lacks_msg(SC, c("USUBJID","SCTEST","SCTESTCD","SCCAT","SCORRES", "SCDTC")))

    }

    else if (EC %lacks_any% c("USUBJID", "ECMOOD", "ECSTDY", "VISIT", "ECSTDTC", "ECOCCUR", "ECROUTE")) {

        fail(lacks_msg(EC, c("USUBJID", "ECMOOD", "ECSTDY", "VISIT", "ECSTDTC", "ECOCCUR", "ECROUTE")))

    }

    else {

        # Filter for records where Study Drug was administered in the eye
        EC <- EC %>% filter(ECROUTE %in% c("CONJUNCTIVAL","INTRACAMERAL", "INTRACORNEAL",
                                           "INTRAOCULAR","INTRAVITREAL", "OPHTHALMIC",
                                           "RETROBULBAR", "SUBRETINAL",
                                           "SUBTENON", "SUBCONJUNCTIVAL"))

        # Check whether EC has any records where study drug was administered in the eye - if not then check passes
        if (nrow(EC) == 0){

            pass()

        }

        else {

            # Now that we know that Study drug is administered in the eye, check for ECLAT and ECLOC
            if (EC %lacks_any% c("ECLAT", "ECLOC")) {

                fail(lacks_msg(EC, c("ECLAT", "ECLOC")))

            }

            # Subset SC data on SC.SCCAT = "STUDY EYE SELECTION", and
            # SCTESTCD = "FOCID". SC.SCORRES OS (oculus sinister) means the left
            # eye and OD (oculus dextrus) means the right eye.
            SC <- SC %>%
                select(USUBJID, SCTESTCD, SCTEST, SCCAT, SCORRES, SCDTC) %>%
                filter(toupper(SCTESTCD) == "FOCID") %>%
                mutate(SC_STUDYEYE = ifelse(toupper(SCORRES) == "OS", "LEFT",
                                            ifelse(toupper(SCORRES) == "OD", "RIGHT","SCORRES Value not OS or OD")))

            # Subset EC data on EC.ECCAT (if available) remove Fellow Eye Records,
            # EC.ECMOOD = "PERFORMED" EC.ECOCCUR = "Y"
            if (any(names(EC) == "ECCAT") == TRUE) {
                EC <- EC %>% filter(toupper(ECMOOD) == "PERFORMED" & ECOCCUR == "Y" &
                                        toupper(ECLOC) == "EYE" & !grepl("FELLOW", ECCAT, ignore.case=TRUE))
            }
            else if (any(names(EC) == "ECCAT") == FALSE) {
                EC <- EC %>% filter(toupper(ECMOOD) == "PERFORMED" & ECOCCUR == "Y" &
                                        toupper(ECLOC) == "EYE")
            }

            perm_var <- c("ECCAT")
            int_var <- intersect(names(EC), perm_var)

            my_select_var <- c("USUBJID", int_var, "ECROUTE", "ECLOC", "ECMOOD",
                               "ECLAT", "ECSTDY", "VISIT", "ECSTDTC", "ECOCCUR")

            EC <- EC[,my_select_var]

            # Merge SC & EC by USUBJID & Create a variable when STUDYEYE do not match with ECLAT
            mydf1 <- full_join(SC, EC, by="USUBJID") %>%
                mutate(MISFLAG = ifelse((toupper(SC_STUDYEYE) != toupper(ECLAT)) | is.na(toupper(ECLAT)) , 1, 0))

            mydf = mydf1 %>% filter(MISFLAG == 1)%>%
                filter(ECROUTE != "NON-OCULAR ROUTE" | is.na(ECROUTE)) %>%
                select(-MISFLAG, -SCTESTCD, -SCTEST, -SCCAT, -ECLOC, -ECMOOD, -ECOCCUR, -SCORRES)

            if ((nrow(mydf) > 0 ) == FALSE) {
                pass()
            } else {
                fail(paste0(nrow(mydf), " record(s) with Study Drug not administered in the Study Eye. "), mydf )
            }
        }
    }
}

