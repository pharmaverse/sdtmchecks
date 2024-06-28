#' @title Check if 1m BCVA test is completed per BCVA 4m result
#'
#' @description This ophthalmology function is to check if BCVA 1m test is done per BCVA 4m result.
#' Patient, Visits, Laterality where Low Vision Tests were done are excluded from this check.
#' 1> If 4m test total <= 19 and 1m test is not done.
#' 2> If 4m test total >= 20 and 1m test is performed
#' Above two conditions will be outputted in the final result data frame, which includes USUBJID, VISIT,
#' OEDTC, OELAT, BCVA_4M_TOTAL, BCVA_1M_TOTAL, ISSUE.
#' Please note that this check will assume that the BCVA 1m and 4m total are accurate and they happen on the same day.
#' If they are happening on different dates, such records will be removed and not checked.
#'
#' @param OE Ophtho Dataset with variables USUBJID, OECAT, OESCAT, OETSTDTL, OESTRESN, OESTAT, OELAT,
#' OERESCAT, VISIT, OEDTC, OEDY
#'
#' @importFrom dplyr %>% filter mutate select lag lead rename arrange summarise group_by ungroup
#'
#' @family OPHTH
#'
#' @keywords OPHTH
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @author Rosemary Li (HackR 2021 Team Eye)
#'
#' @examples
#' OE_1m_done <- data.frame(
#'   USUBJID = "1",
#'   OECAT = "BEST CORRECTED VISUAL ACUITY",
#'   OETSTDTL = c(rep("TESTING DISTANCE: 4M", 4), rep("TESTING DISTANCE: 1M", 3)),
#'   OESCAT = c(rep("", 3), "TOTAL", rep("", 2), "TOTAL"),
#'   OESTAT = rep("", 7),
#'   OERESCAT = c("ROW 1 - SNELLEN 20/200",
#'                "ROW 2 - SNELLEN 20/160",
#'                "ROW 3 - SNELLEN 20/125",
#'                "",
#'                "ROW 1 - SNELLEN 20/200",
#'                "ROW 2 - SNELLEN 20/160",
#'                ""),
#'   VISIT = "WEEK 1",
#'   VISITNUM = 5,
#'   OEDTC = "2020-06-01",
#'   OEDY = 8,
#'   OELOC = "EYE",
#'   OELAT = "LEFT",
#'   OESTRESN = c(9, 9, 3, 21, 3, 2, 5)
#' )
#' check_oe_bcva_4m_vs_1m_req(OE_1m_done)
#'
#' OE_1m_not_done <- data.frame(
#'   USUBJID = "1",
#'   OECAT = "BEST CORRECTED VISUAL ACUITY",
#'   OETSTDTL = "TESTING DISTANCE: 4M",
#'   OESCAT = c(rep("", 3), "TOTAL"),
#'   OESTAT = "",
#'   OERESCAT = c("ROW 1 - SNELLEN 20/200",
#'                "ROW 2 - SNELLEN 20/160",
#'                "ROW 3 - SNELLEN 20/125",
#'                ""),
#'   VISIT = "WEEK 1",
#'   VISITNUM = 5,
#'   OEDTC = "2020-06-01",
#'   OEDY = 8,
#'   OELOC = "EYE",
#'   OELAT = "LEFT",
#'   OESTRESN = c(5, 5, 2, 12)
#' )
#' check_oe_bcva_4m_vs_1m_req(OE_1m_not_done)
#'
check_oe_bcva_4m_vs_1m_req <- function(OE) {
    required_variables <- c(
        "USUBJID", "OECAT", "OESCAT", "OETSTDTL", "OESTAT", "OERESCAT", "VISIT", "VISITNUM", "OEDTC", "OEDY",
         "OELAT", "OESTRESN"
    )
    output_variables <- c("USUBJID", "VISIT", "OEDTC", "OELAT", "BCVA_4M_TOTAL",
                          "BCVA_1M_TOTAL", "ISSUE")
    to_upper_variables <- function(variables) {
        if(is.character(variables)) return(toupper(variables))
        else return(variables)
    }


    if (OE %lacks_any% required_variables) {
        fail(lacks_msg(OE, required_variables))
    } else {
        # preprocess OE dataset to get the relavent BCVA 4m and 1m data
        ### select required variables
        BCVA_4m_1m_total <- OE %>%
            select(all_of(required_variables))
        ### convert character variables to upper case
        for(var in required_variables){
            BCVA_4m_1m_total[[var]]=to_upper_variables(BCVA_4m_1m_total[[var]])
        }
        ### filter on BCVA 4m and 1m criteria
        BCVA_4m_1m_total <- BCVA_4m_1m_total %>%
            filter(OECAT == "BEST CORRECTED VISUAL ACUITY" &
                       OETSTDTL %in% c('TESTING DISTANCE: 4M', 'TESTING DISTANCE: 1M') & OESTAT != "NOT DONE" &
                       OESCAT %in% c("TOTAL", "NORMAL LIGHTING SCORE") & OESCAT != "LOW LUMINANCE") %>%
            select(USUBJID, VISIT, VISITNUM, OEDTC, OEDY, OELAT, OESTRESN, OETSTDTL)

        ### Filter on Low Vision Tests & Get the Distinct list of Patient, VISIT, Laterality
        # If Patient has done low vision test, 1m test req is not applicable.
        BCVA_low_vision <- OE %>%
            filter(OECAT == "BEST CORRECTED VISUAL ACUITY" &
                       OESCAT == "LOW VISION") %>%
            select(USUBJID, VISIT, VISITNUM, OEDTC, OEDY, OELAT)

        BCVA_low_vision <- BCVA_low_vision %>%
            distinct(USUBJID, VISIT, VISITNUM, OEDTC, OEDY, OELAT) %>%
            mutate(lowvision_ind = 1)

        # check if dates are unique for each USUBJID, VISITNUM, OELAT combination, if not, such a combination will be
        # removed from the BCVA check including situations like 1) unscheduled visits on different dates, or 2) TOTAL
        # date for 4m and 1m are recorded differently in the dataset.
        nonunique_dates_for_each_sub_visit <- BCVA_4m_1m_total %>%
            group_by(USUBJID, VISIT, VISITNUM, OELAT) %>%
            summarise(num_of_dates = length(unique(OEDTC)), .groups = "drop") %>%
            filter(num_of_dates > 1) %>%
            ungroup()
        BCVA_4m_1m_total <- BCVA_4m_1m_total %>% left_join(
            nonunique_dates_for_each_sub_visit,
            by = c("USUBJID", "VISIT", "VISITNUM", "OELAT")
        ) %>%
            filter(is.na(num_of_dates)) %>%
            select( - num_of_dates)

        ## Get BCVA 4m dataset
        BCVA_4m <- BCVA_4m_1m_total %>% filter(OETSTDTL == "TESTING DISTANCE: 4M") %>% select(-OETSTDTL)
        ## Get BCVA 4m dataset when result <= 19
        BCVA_4m_19 <- BCVA_4m %>% filter(OESTRESN <= 19)
        ## Get BCVA 4m dataset when result >= 20
        BCVA_4m_20 <- BCVA_4m %>% filter(OESTRESN >= 20)
        ## Get BCVA 1m dataset
        BCVA_1m <- BCVA_4m_1m_total %>% filter(OETSTDTL == "TESTING DISTANCE: 1M") %>% select(-OETSTDTL)


        # 4m test total <= 19 but 1m test not done
        result19 <- BCVA_4m_19 %>%
            left_join(BCVA_1m, by = c("USUBJID", "VISIT", "VISITNUM", "OELAT", "OEDTC", "OEDY")) %>%
            rename(BCVA_4M_TOTAL = OESTRESN.x, BCVA_1M_TOTAL = OESTRESN.y) %>%
            filter(is.na(BCVA_1M_TOTAL) | (BCVA_1M_TOTAL == 0 & BCVA_4M_TOTAL <= 19)) %>%
            mutate(ISSUE = "1m Test Not Done but Required")

        # Remove records when patients have completed low vision
        result19 <- left_join(result19, BCVA_low_vision, by=c("USUBJID", "VISIT", "VISITNUM", "OELAT", "OEDTC", "OEDY")) %>%
            filter(is.na(lowvision_ind)) %>%
            select(-lowvision_ind)

        # 4m test total >= 20 but 1m test done
        result20 <- BCVA_4m_20 %>%
            left_join(BCVA_1m, by = c("USUBJID", "VISIT", "VISITNUM", "OELAT", "OEDTC", "OEDY")) %>%
            rename(BCVA_4M_TOTAL = OESTRESN.x, BCVA_1M_TOTAL = OESTRESN.y) %>%
            filter(!is.na(BCVA_1M_TOTAL)) %>%
            filter(BCVA_1M_TOTAL != 0) %>%
            mutate(ISSUE = "1m Test Done but Not Required")
        # rbind two results
        result <- rbind(result19, result20)
        result <- result %>% select(-VISITNUM, -OEDY) # Remove VISITNUM & OEDY from final FAIL message

        if (nrow(result) != 0) {
            fail(paste0(nrow(result), " BCVA 1m test result(s) not done properly based on 4m result. "), result)
        } else {
            pass()
        }
    }
}
