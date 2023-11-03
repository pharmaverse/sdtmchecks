#' @title Check if 1m BCVA test stops too late, too early and has correct total
#'
#' @description This ophthalmology check is for BCVA 1m test. It checks three conditions: <1> BCVA test stops too late,
#' meaning that lines were read after number of correct letters is <= 3. <2> BCVA test stops too early, meaning that
#' further lines were not read when all numbers of correct letters is > 3. <3> BCVA total score is not correct, meaning
#' that the sum of the number of correct at 1 meter doesn't match with what has been recorded in eCRF
#' (BCVA Scores eCRF Page - C. Total number correct at 1m).
#' Please note that this check only works with USUBJID, VISIT, VISITNUM, OELOC, OELAT combination has unique
#' dates (OEDTC). If your datasets are having situations like 1) unscheduled visits happening on different dates or
#' 2) BCVA TOTAL happens on a different date from BCVA row tests, such combinations
#' will be removed from check.
#' Please note that this check excludes forms BCVA Low Vision Test (BCV5), BCVA Scores (BCV7),
#' BCVA Low Luminance Scores (BCVLL5), BCVA Combined Assessments (BCVAC), BCVA Low Luminance Combined Assessments (BCVACLL)
#' before running check as these forms do not include Row numbers.
#'
#' @param OE Ophtho Dataset with variables USUBJID, OESPID, OECAT, OESCAT, OETSTDTL, OESTRESN, OESTAT, OELOC, OELAT,
#' OERESCAT, VISIT, VISITNUM, OEDTC, OEDY
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @importFrom dplyr %>% filter mutate select lag lead rename arrange summarise group_by ungroup
#' @importFrom tidyselect all_of
#' 
#' @family OPHTH
#' 
#' @keywords OPHTH
#'
#' @export
#'
#' @author Rosemary Li (HackR 2021 Team Eye)
#'
#' @examples
#' OE_too_late <- data.frame(
#'   USUBJID = "1",
#'   OESPID = "FORMNAME-R:2/L:2XXXX",
#'   OECAT = "BEST CORRECTED VISUAL ACUITY",
#'   OETSTDTL = "TESTING DISTANCE: 1M",
#'   OESCAT = c(rep("", 6), "TOTAL"),
#'   OESTAT = "",
#'   OERESCAT = c("ROW 1 - SNELLEN 20/200", 
#'                "ROW 2 - SNELLEN 20/160", 
#'                "ROW 4 - SNELLEN 20/100",
#'                "ROW 3 - SNELLEN 20/125", 
#'                "ROW 5 - SNELLEN 20/80", 
#'                "ROW 6 - SNELLEN 20/63", 
#'                ""),
#'   VISIT = "WEEK 1",
#'   VISITNUM = 5,
#'   OEDTC = "2020-06-01",
#'   OEDY = 8,
#'   OELOC = "EYE",
#'   OELAT = "LEFT",
#'   OESTRESN = c(5, 5, 5, 4, 3, 2, 24)
#' )
#'check_oe_bcva_1m_late_early_tot(OE_too_late)
#'
#' OE_too_early <- data.frame(
#'   USUBJID = "1",
#'   OESPID = "FORMNAME-R:2/L:2XXXX",
#'   OECAT = "BEST CORRECTED VISUAL ACUITY",
#'   OETSTDTL = "TESTING DISTANCE: 1M",
#'   OESCAT = c(rep("", 5), "TOTAL"),
#'   OESTAT = "",
#'   OERESCAT = c("ROW 1 - SNELLEN 20/200", 
#'                "ROW 2 - SNELLEN 20/160", 
#'                "ROW 4 - SNELLEN 20/100",
#'                "ROW 3 - SNELLEN 20/125", 
#'                "ROW 5 - SNELLEN 20/80", 
#'                ""),
#'   VISIT = "WEEK 1",
#'   VISITNUM = 5,
#'   OEDTC = "2020-06-01",
#'   OEDY = 8,
#'   OELOC = "EYE",
#'   OELAT = "LEFT",
#'   OESTRESN = c(5, 5, 5, 4, 4, 23)
#' )
#' check_oe_bcva_1m_late_early_tot(OE_too_early)
#'
#' OE_total_incorrect <- data.frame(
#'   USUBJID = "1",
#'   OESPID = "FORMNAME-R:2/L:2XXXX",
#'   OECAT = "BEST CORRECTED VISUAL ACUITY",
#'   OETSTDTL = "TESTING DISTANCE: 1M",
#'   OESCAT = c(rep("", 6), "TOTAL"),
#'   OESTAT = "",
#'   OERESCAT = c("ROW 1 - SNELLEN 20/200", 
#'                "ROW 2 - SNELLEN 20/160", 
#'                "ROW 4 - SNELLEN 20/100",
#'                "ROW 3 - SNELLEN 20/125", 
#'                "ROW 5 - SNELLEN 20/80", 
#'                "ROW 6 - SNELLEN 20/63", 
#'                ""),
#'   VISIT = "WEEK 1",
#'   VISITNUM = 5,
#'   OEDTC = "2020-06-01",
#'   OEDY = 8,
#'   OELOC = "EYE",
#'   OELAT = "LEFT",
#'   OESTRESN = c(5, 5, 5, 4, 4, 2, 28)
#' )
#' check_oe_bcva_1m_late_early_tot(OE_total_incorrect)
#'


check_oe_bcva_1m_late_early_tot <- function(OE) {

    required_variables <- c(
        "USUBJID", "OECAT", "OESCAT", "OETSTDTL", "OESTAT", "OERESCAT", "VISIT", "VISITNUM", "OEDTC", "OEDY",
        "OELOC", "OELAT", "OESTRESN"
    )
    output_variables <- c( "USUBJID", "OETSTDTL", "VISIT", "OEDTC", "OELAT", "OESTRESN")

    str_match <- function(x, pattern) {
        m <- regexpr(pattern, x)
        regmatches(x, m)
    }

    to_upper_variables <- function(variables) {
        if(is.character(variables)) return(toupper(variables))
        else return(variables)
    }

    if(OE %lacks_any% c("OESPID")){
        fail(lacks_msg(OE, c("OESPID")))
    } else if(OE %>% filter(!grepl("BCV5|BCV7|BCVLL5|BCVAC|BCVACLL", OESPID) &
                            OECAT == "BEST CORRECTED VISUAL ACUITY" & OETSTDTL == "TESTING DISTANCE: 1M") %>% nrow() == 0){
        pass()
    } else if (OE %lacks_any% required_variables) {
        fail(lacks_msg(OE, required_variables))
    } else {
        # preprocessing OE dataset to get the relavant BCVA 1m test data
        ### select required variables
        BCVA_1m <- OE %>%
            select(all_of(required_variables))
        ### change all character variables to uppercase for the OE subset dataset
        # BCVA_1m <- data.frame(lapply(BCVA_1m, to_upper_variables))
        for(var in required_variables){
            BCVA_1m[[var]]=to_upper_variables(BCVA_1m[[var]])
        }
        ### filter on BCVA 1m criteria
        BCVA_1m <- BCVA_1m %>%
            filter(OECAT == "BEST CORRECTED VISUAL ACUITY" & OETSTDTL == "TESTING DISTANCE: 1M" &
                       (!OESTAT %in% c("NOT DONE", "ND")) & OESCAT != "LOW LUMINANCE")

        # check if dates are unique for each USUBJID, VISITNUM, OELAT combination, if not, such a combination will be
        # removed from the BCVA check including situations like 1) unscheduled visits on different dates, or 2) TOTAL
        # date happens on a dfferent date from the BCVA test date
        nonunique_dates_for_each_sub_visit <- BCVA_1m %>%
            group_by(USUBJID, VISIT, VISITNUM, OELAT) %>%
            summarise(num_of_dates = length(unique(OEDTC)), .groups = "drop") %>%
            filter(num_of_dates > 1) %>%
            ungroup()
        BCVA_1m <- BCVA_1m %>% left_join(
            nonunique_dates_for_each_sub_visit,
            by = c("USUBJID", "VISIT", "VISITNUM", "OELAT")
        ) %>%
            filter(is.na(num_of_dates)) %>%
            select( - num_of_dates)


        # subcheck 1: if BCVA 1m test stops too late
        anl1 <- BCVA_1m %>% filter(grepl("ROW \\d+ - SNELLEN \\d+/\\d+", OERESCAT)) %>%
            mutate(ROW = str_match(OERESCAT, "ROW \\d+"), ROWNUM = as.integer(str_match(ROW, "\\d+"))) %>%
            arrange(USUBJID, VISITNUM, OEDTC, OELAT, ROWNUM)
        late_rows <- anl1 %>% group_by(USUBJID, VISITNUM, OEDTC, OELOC, OELAT) %>%
            mutate(LAG_OESTRESN = lag(OESTRESN)) %>%
            filter(LAG_OESTRESN <= 3) %>%
            mutate(MAX_ROW = ifelse(is.na(ROWNUM), NA, min(ROWNUM))) %>%
            filter(ROWNUM == MAX_ROW) %>%
            ungroup() %>%
            select(USUBJID, VISITNUM, OEDTC, OELOC, OELAT, MAX_ROW)
        late1m <- anl1 %>% left_join(late_rows, by = c("USUBJID", "VISITNUM", "OEDTC", "OELOC", "OELAT")) %>%
            mutate(issue = ifelse(ROWNUM >= MAX_ROW, "BCVA 1m check stops too late", NA)) %>%
            filter(!is.na(issue) & OESTRESN != 0) %>%
            mutate(TOTAL = NA) %>%
            select(all_of(output_variables), TOTAL, issue) %>%
            unique()

        # subcheck 2: if BCVA 1m test stops too early
        ## anl2 is the same as anl1
        early1m <- anl1 %>% group_by(USUBJID, VISITNUM, OEDTC, OELOC, OELAT) %>%
            mutate(LEAD_OESTRESN = lead(OESTRESN)) %>%
            filter(is.na(LEAD_OESTRESN) & OESTRESN > 3 & ROWNUM != 6) %>%
            ungroup() %>%
            mutate(
                TOTAL = NA,
                issue = "BCVA 1m check stops too early"
            ) %>%
            select(all_of(output_variables), TOTAL, issue) %>%
            unique()

        # subcheck 3: if BCVA 1m test has the correct total in eCRF
        ## compare total and eCRF, output will include 1) totals in eCRF and calcualtion don't match
        ## 2) total not recorded in eCRF 3) only total in eCRF, not test row/line read
        anl_total <- BCVA_1m %>% filter(OESCAT %in% c("TOTAL", "NORMAL LIGHTING SCORE"))
        anl_sum <- anl1 %>%
            group_by(USUBJID, VISIT, VISITNUM, OEDTC, OELOC, OELAT) %>%
            summarise(TOTAL = sum(OESTRESN), .groups = "drop") %>%
            ungroup()
        incorrect1m <- anl_total %>% full_join(anl_sum, by = c("USUBJID", "VISIT", "VISITNUM", "OEDTC", "OELOC", "OELAT")) %>%
            filter(is.na(TOTAL) | is.na(OESTRESN) | OESTRESN != TOTAL) %>%
            filter(!(OESTRESN == 0 & is.na(TOTAL))) %>% ## filtered out when 1m is not done and total is 0
            mutate(issue = "BCVA 1m score incorrect") %>%
            select(all_of(output_variables), TOTAL, issue) %>%
            unique()

        ## rbind all three subchecks
        df <- rbind(late1m, early1m, incorrect1m)

        if (nrow(df) != 0) {
            fail(paste0(nrow(df), " BCVA 1m test record(s) failed with 1) test stops too late 2) test stops too early 3) total incorrect. "), df)
        } else {
            pass()
        }
    }
}
