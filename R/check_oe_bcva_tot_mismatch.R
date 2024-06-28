#' @title Check mismatch between Derived BCVA Total Score & Total BCVA Score from Data
#'
#' @description This ophthalmology check looks for any mismatch between the
#' Derived Best Corrected Visual Acuity (BCVA) Total Score &
#' reported Total BCVA Score from Data based on OETESTCD = "LOGSCORE" for
#' older studies or OETESTCD = "VACSCORE" for newer studies
#'
#' @param OE Ophtho Dataset with variables USUBJID, OETESTCD, OECAT, OESCAT, OETSTDTL,
#' OESTRESN, OESTAT (if present), OELAT, VISIT, OEDTC
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
#' #Using Old Standard, FAIL Case (4m <=19, so 4m + 1m to match with Rave Total)
#' OE <- data.frame(
#'    USUBJID = 1,
#'    OETESTCD = c("NUMLCOR", "NUMLCOR", "LCORCON", "LOGSCORE"),
#'    OECAT   = rep("BEST CORRECTED VISUAL ACUITY", 4),
#'    OESCAT   = c("TOTAL", "TOTAL","", ""),
#'    OETSTDTL  = c("TESTING DISTANCE: 4M", "TESTING DISTANCE: 1M", "", ""),
#'    OESTRESN = c(18, 0, 30, 48),
#'    OESTAT= rep("", 4),
#'    OELOC   = rep("EYE", 4),
#'    OELAT   = rep("LEFT", 4),
#'    VISIT   = rep("SCREENING", 4),
#'    VISITNUM   = rep(99, 4),
#'    OEDTC = rep("2021-05-19", 4),
#'    OEDY  = rep(1, 4),
#'    stringsAsFactors = FALSE)
#'
#' check_oe_bcva_tot_mismatch(OE)
#'
#'
#' #Using New Standard, PASS Case
#'
#' OE <- data.frame(
#'    USUBJID = 1,
#'    OETESTCD = c("NUMLCOR", "NUMLCOR", "LCORCON", "VACSCORE"),
#'    OECAT   = rep("BEST CORRECTED VISUAL ACUITY", 4),
#'    OESCAT   = c("NORMAL LIGHTING SCORE", "NORMAL LIGHTING SCORE","", ""),
#'    OETSTDTL  = c("TESTING DISTANCE: 4M", "TESTING DISTANCE: 1M", "", ""),
#'    OESTRESN = c(22, 0, 30, 52),
#'    OESTAT= rep("", 4),
#'    OELOC   = rep("EYE", 4),
#'    OELAT   = rep("LEFT", 4),
#'    VISIT   = rep("SCREENING", 4),
#'    VISITNUM   = rep(99, 4),
#'    OEDTC = rep("2021-05-19", 4),
#'    OEDY  = rep(1, 4),
#'    stringsAsFactors = FALSE)
#' check_oe_bcva_tot_mismatch(OE)
#'
#'
#' #Using New Standard, FAIL Case (Total 4m + 1m (As 4m <=19) not equal to CRF Total Score)
#'
#' OE <- data.frame(
#'    USUBJID = 1,
#'    OETESTCD = c("NUMLCOR", "NUMLCOR", "LCORCON", "VACSCORE"),
#'    OECAT   = "BEST CORRECTED VISUAL ACUITY",
#'    OESCAT   = c("NORMAL LIGHTING SCORE", "NORMAL LIGHTING SCORE","", ""),
#'    OETSTDTL  = c("TESTING DISTANCE: 4M", "TESTING DISTANCE: 1M", "", ""),
#'    OESTRESN = c(17, 12, 0, 27),
#'    OESTAT= "",
#'    OELOC   = "EYE",
#'    OELAT   = "LEFT",
#'    VISIT   = "SCREENING",
#'    VISITNUM   = 99,
#'    OEDTC = "2021-05-19",
#'    OEDY  = 1,
#'    stringsAtors = FALSE)
#' check_oe_bcva_tot_mismatch(OE)
#'
#'
#' #FAIL Case without optional variable, OESTAT
#'
#' OE$OESTAT <- NULL
#' check_oe_bcva_tot_mismatch(OE)
#'
#'
#' #missing required variable, OETESTCD
#'
#' OE$OETESTCD <- NULL
#' check_oe_bcva_tot_mismatch(OE)
#'

check_oe_bcva_tot_mismatch <- function(OE) {

    if (OE %lacks_any% c("USUBJID", "OETESTCD", "OECAT", "OESCAT", "OETSTDTL", "OESTRESN",  "OELAT", "VISIT",  "OEDTC")) {

        fail(lacks_msg(OE, c("USUBJID", "OETESTCD", "OECAT", "OESCAT", "OETSTDTL", "OESTRESN",  "OELAT", "VISIT",  "OEDTC")))
    }

    else {

    # Total Score Computed vs Total Score from Data ----

    # Total Score Derivation

    # A. Total # of Correct at 4m
    # B. If value of A >= 20 then 30, else 0
    # C. Total number of correct at 1 m (if not tested enter 0)
    # D. Visual Acuity Score = A + B + C

    perm_var <- c("OESTAT")
    int_var <- intersect(names(OE), perm_var)

    my_select_var <- c("USUBJID", "OETESTCD", "OECAT", "OESCAT", "OETSTDTL", "OESTRESN", "OELAT", "VISIT", "OEDTC", int_var)

    if ((any(names(OE) == "OESTAT")) == TRUE) {

        OE <- OE %>% filter(toupper(OESTAT) != "NOT DONE")

    }

    OE <- OE %>% mutate(OETESTCD = toupper(OETESTCD),
                        OESCAT = toupper(OESCAT),
                        OETSTDTL = toupper(OETSTDTL),
                        OECAT = toupper(OECAT))


    bcva_t1 <- OE %>% filter((OETESTCD == "NUMLCOR" & OESCAT == "TOTAL" & OETSTDTL == 'TESTING DISTANCE: 4M' & OECAT == "BEST CORRECTED VISUAL ACUITY") |   #Old Standard
                             (OETESTCD == "NUMLCOR" & OESCAT == "NORMAL LIGHTING SCORE" & OETSTDTL == 'TESTING DISTANCE: 4M' & OECAT == "BEST CORRECTED VISUAL ACUITY")) %>% #New Standard
                      mutate(TOT_4M = OESTRESN) %>%
                      select(USUBJID, OELAT, VISIT, OEDTC, TOT_4M)


    bcva_t2 <- OE %>% filter((OETESTCD == "NUMLCOR" & OESCAT == "TOTAL" & OETSTDTL == 'TESTING DISTANCE: 1M' & OECAT == "BEST CORRECTED VISUAL ACUITY") |   #Old Standard
                             (OETESTCD == "NUMLCOR" & OESCAT == "NORMAL LIGHTING SCORE" & OETSTDTL == 'TESTING DISTANCE: 1M' & OECAT == "BEST CORRECTED VISUAL ACUITY")) %>% #New Standard
                        mutate(TOT_1M = OESTRESN) %>%
                        select(USUBJID, OELAT, VISIT, OEDTC, TOT_1M)


    bcva_t3 <- OE %>% filter(OETESTCD == "LCORCON" & OECAT == "BEST CORRECTED VISUAL ACUITY") %>%
                        mutate(PARTB_CONST = OESTRESN) %>%
                        select(USUBJID, OELAT, VISIT, OEDTC, PARTB_CONST)


    bcva_t4 <- OE %>% filter((OETESTCD == "LOGSCORE" & OECAT == "BEST CORRECTED VISUAL ACUITY") | #Old Standard
                             (OETESTCD == "VACSCORE" & OECAT == "BEST CORRECTED VISUAL ACUITY")) %>% #New Standard
                        mutate(BCVATOT_CRF = OESTRESN) %>%
                        select(USUBJID, OELAT, VISIT, OEDTC, BCVATOT_CRF)


    bcva_t <- Reduce(function(x,y) merge(x = x, y = y, c("USUBJID",  "OELAT", "VISIT", "OEDTC"), all=TRUE),
                     list(bcva_t1, bcva_t2, bcva_t3, bcva_t4))

    bcva_t <- bcva_t %>%
                mutate(BCVATOT_DERIVED = ifelse(TOT_4M >=20, TOT_4M + 30,
                                    ifelse(TOT_4M <= 19, TOT_4M + TOT_1M , NA)))

    mydf <- bcva_t %>% filter(BCVATOT_DERIVED != BCVATOT_CRF)

    if ((nrow(mydf) > 0 ) == FALSE) {
        pass()
        }
    else {
        fail(paste0(nrow(mydf), " record(s) with BCVA Total Score Derived and from RAVE/eCRF with mismatch. "), mydf )
        }
    }

}

