#' @title Check that all RS dates for INV Overall Response are duplicated or
#' earlier than last visit's (possible date entry error)
#'
#' @description This check identifies RSDTC values when RSEVAL == 'INVESTIGATOR'
#' and RSTESTCD == 'OVRLRESP' that are duplicated or earlier than last visit's.
#' Unscheduled and 'NOT DONE' visits are excluded.
#'
#' @param RS Response SDTM dataset with variables USUBJID, VISITNUM, VISIT,
#' RSDTC, RSTESTCD, RSEVAL, RSSTAT
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author James Zhang
#'
#' @examples
#'
#' # no cases
#' RS<- data.frame(USUBJID = 101:102,
#'                 RSDTC=rep(c("2017-01-01T08:25", "2017-01-05T09:25",
#'                        "2017-01-15T10:25","2017-01-20T08:25","2017-01-25T08:25"), 2),
#'                 VISITNUM=rep(1:5,2),
#'                 VISIT=rep(c("Screening", "Cycle 1", "Cycle 2","Cycle 3","Follow-up"),2),
#'                 RSTESTCD="OVRLRESP",
#'                 RSEVAL="INVESTIGATOR",
#'                 RSSTAT="",
#'                 stringsAsFactors=FALSE)
#' check_rs_rsdtc_visit_ordinal_error(RS)
#'
#' # adding cases with earler date
#' RS$RSDTC[RS$USUBJID == 101 & RS$VISIT == "Cycle 3"] <- "2017-01-02T08:25"
#' RS$RSDTC[RS$USUBJID == 102 & RS$VISIT == "Cycle 1"] <- "2017-01-01T06:25"
#' check_rs_rsdtc_visit_ordinal_error(RS)
#'

check_rs_rsdtc_visit_ordinal_error <- function(RS){

    class(RS) <- 'data.frame'
    vars = c("USUBJID", "VISITNUM", "VISIT", "RSDTC", "RSTESTCD","RSEVAL","RSSTAT")

    ### First check that required variables exist and return a message if they don't
    if (RS %lacks_any% vars) {

        fail(lacks_msg(RS, vars))

        ### Dont run if VISITNUM is all missing
    } else if (length(unique(RS[["VISITNUM"]]))<=1) {

        fail("VISITNUM exists but only a single value. ")

    } else {
        #only keep INV overall responses not indicated as Not Done
        subsetdf = subset(RS,RS$RSTESTCD=="OVRLRESP" & RS$RSEVAL=="INVESTIGATOR" & RS$RSSTAT != "NOT DONE" & !grepl("UNSCHEDU",toupper(RS$VISIT)),)

        if(nrow(subsetdf)>0){

            mydf2 <- dtc_dupl_early(dts = subsetdf, vars = vars,
                                    ### groupby variables used for grouping and visit.order derivation
                                    groupby = vars[c(1)],
                                    dtc = vars[4],
                                    ### variables used for ordering before visit.order derivation
                                    vars[1], vars[2], vars[3], vars[4])

            ### Subset if Vis_order not equal Dtc_order
            myout <- mydf2[!is.na(mydf2$check.flag), ]

            ### Different check already doing dups
            myout = subset(myout,myout$check.flag != "Duplicated",)


            ###Print to report

            ### Return message if no records with RSDTC per VISITNUM
            if (nrow(myout) == 0) {
                pass()
                ### Return subset dataframe if there are records with Possible RSDTC data entry error
            } else if (nrow(myout) > 0) {
                rownames(myout) = NULL
                fail(paste("RS has ",nrow(myout)," records with Possible RSDTC data entry error. ",sep = ""), myout)
            }

        } else{fail("No records when subset to overall responses by INV. ")}
    }
}
