#' @title Check for COVID-19 AE with DRUG WITHDRAWN action without "ADVERSE EVENT" for DS Trt Discon
#'
#' @description This checks for a COVID-19 AE reported with Action Taken 
#' (AEACN*==DRUG WITHDRAWN) but without a corresponding DS record indicating 
#' DS.DSDECOD with "ADVERSE EVENT" on any Treatment Discontinuation 
#' form. This relies on DSSPID with the string "DISCTX" when DSCAT == 
#' "DISPOSITION EVENT" to select Treatment Discontinuation records in DS, 
#' as DSSCAT typically includes a text string referring to specific study
#' drug(s).
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AETERM, AEDECOD, 
#' AESTDTC, AEACNx
#' @param DS Disposition SDTM dataset with variables USUBJID, DSSPID, DSCAT, DSDECOD
#' @param covid_df Dataframe of AE preferred terms for COVID-19, containing the 
#' variable REFTERM
#'
#' @return boolean value if check returns 0 obs, otherwise return subset dataframe.
#'
#' @export
#'
#' @author Sarwan Singh
#' 
#' @family COVID
#' 
#' @keywords COVID
#'
#' @examples
#' 
#' covid_df = data.frame(REFTERM = c("COVID-19",
#'                                   "CORONAVIRUS POSITIVE"
#'                                   )
#'                      )
#' AE <- data.frame(
#'     STUDYID = 1,
#'     USUBJID = c(1,2,3,1,2,3),
#'     AESTDTC = '2020-05-05',
#'     AETERM  = c("abc Covid-19", "covid TEST POSITIVE",rep("other AE",4)),
#'     AEDECOD = c("COVID-19", "CORONAVIRUS POSITIVE", rep("OTHER AE",4)),
#'     AEACN = c("DRUG WITHDRAWN", rep("DOSE NOT CHANGED",5)),
#'     AESPID = "FORMNAME-R:13/L:13XXXX",
#'     stringsAsFactors = FALSE
#' )
#'
#' DS <- data.frame(
#'  USUBJID = c(1,1,2,3,4),
#'  DSSPID  = 'XXX-DISCTX-XXX',
#'  DSCAT   = "DISPOSITION EVENT",
#'  DSDECOD = "OTHER REASON",
#'  DSSEQ = c(1,2,1,1,1),
#'  stringsAsFactors = FALSE
#' )
#'
#' # expect fail
#' check_ae_aeacn_ds_disctx_covid(AE, DS, covid_df)
#'
#' 
#' AE2 <- data.frame( 
#'     AEACN1 =  rep(NA, nrow(AE)),
#'     AEACN2 = c("DRUG WITHDRAWN", rep("DOSE NOT CHANGED", nrow(AE)-1)),
#'     AEACN3 = c("DRUG WITHDRAWN", rep("DOSE NOT CHANGED", nrow(AE)-1)),
#'     AEACN4 = "",
#'     stringsAsFactors = FALSE
#' )
#' AE2 <- cbind(AE, AE2) 
#' AE2$AEACN <- "MULTIPLE" 
#' 
#' # expect fail
#' check_ae_aeacn_ds_disctx_covid(AE2, DS, covid_df)
#' 
#' DS[1, "DSDECOD"] <- 'ADVERSE EVENT'
#' # this passes, one form with DSDECOD = "ADVERSE EVENT"
#' ## NOTE: This may be a false negative if study-specific data collection 
#' ##       requires >1 record with DSDECOD = "ADVERSE EVENT" and not just one record
#' check_ae_aeacn_ds_disctx_covid(AE2, DS, covid_df)
#'
#' # expect pass
#' check_ae_aeacn_ds_disctx_covid(AE, DS, covid_df)
#' 
#'
#' # non-required variable is missing
#' DS$DSSEQ <- NULL
#' check_ae_aeacn_ds_disctx_covid(AE, DS, covid_df)
#' 
#' # required variable is missing 
#' DS$DSSPID <- NULL
#' check_ae_aeacn_ds_disctx_covid(AE, DS, covid_df)
#' 
#' 

check_ae_aeacn_ds_disctx_covid <- function(AE,DS,covid_df = NULL) {

    if (is.null(covid_df)){
       
        message("check_ae_aeacn_ds_disctx_covid: Check not run, did not detect COVID-19 preferred terms")
        fail("Check not run, did not detect COVID-19 preferred terms") 
        
    } else if( AE %lacks_any% c("USUBJID", "AETERM", "AEDECOD", "AESTDTC", "AEACN")){

        fail(lacks_msg(AE, c("USUBJID", "AETERM", "AEDECOD", "AESTDTC", "AEACN")))

    } else if( DS %lacks_any% c("USUBJID", "DSSPID", "DSCAT", "DSDECOD")){

        fail(lacks_msg(DS, c("USUBJID", "DSSPID", "DSCAT", "DSDECOD")))

    } else{

        # keep COVID-19 AEs on which we want to check action taken with treatment
        ae_covid <- merge(x = AE, y = covid_df,
                          by.x = toupper("AEDECOD"), by.y = toupper("REFTERM"))

        # keep columns that are needed
        ae0 <- subset(ae_covid,
                      select = c("USUBJID", "AEDECOD", "AESTDTC", "AEACN",
                                 grep("AEACN[0-9]", names(AE), value=TRUE)))


        # find how many AEACNx variables are in AE
        aeacnvars <- grep("AEACN[0-9]", names(ae0), value=TRUE)

        #create a where condition for AE based on count of AEACNx
        whrcond <- paste("AEACN=='DRUG WITHDRAWN'")
        for (i in aeacnvars) {
            whrcond <- paste(whrcond, " | ", i, "=='DRUG WITHDRAWN'", sep="")
        }

        # loop through each AEACNx and keep DRUG WITHDRAWN AEs
        ae1 <- subset(ae0,
                      eval(parse(text = whrcond)) )

        # keep records with discontinuation treatment due to AE
        ds0 <- subset(DS,
                      DSCAT == "DISPOSITION EVENT" & grepl("DISCTX", DSSPID) & DSDECOD == "ADVERSE EVENT",
                      select = c("USUBJID", "DSDECOD"))

        # merge AE and DS
        finout <- merge(x = ae1, y = ds0,
                        by= "USUBJID",
                        all.x = TRUE)

        # keep if only in AE and not found in DS
        mydf <- subset(finout,
                       is_sas_na(DSDECOD),
                       select = -c(DSDECOD)
        )

        # Drop AEACN if its multiple
        if ("AEACN" %in% names(AE) & any(AE$AEACN=="MULTIPLE")) {
            mydf$AEACN=NULL
        }

        rownames(mydf)=NULL

        ### Return message if no inconsistency between AE and DS
        if (nrow(mydf)==0) {
            pass()

            ### Return subset dataframe if there are records with inconsistency
        } else if (nrow(mydf)>0) {

            return(fail(paste(length(unique(mydf$USUBJID)),
                              " patient(s) with COVID-19 AE indicating drug withdrawn but no Treatment Discon form indicating AE. ", sep=""), mydf))

        } #end else if mydf>0
    }  # end else if all required vars present
} # end function
