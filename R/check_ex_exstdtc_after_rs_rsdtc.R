#' @title Check EXSTDTC records occuring on or after RSDTC
#' 
#' @description This check identifies records where EXSTDTC dates occurs
#'  on or after RSDTC. Only applies to assessments by investigator, 
#'  RSEVAL = "INVESTIGATOR",RSTESTCD == "OVRLRESP" and where EXDOSE is greater than 0.
#'
#' @param EX Exposure SDTM dataset with variables USUBJID, EXSTDTC, and EXDOSE
#' @param RS Disease Response SDTM dataset with variables USUBJID, RSDTC,
#' RSTESTCD, and RSEVAL
#' @param preproc An optional company specific preprocessing script
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select left_join
#' @importFrom tidyselect any_of
#'
#' @author Josue Alcaraz
#'
#' @examples
#' 
#' 
#' # No errors
#' EX_SAMPLE <- data.frame(USUBJID = LETTERS[1:5],
#'                  EXSTDTC = c("2015-12-31","2016-12-31","2017-12-31","2009-12-31","2001-12-31" ),
#'                  EXDOSE = 1:5,
#'                  stringsAsFactors = FALSE)
#' 
#' 
#' RS_SAMPLE<- data.frame(USUBJID = LETTERS[1:5],
#'                 RSDTC=c("2016-02-01T08:25", "2018-01-05T09:25",
#'                        "2022-03-15T10:25","2017-01-20T08:25","2017-01-25T08:25",
#'                        "2017-02-01T08:25", "2019-01-05T09:25",
#'                        "2023-03-15T10:25","2018-01-20T08:25","2018-01-25T08:25"),
#'                 RSTESTCD="OVRLRESP",
#'                 RSEVAL="INVESTIGATOR",
#'                 RSSPID = "FORMNAME-R:19/L:19XXXX",
#'                 stringsAsFactors=FALSE)
#' 
#' check_ex_exstdtc_after_rs_rsdtc(EX= EX_SAMPLE,RS= RS_SAMPLE)
#' 
#' 
#' 
#' # Four errors, preproc=identity
#' EX_SAMPLE <- data.frame(USUBJID = LETTERS[1:5],
#'                  EXSTDTC = c("2015-12-31","2016-12-31","2017-12-31","2009-12-31","2001-12-31" ),
#'                  EXDOSE = 1:5,
#'                  stringsAsFactors = FALSE)
#' 
#' 
#' RS_SAMPLE<- data.frame(USUBJID = LETTERS[1:5],
#'                 RSDTC=c("2015-12-31T08:25", "2018-01-05T09:25",
#'                        "2017-03-15T10:25","2017-01-20T08:25","2017-01-25T08:25",
#'                        "1998-02-01T08:25", "2019-01-05T09:25",
#'                        "2016-03-15T10:25","2018-01-20T08:25","2018-01-25T08:25"),
#'                 RSTESTCD="OVRLRESP",
#'                 RSEVAL="INVESTIGATOR",
#'                 RSSPID = "FORMNAME-R:19/L:19XXXX",
#'                 stringsAsFactors=FALSE)
#' 
#' check_ex_exstdtc_after_rs_rsdtc(EX= EX_SAMPLE,RS= RS_SAMPLE)
#' 
#' 
#' 
#' # missing column
#' EX_SAMPLE <- data.frame(USUBJID = LETTERS[1:5],
#'                  # EXSTDTC = c("2015-12-31","2016-12-31","2017-12-31","2009-12-31","2001-12-31" ),
#'                  EXDOSE = 1:5,
#'                  stringsAsFactors = FALSE)
#' 
#' 
#' RS_SAMPLE<- data.frame(USUBJID = LETTERS[1:5],
#'                 RSDTC=c("1999-02-01T08:25", "2018-01-05T09:25",
#'                        "2017-03-15T10:25","2017-01-20T08:25","2017-01-25T08:25",
#'                        "1998-02-01T08:25", "2019-01-05T09:25",
#'                        "2016-03-15T10:25","2018-01-20T08:25","2018-01-25T08:25"),
#'                 RSTESTCD="OVRLRESP",
#'                 RSEVAL="INVESTIGATOR",
#'                 RSSPID = "FORMNAME-R:19/L:19XXXX",
#'                 stringsAsFactors=FALSE)
#' 
#' check_ex_exstdtc_after_rs_rsdtc(EX= EX_SAMPLE,RS= RS_SAMPLE)
#' 
#' 
#' 
#' 
#' # Return df with errors
#' EX_SAMPLE <- data.frame(USUBJID = LETTERS[1:5],
#'                  EXSTDTC = c("2015-12-31","2016-12-31","2017-12-31","2009-12-31","2001-12-31" ),
#'                  EXDOSE = 1:5,
#'                  stringsAsFactors = FALSE)
#' 
#' 
#' RS_SAMPLE<- data.frame(USUBJID = LETTERS[1:5],
#'                 RSDTC=c("2015-12-31T08:25", "2018-01-05T09:25",
#'                        "2017-03-15T10:25","2017-01-20T08:25","2017-01-25T08:25",
#'                        "1998-02-01T08:25", "2019-01-05T09:25",
#'                        "2016-03-15T10:25","2018-01-20T08:25","2018-01-25T08:25"),
#'                 RSTESTCD="OVRLRESP",
#'                 RSEVAL="INVESTIGATOR",
#'                 RSSPID = "FORMNAME-R:19/L:19XXXX",
#'                 stringsAsFactors=FALSE)
#' 
#' check_ex_exstdtc_after_rs_rsdtc(EX= EX_SAMPLE,RS= RS_SAMPLE, preproc=roche_derive_rave_row)



check_ex_exstdtc_after_rs_rsdtc <- function(EX, RS, preproc=identity,...){
  
  # Lacks message
  lacks_msgs <- NULL # For storing lack_msg from all required SDTM datasets
  
  # First check that required variables exist and return a message if they don't
  if( EX %lacks_any% c("USUBJID","EXSTDTC","EXDOSE")){
    lacks_msgs <- c(lacks_msgs, lacks_msg(EX, c("USUBJID","EXSTDTC","EXDOSE")))
  }
  
  if( RS %lacks_any% c("USUBJID","RSDTC","RSTESTCD","RSEVAL")){
    lacks_msgs <- c(lacks_msgs, lacks_msg(RS, c("USUBJID","RSDTC","RSTESTCD","RSEVAL")))
  } 
  
  
  if (length(lacks_msgs) > 0) {
    fail(msg = paste0(lacks_msgs, collapse = ". "))
    
  } else {
    
    #Apply company specific preprocessing function
    RS = preproc(RS,...)
    
    # Select needed variables from EX domain. Filters to patients with a dose and new date format.
    exSub <- EX %>%
      select(any_of(c("USUBJID", "EXSTDTC", "EXDOSE")))%>%
      filter(EXDOSE>0)%>%
      mutate(EXSTDTC = as.Date(EXSTDTC, format = "%Y-%m-%d"))
    
    # Select needed variables from RS domain. Filters patients and new date format.
    rsSub <- RS %>% 
      select(any_of(c("USUBJID", "RSDTC", "RSTESTCD", "RSEVAL","RAVE")))%>%
      filter(RSEVAL== "INVESTIGATOR", RSTESTCD == "OVRLRESP" )%>%
      mutate(RSDTC = as.Date(RSDTC, format = "%Y-%m-%d"))
    
    # First dose date for each unique patient 
    FirstDoseDateEX <- exSub %>%
      group_by(USUBJID)%>%
      summarise(EXSTDTC = min(EXSTDTC, na.rm = TRUE), .groups = "drop")
    
    # Merging domains and checking for dates where RS_Date on or before EX_Date
    df <- FirstDoseDateEX %>%
      inner_join(rsSub, by = "USUBJID")%>%
      filter(EXSTDTC >= RSDTC)%>%
      select(any_of(c("USUBJID", "RSDTC", "EXSTDTC", "RAVE")))%>%
      distinct()
    
    # Message based on results.
    
    if(nrow(df)==0){
      pass()
      
    }else if(nrow(df)>0){
      fail(paste("EX has ",nrow(df)," records with EXSTDTC on or before RSSTDTC. "), df)
    }
  }
}


